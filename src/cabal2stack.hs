{-# LANGUAGE ApplicativeDo          #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Main (main) where

import Control.Applicative       (optional, (<**>), (<|>))
import Control.Exception         (IOException, catch)
import Control.Monad             (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, execStateT)
import Data.Char                 (isHexDigit)
import Data.Foldable             (for_, traverse_)
import Data.Map.Strict           (Map)
import Data.Set                  (Set)
import Data.Text                 (Text)
import System.Directory          (getCurrentDirectory, listDirectory)
import System.Exit               (ExitCode (..), exitFailure)
import System.FilePath           (makeRelative, (</>))
import System.IO                 (hPutStrLn, stderr)

import Optics.Core
       (A_Lens, LabelOptic (..), at, lensVL, traversalVL, traverseOf, (%),
       (<&>))
import Optics.State.Operators ((%=), (<<?=), (?=))

import qualified Cabal.Plan           as P
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict      as Map
import qualified Data.Set             as Set
import qualified Data.Text            as T
import qualified Data.YAML            as Y
import qualified Options.Applicative  as O
import qualified System.Process       as Proc

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    Opts {..} <- O.execParser $ O.info (optsP <**> O.helper)
        (O.fullDesc <> O.progDesc "Create stack.yaml from cabal-install's plan.json")

    cwd <- getCurrentDirectory

    plan <- P.findAndDecodePlanJson $ maybe
        (P.ProjectRelativeToDir cwd)
        P.ExactPath
        optsPlanJson

    S packages _pkgVers extraDeps flags gitPackages <- processUnits (P.pjUnits plan)

    let stackYaml0 :: StackYaml
        stackYaml0 = StackYaml
            { syResolver    = P.pjCompilerId plan
            , sySystemGHC   = optsSystemGHC
            , syAllowNewer  = optsAllowNewer
            , syPackages    = Set.map (makeRelative cwd) packages
            , syExtraDeps   = Set.fromList [ P.PkgId pn ver | (pn, ver) <- Map.toList extraDeps ]
            , syFlags       = flags
            , syGitPackages = gitPackages
            }

    stackYaml <- traverseOf (#gitPackages % traversalVL traverseSet) tagsToCommits stackYaml0

    case optsOutput of
        "-" -> LBS.putStr (Y.encode [stackYaml])
        _   -> LBS.writeFile optsOutput (Y.encode [stackYaml])

-------------------------------------------------------------------------------
-- options
-------------------------------------------------------------------------------

data Opts = Opts
    { optsSystemGHC  :: !Bool
    , optsAllowNewer :: !Bool
    , optsPlanJson   :: !(Maybe FilePath)
    , optsOutput     :: !FilePath
    }
  deriving Show

optsP :: O.Parser Opts
optsP = do
    optsSystemGHC <-
        O.flag' True  (O.long "system-ghc" <> O.help "Use system ghc") <|>
        O.flag' False (O.long "no-system-ghc" <> O.help "Use stack's ghc") <|>
        pure True

    optsAllowNewer <-
        O.flag' True  (O.long "allow-newer" <> O.help "Include allow-newer: True") <|>
        O.flag' False (O.long "no-allow-newer" <> O.help "Don't include allow-newer: True") <|>
        pure False

    optsPlanJson <- optional $ O.strOption (O.long "plan-json" <> O.metavar "PATH" <> O.help "Use provided plan.json")

    optsOutput <- O.strOption (O.short 'o' <> O.long "output" <> O.metavar "PATH" <> O.value "stack.yaml" <> O.help "Output location")

    pure Opts {..}

-------------------------------------------------------------------------------
-- git tags to commits
-------------------------------------------------------------------------------

tagsToCommits :: GitRepo -> IO GitRepo
tagsToCommits gitrepo@(GitRepo url tag subdir)
    | isCommit tag = pure gitrepo
    | otherwise    = do
        let srcDir = "dist-newstyle/src"
        contents <- catchIO (listDirectory srcDir) (\_ -> return [])
        mcommit <- findM contents $ \dir -> handleIO (\_ -> return Nothing) $ do
            -- look up that url is in remotes
            (ec, out, _) <- Proc.readCreateProcessWithExitCode (Proc.proc "git" ["remote", "-v"]) { Proc.cwd = Just (srcDir </> dir) } ""
            if isExitSuccess ec && T.isInfixOf url (T.pack out)
            then do
                -- try to lookup tag
                (ec2, out2, _) <- Proc.readCreateProcessWithExitCode (Proc.proc "git" ["rev-list", "-n", "1", T.unpack tag]) { Proc.cwd = Just (srcDir </> dir) } ""
                let commit = T.strip (T.pack out2)
                if isExitSuccess ec2 && isCommit commit
                then return (Just commit)
                else return Nothing
            else return Nothing

        case mcommit of
            Just commit -> do
                hPutStrLn stderr $ "Tag is not a commit in " ++ show gitrepo ++ "; found commit " ++ T.unpack commit
                return (GitRepo url commit subdir)

            Nothing -> do
                hPutStrLn stderr $ "Tag is not a commit in " ++ show gitrepo ++ "; commit not found, stack will barf"
                return gitrepo
  where
    isCommit t = T.length t == 40 && T.all isHexDigit t



-------------------------------------------------------------------------------
-- units
-------------------------------------------------------------------------------

data S = S
    { _sPackages  :: !(Set FilePath)
    , _sPkgVers   :: !(Map P.PkgName P.Ver)
    , _sExtraDeps :: !(Map P.PkgName P.Ver)
    , _sFlags     :: !(Map P.PkgName (Map P.FlagName Bool))
    , _sGitPkgs   :: !(Set GitRepo)
    }
  deriving Show

data GitRepo = GitRepo !Text !Text !(Maybe FilePath)
  deriving (Show, Eq, Ord)

instance (k ~ A_Lens, a ~ Set FilePath, b ~ Set FilePath) => LabelOptic "packages" k S S a b where
    labelOptic = lensVL $ \f (S x a b c d) -> f x <&> \x' -> S x' a b c d

instance (k ~ A_Lens, a ~ Map P.PkgName P.Ver, b ~ Map P.PkgName P.Ver) => LabelOptic "pkgVers" k S S a b where
    labelOptic = lensVL $ \f (S a x b c d) -> f x <&> \x' -> S a x' b c d

instance (k ~ A_Lens, a ~ Map P.PkgName P.Ver, b ~ Map P.PkgName P.Ver) => LabelOptic "extraDeps" k S S a b where
    labelOptic = lensVL $ \f (S a b x c d) -> f x <&> \x' -> S a b x' c d

instance (k ~ A_Lens, a ~ Map P.PkgName (Map P.FlagName Bool), b ~ Map P.PkgName (Map P.FlagName Bool)) => LabelOptic "flags" k S S a b where
    labelOptic = lensVL $ \f (S a b c x d) -> f x <&> \x' -> S a b c x' d

instance (k ~ A_Lens, a ~ Set GitRepo, b ~ Set GitRepo) => LabelOptic "gitPackages" k S S a b where
    labelOptic = lensVL $ \f (S a b c d x) -> f x <&> \x' -> S a b c d x'

emptyS :: S
emptyS = S Set.empty Map.empty Map.empty Map.empty Set.empty

processUnits :: Foldable f => f P.Unit -> IO S
processUnits units = execStateT (traverse_ f units) emptyS where
    f :: P.Unit -> StateT S IO ()
    f unit = do
        let P.PkgId pn ver = P.uPId unit
        checkMultipleVersions pn ver

        #flags %= Map.insert pn (P.uFlags unit)

        case P.uPkgSrc unit of
            Nothing -> pure ()

            Just (P.LocalUnpackedPackage fp) -> do
                #packages %= Set.insert fp

            Just (P.RepoTarballPackage _) -> do
                #extraDeps % at pn ?= ver

            Just (P.RemoteSourceRepoPackage (P.SourceRepo { P.srType = Just P.Git, P.srLocation = Just l, P.srTag = Just t, P.srSubdir = d })) -> do
                #gitPackages %= Set.insert (GitRepo l t d)

            Just ps -> lift $ do
                hPutStrLn stderr $ "Not implemented PkgLoc: " ++ show ps
                exitFailure

    checkMultipleVersions :: P.PkgName -> P.Ver -> StateT S IO ()
    checkMultipleVersions pn ver = do
        mver' <- #pkgVers % at pn <<?= ver
        for_ mver' $ \ver' -> unless (ver == ver') $
            lift $ hPutStrLn stderr $ T.unpack $ "WARN: package " <> dispPkgName pn <> " has multiple versions: " <> P.dispVer ver <> " and " <> P.dispVer ver'

-------------------------------------------------------------------------------
-- stack.yaml
-------------------------------------------------------------------------------

-- TODO: group GitRepos
-- TODO: sha256 hashes on extra-deps?
data StackYaml = StackYaml
    { syResolver    :: !P.PkgId
    , sySystemGHC   :: !Bool
    , syAllowNewer  :: !Bool
    , syPackages    :: !(Set FilePath)
    , syExtraDeps   :: !(Set P.PkgId)
    , syFlags       :: !(Map P.PkgName (Map P.FlagName Bool))
    , syGitPackages :: !(Set GitRepo)
    }
  deriving Show

instance (k ~ A_Lens, a ~ Set GitRepo, b ~ Set GitRepo) => LabelOptic "gitPackages" k StackYaml StackYaml a b where
    labelOptic = lensVL $ \f s -> f (syGitPackages s) <&> \x -> s { syGitPackages = x }

instance Y.ToYAML StackYaml where
    toYAML StackYaml {..} = Y.mapping
        [ "resolver"    Y..= P.dispPkgId syResolver
        , "system-ghc"  Y..= sySystemGHC
        , "allow-newer" Y..= syAllowNewer
        , "packages"    Y..= map T.pack (Set.toList syPackages)
        , "extra-deps"  Y..= extraDeps
        , "flags"       Y..= Y.mapping
            [ pn Y..= Y.mapping
                [ fn Y..= fv
                | (P.FlagName fn, fv) <- Map.toList flags
                ]
            | (P.PkgName pn, flags) <- Map.toList syFlags
            , not (null flags)
            ]
        ]
      where
        extraDeps = map (Y.toYAML . P.dispPkgId) (Set.toList syExtraDeps) ++
            [ Y.mapping
                [ "git"     Y..= l
                , "commit"  Y..= t
                , "subdirs" Y..= [maybe "." T.pack d]
                ]

            | GitRepo l t d <- Set.toList syGitPackages
            ]

-------------------------------------------------------------------------------
-- utilities
-------------------------------------------------------------------------------

traverseSet :: (Applicative f, Ord b) => (a -> f b) -> Set a -> f (Set b)
traverseSet f xs = Set.fromList <$> traverse f (Set.toList xs)

catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = catch

handleIO :: (IOException -> IO a) -> IO a -> IO a
handleIO = flip catchIO

findM :: Monad m => [a] -> (a -> m (Maybe b)) -> m (Maybe b)
findM []     _ = return Nothing
findM (x:xs) f = do
    y <- f x
    case y of
        Just _  -> return y
        Nothing -> findM xs f

isExitSuccess :: ExitCode -> Bool
isExitSuccess ExitSuccess     = True
isExitSuccess (ExitFailure _) = False

dispPkgName :: P.PkgName -> Text
dispPkgName (P.PkgName n) = n
