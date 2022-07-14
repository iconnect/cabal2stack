{-# LANGUAGE ApplicativeDo          #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE UndecidableInstances   #-}
module Main (main) where

import Control.Applicative       ((<**>), (<|>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, execStateT)
import Data.Foldable             (traverse_)
import Data.Map.Strict           (Map)
import Data.Set                  (Set)
import Data.Text                 (Text)
import System.Directory          (getCurrentDirectory)
import System.Exit               (exitFailure)
import System.FilePath           (makeRelative)
import System.IO                 (hPutStrLn, stderr)

import Optics                 (A_Lens, LabelOptic (..), lensVL, (<&>))
import Optics.State.Operators ((%=))

import qualified Cabal.Plan           as P
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict      as Map
import qualified Data.Set             as Set
import qualified Data.Text            as T
import qualified Data.YAML            as Y
import qualified Options.Applicative  as O

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    Opts {..} <- O.execParser $ O.info (optsP <**> O.helper)
        (O.fullDesc <> O.progDesc "Create stack.yaml from cabal-install's plan.json")

    cwd <- getCurrentDirectory

    plan <- P.findAndDecodePlanJson (P.ProjectRelativeToDir cwd)

    S packages extraDeps flags gitPackages <- processUnits (P.pjUnits plan)

    let stackYaml :: StackYaml
        stackYaml = StackYaml
            { syResolver    = P.pjCompilerId plan
            , sySystemGHC   = optsSystemGHC
            , syAllowNewer  = optsAllowNewer
            , syPackages    = Set.map (makeRelative cwd) packages
            , syExtraDeps   = extraDeps
            , syFlags       = flags
            , syGitPackages = gitPackages
            }

    case optsOutput of
        "-" -> LBS.putStr (Y.encode [stackYaml])
        _   -> LBS.writeFile optsOutput (Y.encode [stackYaml])

-------------------------------------------------------------------------------
-- options
-------------------------------------------------------------------------------

data Opts = Opts
    { optsSystemGHC  :: !Bool
    , optsAllowNewer :: !Bool
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

    optsOutput <- O.strOption (O.short 'o' <> O.long "output" <> O.value "stack.yaml" <> O.help "Output location")

    pure Opts {..}

-------------------------------------------------------------------------------
-- units
-------------------------------------------------------------------------------

data S = S
    { _sPackages  :: !(Set FilePath)
    , _sExtraDeps :: !(Set P.PkgId)
    , _sFlags     :: !(Map P.PkgName (Map P.FlagName Bool))
    , _sGitPkgs   :: !(Set GitRepo)
    }
  deriving Show

data GitRepo = GitRepo !Text !Text !(Maybe FilePath)
  deriving (Show, Eq, Ord)

instance (k ~ A_Lens, a ~ Set FilePath, b ~ Set FilePath) => LabelOptic "packages" k S S a b where
    labelOptic = lensVL $ \f (S x a b c) -> f x <&> \x' -> S x' a b c

instance (k ~ A_Lens, a ~ Set P.PkgId, b ~ Set P.PkgId) => LabelOptic "extraDeps" k S S a b where
    labelOptic = lensVL $ \f (S a x b c) -> f x <&> \x' -> S a x' b c

instance (k ~ A_Lens, a ~ Map P.PkgName (Map P.FlagName Bool), b ~ Map P.PkgName (Map P.FlagName Bool)) => LabelOptic "flags" k S S a b where
    labelOptic = lensVL $ \f (S a b x c) -> f x <&> \x' -> S a b x' c

instance (k ~ A_Lens, a ~ Set GitRepo, b ~ Set GitRepo) => LabelOptic "gitPackages" k S S a b where
    labelOptic = lensVL $ \f (S a b c x) -> f x <&> \x' -> S a b c x'

emptyS :: S
emptyS = S Set.empty Set.empty Map.empty Set.empty

processUnits :: Foldable f => f P.Unit -> IO S
processUnits units = execStateT (traverse_ f units) emptyS where
    f :: P.Unit -> StateT S IO ()
    f unit = do
        let P.PkgId pn _ = P.uPId unit
        #flags %= Map.insert pn (P.uFlags unit)

        case P.uPkgSrc unit of
            Nothing -> return ()
            Just (P.LocalUnpackedPackage fp) -> do
                #packages %= Set.insert fp

            Just (P.RepoTarballPackage _) -> do
                #extraDeps %= Set.insert (P.uPId unit)

            Just (P.RemoteSourceRepoPackage (P.SourceRepo { P.srType = Just P.Git, P.srLocation = Just l, P.srTag = Just t, P.srSubdir = d })) -> do
                #gitPackages %= Set.insert (GitRepo l t d)

            Just ps -> lift $ do
                hPutStrLn stderr $ "Not implemented PkgLoc: " ++ show ps
                exitFailure

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