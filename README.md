# cabal2stack

This is a tool to create a `stack.yaml` file replicating a build plan
constructed by `cabal-install`.  To use, first run `cabal build --dry-run` to
produce a `plan.json` file under `dist-newstyle/cache`, then run `cabal2stack`
to output a corresponding `stack.yaml`.
