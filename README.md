Stackage-Everything generator
=============================

This meta-meta-package generates packages to depend on the entirety of
Stackage packages of a certain LTS release.

```bash
# Script must be run from "."!
wget -O "lts-5.4" "https://www.stackage.org/lts-5.4/cabal.config"
# Adjust the parameters at the top of Generate.hs
./Generate.hs && ( cd output && cabal check && cabal sdist )
```

Then go to `output/dist` and upload the generated source distribution.
