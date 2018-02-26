Stackage-Everything generator
=============================

This meta-meta-package generates packages to depend on the entirety of Stackage
packages of a certain LTS release. This is useful in conjunction with Stack’s
package preloading feature: get all the sources of Stackage before boarding a
plane, and everything’s available even if there is no internet.

Usage
-----

First, get the Cabal config of the desired Stackage release.

```bash
# Script must be run from "."!
wget -O "lts-10.5" "https://www.stackage.org/lts-10.5/cabal.config"
```

Next, adjust the parameters at the top of `Generate.hs` to your likings. Note
that the script assumes that the downloaded cabal.config is in its working
directory (because I was lazy when I wrote it).

Now run `./Generate.hs`. The result will be put into the `output` directory,
where you can od various things with it.

```bash
set -euo pipefail
./Generate.hs
( cd output && cabal check && cabal sdist ) # Build proper package
( cd output && stack build --dry-run --prefetch ) # Download all sources
```
