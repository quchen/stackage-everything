Stackage-Everything generator
=============================

tl;dr: »I want Stackage on an airplanes

This meta-meta-package generates packages to depend on the entirety of Stackage
of a certain LTS release. This is useful in conjunction with Stack’s package
preloading feature: prefetch all the sources of Stackage before boarding a
plane, and everything’s available even if there is no internet. Required
packages, or even just their documentation, can then be built without being
online.

Usage
-----

First, get the Cabal config of the desired Stackage release.

```bash
wget -O "lts-10.5" "https://www.stackage.org/lts-10.5/cabal.config"
```

Next, adjust the parameters in `main` of `Generate.hs` to your likings, like
which LTS to base the result on, and where the `cabal.config` file is located
(relative to the script).

Now run `./Generate.hs`. The result will be put into the `output` directory,
where you can od various things with it, for example prefetch all sources,

```bash
( cd output && stack build --dry-run --prefetch )
```
