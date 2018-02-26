Stackage-Everything generator
=============================

tl;dr: »I want Stackage on an airplane«

This small script generates a script to prefetch all the sources of a Stackage
LTS release, so they can later be installed (including documentation) even when
no internet connection is available. So if you’ve got a long flight ahead of
you, or a weekend with your Granny in Siberia, or a dive into the Mariana
Trench, or even worse – a trip through the German countryside by train – this is
for you.

Usage
-----

```bash
./Generate.hs --lts 10.6 > everything # Generate install script
chmod u+x everything                  # Make it runnable
./everything                          # Run it
```

Afterwards, you’ll be able to compile packages via the usual means (`stack
build` or as dependencies) without internet access, because Stack first searches
the local folders for source files before attempting to download them.
