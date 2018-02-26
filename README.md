Stackage-Everything generator
=============================

tl;dr: »I want Stackage on an airplane«

This small script generates a script to prefetch all the sources of a Stackage
LTS release. If you’ve got a long flight ahead of you, a weekend with your
Granny in Siberia, or a dive into in the Mariana Trench, and you’d like to have
all the packages available to you should you need them, this is for you.
Usage
-----

```bash
./Generate.hs > stackage-everything.bash # Generate install script
chmod u+x stackage-everything.bash       # Make it runnable
./stackage-everything.bash               # Run it
```
