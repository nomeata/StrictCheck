# Reifying the strictness behavior of Haskell programs within Haskell


Haskell programmers are often confounded by unexpected behaviors related to lazy evaluation. Nevertheless, they frequently make bold claims about the strictness behavior of their functions! As of now, the tools available to observe the evaluation of Haskell programs have only been extrinsic to Haskell,* and often they are intrusive to the running program. While it is useful to observe evaluation “from afar,” manipulating such information from within Haskell offers new opportunities for pedagogy and automated testing, among other things.

This repo is a work-in-progress: a library for dynamically reifying the strictness behavior of a Haskell function; a generic programming library for writing executable specifications of that strictness behavior; and integration with QuickCheck for using those specifications in property-based random testing.

*To our knowledge
