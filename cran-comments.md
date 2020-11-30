# CRAN notes for allcontributors_0.0.2 submission

This package enables all developers with packages hosted on github to acknowledge all contributions to their packages with a single function call. The ability to do so will enable easier, and hopefully more widespread, acknowledgement of the communities surrounding and involved in the development of R packages.

In response to comments on previous attempt to submit, the package now includes example code for most functions, although these are all wrapped in "\dontrun{}", because they all rely on external URL calls. The package nevertheless contains a test suite which covers most functionality by mocking the results of these calls with internal data. Finally, there are no references for the methodology of this package, and so no DOI is able to be provided, because the package is a "meta" package to acknowledge and encourage community engagement. It extends a number of prior systems, none of which are representing in published forms able to be cited via DOIs.

The submission generates no notes or warnings on:

* Ubuntu 18.04: R-oldrelease, R-release
* Windows: R-oldrelease, R-release, R-devel
* win-builder (R-release, R-devel, R-oldrelease)
