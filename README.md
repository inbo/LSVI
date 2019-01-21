[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![wercker status](https://app.wercker.com/status/91ffc2688434d321a85dc5d79fd7c428/s/develop "wercker status")](https://app.wercker.com/project/byKey/91ffc2688434d321a85dc5d79fd7c428)
[![codecov](https://codecov.io/gh/inbo/LSVI/branch/develop/graph/badge.svg)](https://codecov.io/gh/inbo/LSVI)

# Local conservation status for Natura 2000 habitats in Flanders

The `LSVI` package bundles a number of functions to support researchers studying the local conservation status of habitats. Several functions give information on the criteria that are used to determine the local conservation status in Flanders, others allow to calculate the conservation status. Criteria are stored in a database, which is indispensable for running functions. (The database is now external for development reasons, but it will be integrated in the package as soon as it is completed).

The package is written in Dutch.

# Installation

To install the development version, start a new R session and run this code (before loading any packages). 

```r
# install.package("devtools")
devtools::install_github("inbo/LSVI@develop", build_vignettes = TRUE)
```

To have access to the database (which is a precondition for using the package), one should be logged in as a user on the INBO network (locally or by using VPN).

# Citation

Please use the output of `citation("LSVI")`

# Contributor Code of Conduct

Please note that the 'LSVI' project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.
