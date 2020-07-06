[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![wercker status](https://app.wercker.com/status/91ffc2688434d321a85dc5d79fd7c428/s/develop "wercker status")](https://app.wercker.com/project/byKey/91ffc2688434d321a85dc5d79fd7c428)
[![codecov](https://codecov.io/gh/inbo/LSVI/branch/develop/graph/badge.svg)](https://codecov.io/gh/inbo/LSVI)

# Local conservation status for Natura 2000 habitats in Flanders

The `LSVI` package bundles a number of functions to support researchers studying the local conservation status of habitats. Several functions give information on the criteria that are used to determine the local conservation status in Flanders, others allow to calculate the conservation status. Criteria are stored in a database, which is integrated in the package and indispensable for running functions.

The package is written in Dutch.

# Installation

To install `LSVI` from Github, start a new R session and run this code (before loading any packages):

```r
# install.packages("remotes")
remotes::install_github("inbo/LSVI", build_vignettes = TRUE)
```

To install the development version with the latest changes included (but less stable), run this code: 

```r
remotes::install_github("inbo/LSVI@develop", build_vignettes = TRUE)
```

# Documentation

All helpfiles and vignettes are available at [https://inbo.github.io/LSVI/](https://inbo.github.io/LSVI/)

# Citation

Please use the output of `citation("LSVI")`

# Contributor Code of Conduct

Please note that the 'LSVI' project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.
