<!-- spell-check: ignore:start -->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![Release](https://img.shields.io/github/release/inbo/LSVI.svg)](https://github.com/inbo/LSVI/releases)
[![R build status](https://github.com/inbo/LSVI/actions/workflows/check_on_different_r_os.yml/badge.svg)](https://github.com/inbo/LSVI/actions)
[![codecov](https://codecov.io/gh/inbo/LSVI/branch/main/graph/badge.svg)](https://codecov.io/gh/inbo/LSVI)
![r-universe
name](https://inbo.r-universe.dev/badges/:name?color=c04384)
[![r-universe package](https://inbo.r-universe.dev/badges/LSVI)](https://inbo.r-universe.dev/LSVI)
![GitHub code size in
bytes](https://img.shields.io/github/languages/code-size/inbo/LSVI.svg)
![GitHub repo
size](https://img.shields.io/github/repo-size/inbo/LSVI.svg)
<!-- badges: end -->
<!-- spell-check: ignore:end -->

# Local conservation status for Natura 2000 habitats in Flanders

The `LSVI` package bundles a number of functions to support researchers studying the local conservation status of habitats. Several functions give information on the criteria that are used to determine the local conservation status in Flanders, others allow to calculate the conservation status. Criteria are stored in a database, which is indispensable for running functions. (The database is now external for development reasons, but it will be integrated in the package as soon as it is completed).

The package is written in Dutch.

# Installation

To install `LSVI` from the [INBO universe](https://inbo.r-universe.dev/ui#builds),
start a new R session and run this code (before loading any packages):

```r
# Enable the INBO universe (not needed for INBO employees, as this is the default setting)
options(
  repos = c(
    inbo = "https://inbo.r-universe.dev", CRAN = "https://cloud.r-project.org"
  )
)
# Install the packages
install.packages("LSVI")
```

To install `LSVI` from GitHub, start a new R session and run this code (before loading any packages):

```r
# install.packages("remotes")
remotes::install_github("inbo/LSVI")
```

To install the development version with the latest changes included (but less stable), run this code: 

```r
remotes::install_github("inbo/LSVI@develop")
```

# Citation

Please use the output of `citation("LSVI")`

# Contributor Code of Conduct

Please note that the 'LSVI' project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.
