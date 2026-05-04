# Local conservation status for Natura 2000 habitats in Flanders

The `LSVI` package bundles a number of functions to support researchers
studying the local conservation status of habitats. Several functions
give information on the criteria that are used to determine the local
conservation status in Flanders, others allow to calculate the
conservation status. Criteria are stored in a database, which is
indispensable for running functions. (The database is now external for
development reasons, but it will be integrated in the package as soon as
it is completed).

The package is written in Dutch.

# Installation

To install `LSVI` from the [INBO
universe](https://inbo.r-universe.dev/ui#builds), start a new R session
and run this code (before loading any packages):

``` r

# Enable the INBO universe (not needed for INBO employees, as this is the default setting)
options(
  repos = c(
    inbo = "https://inbo.r-universe.dev", CRAN = "https://cloud.r-project.org"
  )
)
# Install the packages
install.packages("LSVI")
```

To install `LSVI` from GitHub, start a new R session and run this code
(before loading any packages):

``` r

# install.packages("remotes")
remotes::install_github("inbo/LSVI")
```

To install the development version with the latest changes included (but
less stable), run this code:

``` r

remotes::install_github("inbo/LSVI@develop")
```

# Citation

Please use the output of `citation("LSVI")`

# Contributor Code of Conduct

Please note that the ‘LSVI’ project is released with a [Contributor Code
of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you
agree to abide by its terms.
