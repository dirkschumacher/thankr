
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/thankr)](https://cran.r-project.org/package=thankr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/dirkschumacher/thankr?branch=master&svg=true)](https://ci.appveyor.com/project/dirkschumacher/thankr) [![Travis-CI Build Status](https://travis-ci.org/dirkschumacher/thankr.svg?branch=master)](https://travis-ci.org/dirkschumacher/thankr) [![Coverage Status](https://img.shields.io/codecov/c/github/dirkschumacher/thankr/master.svg)](https://codecov.io/github/dirkschumacher/thankr?branch=master)

thankr
======

> standing on the shoulders of giants

Find out what packages you use and maybe thank the authors :)

Install
-------

### From CRAN

``` r
install.packages("thankr")
```

### Latest development version

``` r
devtools::install_github("dirkschumacher/thankr")
```

Usage
-----

``` r
library(thankr)
```

``` r
# identifies the packages currently loaded in your session
shoulders() # equal to shoulders("session")
```

``` r
# shows all maintainers of your package library
shoulders("library")
```

``` r
# find out about a specific package
# by default it lists all dependencies of that package as well
shoulders("package", "ropenaq", include_dependencies = FALSE)
shoulders("package", "ropenaq")
```

Only in the development version
-------------------------------

Everything here is still work in progress. Any feedback appreciated.

Find out what packages you have already starred on Github. Inspired by the node module [appreciate](https://github.com/musically-ut/appreciate).

``` r
gh_starred(c("ropenaq", "Rcpp", "dplyr"))
#> hadley/dplyr:         ★ Starred. 
#> RcppCore/Rcpp:        ★ Starred. 
#> ropensci/ropenaq:     ★ Starred.
```

If you are developing a package, you can call `appreciate` in your working directory. If there is a `DESCRIPTION` file, it parses it and checks if you have already starred the dependencies on github. The function fails, if the working directory does not contain a `DESCRIPTION` file.

``` r
thankr::appreciate()
#> Checking dependencies for thankr:
#> gaborcsardi/crayon:     ★ Starred. 
#> hadley/testthat:        ★ Starred. 
#> jimhester/covr:         ★ Starred. 
#> ropensci/crul:          ★ Starred.
```

Contributing
------------

If you found a bug or want to propose a feature, feel free to visit the [issues page](https://github.com/dirkschumacher/thankr/issues).
