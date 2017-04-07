
[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

thankr
======

> standing on the shoulders of giants

Find out what packages you use and maybe thank the authors :)

Install
-------

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
shoulders() # equal to shoulders("namespace")
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
