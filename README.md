
<!-- README.md is generated from README.Rmd. Please edit that file -->
sparrow
=======

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://img.shields.io/badge/lifecycle-experimental-orange.svg)

This package currently handles only the 1992 National Scale Phosphorus Model.

Installation
------------

You can install sparrow from github with:

``` r
# install.packages("devtools")
devtools::install_github("jsta/sparrow")
```

Data
----

GIS data can be obtained automatically with the `erf_get` function. Phosphorus data must be obtained manually and placed at the location returned by `sparrow:::cache_path()` before running `p_get` (until the SPARROW decision support system is made available again).

Further Reading
---------------

See <https://water.usgs.gov/nawqa/sparrow/>
