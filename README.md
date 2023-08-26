
<!-- README.md is generated from README.Rmd. Please edit that file -->

# formods <img src="man/figures/logo.png" align="right" height="138.5" />

<!-- badges: start -->
<!---
[![Codecov test coverage](https://codecov.io/gh/john-harrold/formods/branch/master/graph/badge.svg)](https://app.codecov.io/gh/john-harrold/formods?branch=master)
--->

[![version](https://www.r-pkg.org/badges/version/formods)](https://CRAN.R-project.org/package=formods)
![cranlogs](https://cranlogs.r-pkg.org/badges/formods)
![Active](https://www.repostatus.org/badges/latest/active.svg)
[![Lifecycle:
Experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check](https://github.com/john-harrold/formods/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/john-harrold/formods/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Shiny apps can often make use of the same key elements,
[formods](https://formods.ubiquity.tools) provides modules for common
tasks (data upload, wragling data, figure genration and saving the app
state). These modules can react and interact as well as generate code to
create reproducable analyses. formods also defines a framework for
creating reactive modules. The vignettes outline how to use these
modules as well as how to create other modules within this framework.

# Installation

## From CRAN
You can install the released version of formods from [CRAN](https://cran.r-project.org/package=formods) with:

``` r
install.packages("formods")
```

## Development version

You can install the development version from
[GitHub](https://github.com/john-harrold/formods) with:

``` r
# Installing devtools if it's not already installed
if(system.file(package="devtools") == ""){
  install.packages("devtools") 
}
devtools::install_github("john-harrold/onbrand", dependencies=TRUE)
devtools::install_github("john-harrold/formods", dependencies=TRUE)
```

Note that because `formods` depends on `onbrand` you will need to first
install the development version of `onbrand`.

# Getting started

``` r
library(shiny)
library(formods)
runApp(system.file(package="formods", "templates","FM_compact.R"))
```
