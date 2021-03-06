
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![Codecov test
coverage](https://codecov.io/gh/BarkleyBG/glmerGOF/branch/master/graph/badge.svg)](https://codecov.io/gh/BarkleyBG/glmerGOF?branch=master)
[![Travis-CI Build
Status](https://travis-ci.org/BarkleyBG/glmerGOF.svg?branch=master)](https://travis-ci.org/BarkleyBG/glmerGOF)
<!-- badges: end -->

# glmerGOF

The goal of glmerGOF is to provide a goodness of fit test of the
presumed Gaussian distribution of the random effect in logistic mixed
models fit with `lme4::glmer(family = "binomial")`. The method
implemented is introduced in Tchetgen Tchetgen and Coull (2006):

Tchetgen Tchetgen, E. J., & Coull, B. A. (2006) *A Diagnostic Test for
the Mixing Distribution in a Generalised Linear Mixed Model*.
Biometrika, 93(4), 1003-1010. DOI:
[10.1093/biomet/93.4.1003](https://doi.org/10.1093/biomet/93.4.1003)

## Installation

glmerGOF is available through github:

``` r
remotes::install_github("BarkleyBG/glmerGOF")
```

## Usage

An introductory tutorial is provided that describes how to use this
package. The function that implements the test is `glmerGOF::testGOF()`,
which takes as mandatory input:

  - a fitted `lme4::glmer()` model
  - a fitted `survival::clogit()` model
  - the original dataset
  - a list providing two variable names

Once the two models are fitted, then test statistics can be found:

``` r
test_results <- testGOF(
  data = my_data,
  fitted_model_clogit = fit_clogit,
  fitted_model_glmm  = fit_glmm,
  var_names = list(DV = "y", grouping = "id"), 
  gradient_derivative_method = "simple"
)
```

The test results can be shown as:

``` r
test_results$results
# $D
# [1] 1.230103
# 
# $p_value
# [1] 0.267387
```

Please see the Introduction vignette for a working example.

# Potential future developments

Please refer to the lifecycle badge for its current status. This package
was created in early 2019 and may undergo future developments. Please
email the maintainer if any of these changes are of interest, or if you
would like to work on them:

  - Ability to manually align glmer and clogit coefficients
  - Parallelization backend
  - Updated model options

# Contributor Code of Conduct

Please note that the ‘glmerGOF’ project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.
