
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mixedup

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/m-clark/mixedup.svg?branch=master)](https://travis-ci.org/m-clark/mixedup)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/m-clark/mixedup?branch=master&svg=true)](https://ci.appveyor.com/project/m-clark/mixedup)
[![Codecov test
coverage](https://codecov.io/gh/m-clark/mixedup/branch/master/graph/badge.svg)](https://codecov.io/gh/m-clark/mixedup?branch=master)
<!-- badges: end -->

This package provides extended functionality for mixed models. The goal
of `mixedup` is to solve little problems that slip through the cracks
from the various packages, broom, and others in trying to get
presentable output. Basically the idea is to create (tidy) objects that
are easy to use and essentially ready for presentation, as well as
consistent across packages and across functions. I use several of these
packages (including mgcv) for mixed models, and typically have to some
notable post processing to get some basic output even with `broom::tidy`
options, and this effort often isn’t applicable if I switch to another
package for the same model. These functions attempt to fill my specific
niche.

An additional perk is minimal dependency. Other than the package that
created the object, nothing except the tidyverse is needed (presently
just `dplyr`, `tidyr`, and `purrr`).

## Installation

You can install mixedup from GitHub with `devtools`:

``` r
devtools::install_github('m-clark/mixedup')
```

## Feature list

##### Extract Variance Components

  - \[X\] lme4
  - \[X\] glmmTMB
  - \[X\] nlme
  - \[X\] brms
  - \[X\] mgcv

##### Extract Random Effects

  - \[X\] lme4
  - \[X\] glmmTMB
  - \[X\] nlme
  - \[X\] brms
  - \[X\] mgcv

##### Extract Fixed Effects

  - \[X\] lme4
  - \[X\] glmmTMB
  - \[X\] nlme
  - \[X\] brms
  - \[X\] mgcv

##### Extract Random Coefficients

  - \[X\] lme4
  - \[X\] glmmTMB
  - \[X\] nlme
  - \[X\] brms
  - \[X\] mgcv

##### Extract Heterogeneous Variances

  - \[ \] glmmTMB
  - \[X\] nlme

##### Find Typical

  - \[X\] lme4
  - \[X\] glmmTMB
  - \[X\] nlme
  - \[X\] brms
  - \[X\] mgcv

## Examples

### Setup

``` r
library(lme4)
Loading required package: Matrix

lmer_model <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)

library(glmmTMB)

tmb_model <- glmmTMB(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)

library(nlme)

Attaching package: 'nlme'
The following object is masked from 'package:lme4':

    lmList

nlme_model <-  nlme(
  height ~ SSasymp(age, Asym, R0, lrc),
  data = Loblolly,
  fixed = Asym + R0 + lrc ~ 1,
  random = Asym ~ 1,
  start = c(Asym = 103, R0 = -8.5, lrc = -3.3)
)

library(brms)
Loading required package: Rcpp
Registered S3 method overwritten by 'xts':
  method     from
  as.zoo.xts zoo 
Loading 'brms' package (version 2.10.0). Useful instructions
can be found by typing help('brms'). A more detailed introduction
to the package is available through vignette('brms_overview').

Attaching package: 'brms'
The following object is masked from 'package:lme4':

    ngrps

brm_model = brm(
  Reaction ~ Days + (1 + Days | Subject), 
  data = sleepstudy, 
  refresh = -1,
  verbose = FALSE,
  cores = 4
)
Compiling the C++ model
Start sampling

library(mgcv)
This is mgcv 1.8-29. For overview type 'help("mgcv-package")'.

Attaching package: 'mgcv'
The following objects are masked from 'package:brms':

    s, t2

gam_model = gam(
  Reaction ~  Days +
    s(Subject, bs = 're') +
    s(Days, Subject, bs = 're'),
  data = lme4::sleepstudy,
  method = 'REML'
)
```

### Extract Output from a Mixed Model

``` r
library(mixedup)

extract_random_effects(tmb_model)
# A tibble: 36 x 7
   group_var effect    group  value    se lower_2.5 upper_97.5
   <chr>     <chr>     <fct>  <dbl> <dbl>     <dbl>      <dbl>
 1 Subject   Intercept 308     2.82  13.7    -23.9        29.6
 2 Subject   Intercept 309   -40.0   13.8    -67.2       -12.9
 3 Subject   Intercept 310   -38.4   13.7    -65.4       -11.5
 4 Subject   Intercept 330    22.8   13.9     -4.51       50.2
 5 Subject   Intercept 331    21.6   13.6     -5.11       48.2
 6 Subject   Intercept 332     8.82  12.9    -16.5        34.1
 7 Subject   Intercept 333    16.4   13.1     -9.23       42.1
 8 Subject   Intercept 334    -7.00  12.9    -32.3        18.3
 9 Subject   Intercept 335    -1.04  14.0    -28.5        26.4
10 Subject   Intercept 337    34.7   13.6      7.94       61.4
# … with 26 more rows

extract_fixed_effects(nlme_model)
# A tibble: 3 x 7
  term   value    se     t p_value lower_2.5 upper_97.5
  <chr>  <dbl> <dbl> <dbl>   <dbl>     <dbl>      <dbl>
1 Asym  101.   2.46   41.2       0     96.5      106.  
2 R0     -8.63 0.318 -27.1       0     -9.26      -7.99
3 lrc    -3.23 0.034 -94.4       0     -3.30      -3.16

extract_random_coefs(lmer_model)
# A tibble: 36 x 7
   group_var effect    group value    se lower_2.5 upper_97.5
   <chr>     <chr>     <fct> <dbl> <dbl>     <dbl>      <dbl>
 1 Subject   Intercept 308    254.  18.9      217.       291.
 2 Subject   Intercept 309    211.  18.9      174.       248.
 3 Subject   Intercept 310    212.  18.9      175.       249.
 4 Subject   Intercept 330    275.  18.9      238.       312.
 5 Subject   Intercept 331    274.  18.9      237.       311.
 6 Subject   Intercept 332    260.  18.9      223.       297.
 7 Subject   Intercept 333    268.  18.9      231.       305.
 8 Subject   Intercept 334    244.  18.9      207.       281.
 9 Subject   Intercept 335    251.  18.9      214.       288.
10 Subject   Intercept 337    286.  18.9      249.       323.
# … with 26 more rows

extract_vc(brm_model, ci_level = .8)
     group    effect variance     sd  sd_10  sd_90 var_prop
1  Subject Intercept  716.876 26.775 18.605 35.788    0.501
2  Subject      Days   42.170  6.494  4.787  8.353    0.029
3 Residual            672.616 25.935 23.967 28.021    0.470

find_typical(gam_model, probs = c(.25, .50, .75))
# A tibble: 6 x 8
  group_var effect    group   value    se lower_2.5 upper_97.5 probs
  <chr>     <chr>     <chr>   <dbl> <dbl>     <dbl>      <dbl> <chr>
1 Subject   Days      331    -3.19   2.67     -8.43       2.04 25%  
2 Subject   Days      369     0.873  2.67     -4.36       6.11 50%  
3 Subject   Days      352     3.51   2.67     -1.73       8.75 75%  
4 Subject   Intercept 350   -13.9   13.3     -39.9       12.2  25%  
5 Subject   Intercept 369     3.26  13.3     -22.8       29.3  50%  
6 Subject   Intercept 333    17.2   13.3      -8.87      43.2  75%  
```

## Consistent output

``` r
extract_vc(tmb_model)
     group    effect variance     sd sd_2.5 sd_97.5 var_prop
1  Subject Intercept  565.516 23.781 15.017  37.658    0.451
2  Subject      Days   32.682  5.717  3.805   8.588    0.026
3 Residual            654.941 25.592 22.800  28.725    0.523


extract_vc(nlme_model)
     group effect variance    sd sd_2.5 sd_97.5 var_prop
1     Seed   Asym   13.327 3.651  2.479   5.375    0.963
2 Residual           0.517 0.719  0.609   0.849    0.037


extract_vc(lmer_model)
Computing profile confidence intervals ...
     group    effect variance     sd sd_2.5 sd_97.5 var_prop
1  Subject Intercept  611.898 24.737 14.382  37.714    0.470
2  Subject      Days   35.081  5.923  3.801   8.754    0.027
3 Residual            654.941 25.592 22.898  28.858    0.503


extract_vc(brm_model)
     group    effect variance     sd sd_2.5 sd_97.5 var_prop
1  Subject Intercept  716.876 26.775 15.478  42.653    0.501
2  Subject      Days   42.170  6.494  4.138   9.854    0.029
3 Residual            672.616 25.935 23.066  29.206    0.470


extract_vc(gam_model)
     group    effect variance     sd sd_2.5 sd_97.5 var_prop
1  Subject Intercept  627.571 25.051 16.085  39.015    0.477
2  Subject      Days   35.858  5.988  4.025   8.908    0.027
3 Residual            653.582 25.565 22.792  28.676    0.496
```

### Code of Conduct

Please note that the ‘mixedup’ project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md).

By contributing to this project, you agree to abide by its terms.
