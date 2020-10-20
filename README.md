
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/package_logo.png" style="margin: 0 auto; width: 120px; valign: top" align = 'right'  alt="mixedup Logo" width = 120><br>

# mixedup

##### a package for extracting clean results from mixed models

<br> <br>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Codecov test
coverage](https://codecov.io/gh/m-clark/mixedup/branch/master/graph/badge.svg)](https://codecov.io/gh/m-clark/mixedup?branch=master)
[![R build
status](https://github.com/m-clark/mixedup/workflows/R-CMD-check/badge.svg)](https://github.com/m-clark/mixedup/actions)
<!-- badges: end -->

This package provides extended functionality for mixed models. The goal
of `mixedup` is to solve little problems I have had that slip through
the cracks from the various modeling packages and others in trying to
get presentable output. Basically the idea is to create (tidy) objects
that are easy to use and essentially ready for presentation, as well as
*consistent* across packages and across functions. Such objects would be
things like variance components and random effects. I use several of
these packages (including mgcv) for mixed models, and typically have to
do some notable post processing to get some viable output even with
`broom::tidy`, and this effort often isn’t applicable if I switch to
another package for the same type of model. These functions attempt to
address this issue.

For more details and examples see <https://m-clark.github.io/mixedup/>.

## Installation

You can install mixedup from GitHub with `remotes`. Use the second
approach if you don’t already have `rstanarm` or `brms` (they aren’t
required to use in general).

``` r
remotes::install_github('m-clark/mixedup')

# if you don't already have rstanarm and/or brms

withr::with_envvar(c(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true"), 
  remotes::install_github('m-clark/mixedup')
)
```

## Supported models

  - `lme4`
  - `glmmTMB`
  - `nlme`
  - `mgcv`
  - `rstanarm`
  - `brms`

## Feature list

  - Extract Variance Components
  - Extract Random Effects
  - Extract Fixed Effects
  - Extract Random Coefficients
  - Extract Heterogeneous Variances
  - Extract Correlation Structure
  - Extract Model Data
  - Summarize Model
  - Find Typical

Not all features are available to the various modeling packages
(e.g. autocorrelation for `lme4`), and some functionality may just not
be supported for this package, but most functions are applicable to the
packages listed.

## Examples

### Setup

In the following I suppress the package startup and other information
that isn’t necessary for demo.

``` r
library(lme4)

lmer_model <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)

library(glmmTMB)

tmb_model <- glmmTMB(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)

library(nlme)

nlme_model <-  nlme(
  height ~ SSasymp(age, Asym, R0, lrc),
  data = Loblolly,
  fixed = Asym + R0 + lrc ~ 1,
  random = Asym ~ 1,
  start = c(Asym = 103, R0 = -8.5, lrc = -3.3)
)

library(brms)

# brm_model = brm(
#   Reaction ~ Days + (1 + Days | Subject), 
#   data = sleepstudy, 
#   refresh = -1,
#   verbose = FALSE,
#   open_progress = FALSE,
#   cores = 4,
#   iter = 1000
# )

library(rstanarm)

# rstanarm_model = stan_glmer(
#   Reaction ~ Days + (1 + Days | Subject), 
#   data = sleepstudy, 
#   refresh = -1,
#   verbose = FALSE,
#   show_messages = FALSE,
#   open_progress = FALSE,
#   cores = 4,
#   iter = 1000
# )

library(mgcv)

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
  term   value    se     z p_value lower_2.5 upper_97.5
  <chr>  <dbl> <dbl> <dbl>   <dbl>     <dbl>      <dbl>
1 Asym  101.   2.46   41.2       0     96.5      106.  
2 R0     -8.63 0.318 -27.1       0     -9.26      -7.99
3 lrc    -3.23 0.034 -94.4       0     -3.30      -3.16

extract_random_coefs(lmer_model)
# A tibble: 36 x 7
   group_var effect    group value    se lower_2.5 upper_97.5
   <chr>     <chr>     <fct> <dbl> <dbl>     <dbl>      <dbl>
 1 Subject   Intercept 308    254.  13.9      226.       281.
 2 Subject   Intercept 309    211.  13.9      184.       238.
 3 Subject   Intercept 310    212.  13.9      185.       240.
 4 Subject   Intercept 330    275.  13.9      248.       302.
 5 Subject   Intercept 331    274.  13.9      246.       301.
 6 Subject   Intercept 332    260.  13.9      233.       288.
 7 Subject   Intercept 333    268.  13.9      241.       295.
 8 Subject   Intercept 334    244.  13.9      217.       271.
 9 Subject   Intercept 335    251.  13.9      224.       278.
10 Subject   Intercept 337    286.  13.9      259.       313.
# … with 26 more rows

extract_vc(brm_model, ci_level = .8)
     group    effect variance     sd  sd_10  sd_90 var_prop
1  Subject Intercept  680.180 26.080 18.482 34.451    0.488
2  Subject      Days   44.483  6.670  4.867  8.544    0.032
3 Residual            669.604 25.877 24.171 27.862    0.480

summarize_model(lmer_model, cor_re = TRUE, digits = 1)
Computing profile confidence intervals ...

Variance Components:
    Group    Effect Variance   SD SD_2.5 SD_97.5 Var_prop
  Subject Intercept    612.1 24.7   14.4    37.7      0.5
  Subject      Days     35.1  5.9    3.8     8.8      0.0
 Residual              654.9 25.6   22.9    28.9      0.5

Correlation of Random Effects:
          Intercept Days
Intercept       1.0  0.1
Days            0.1  1.0

Fixed Effects:
      Term Value  SE    t P_value Lower_2.5 Upper_97.5
 Intercept 251.4 6.8 36.8     0.0     238.0      264.8
      Days  10.5 1.5  6.8     0.0       7.4       13.5

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

### Consistent output

``` r
mods = list(
  tmb  = tmb_model,
  lmer = lmer_model, 
  brm  = brm_model,
  stan = rstanarm_model,
  gam  = gam_model
)

purrr::map_df(mods, extract_vc, .id = 'model') 
Computing profile confidence intervals ...
   model    group    effect variance     sd sd_2.5 sd_97.5 var_prop
1    tmb  Subject Intercept  565.516 23.781 15.017  37.658    0.451
2    tmb  Subject      Days   32.682  5.717  3.805   8.588    0.026
3    tmb Residual            654.942 25.592     NA      NA    0.523
4   lmer  Subject Intercept  612.100 24.741 14.381  37.716    0.470
5   lmer  Subject      Days   35.072  5.922  3.801   8.753    0.027
6   lmer Residual            654.940 25.592 22.898  28.858    0.503
7    brm  Subject Intercept  680.180 26.080 16.324  37.155    0.488
8    brm  Subject      Days   44.483  6.670  4.424  10.270    0.032
9    brm Residual            669.604 25.877 23.519  28.958    0.480
10  stan  Subject Intercept  596.717 24.428 12.255  35.737    0.448
11  stan  Subject      Days   45.829  6.770  4.374   9.755    0.034
12  stan Residual            688.537 26.240  4.790   5.382    0.517
13   gam  Subject Intercept  627.571 25.051 16.085  39.015    0.477
14   gam  Subject      Days   35.858  5.988  4.025   8.908    0.027
15   gam Residual            653.582 25.565 22.792  28.676    0.496
```

## Code of Conduct

Please note that the ‘mixedup’ project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md).

By contributing to this project, you agree to abide by its terms.
