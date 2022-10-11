
<!-- README.md is generated from README.Rmd. Please edit here -->

<img src="man/figures/package_logo.png" style="margin: 0 auto; width: 120px; valign: top" align = 'right'  alt="mixedup Logo" width = 120><br>

# mixedup

##### a package for extracting clean results from mixed models

<br> <br>

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/m-clark/mixedup/branch/master/graph/badge.svg)](https://codecov.io/gh/m-clark/mixedup?branch=master)
[![R-CMD-check](https://github.com/m-clark/mixedup/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/m-clark/mixedup/actions/workflows/check-standard.yaml)
[![pkgdown](https://github.com/m-clark/mixedup/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/m-clark/mixedup/actions/workflows/pkgdown.yaml)
[![test-coverage](https://github.com/m-clark/mixedup/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/m-clark/mixedup/actions/workflows/test-coverage.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
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

-   `lme4`
-   `glmmTMB`
-   `nlme`
-   `mgcv`
-   `rstanarm`
-   `brms`

## Feature list

-   Extract Variance Components
-   Extract Random Effects
-   Extract Fixed Effects
-   Extract Random Coefficients
-   Extract Heterogeneous Variances
-   Extract Correlation Structure
-   Extract Model Data
-   Summarize Model
-   Find Typical

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
# A tibble: 36 × 7
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
# A tibble: 3 × 7
  term   value    se     z p_value lower_2.5 upper_97.5
  <chr>  <dbl> <dbl> <dbl>   <dbl>     <dbl>      <dbl>
1 Asym  101.   2.46   41.2       0     96.5      106.  
2 R0     -8.63 0.318 -27.1       0     -9.26      -7.99
3 lrc    -3.23 0.034 -94.4       0     -3.30      -3.16

extract_random_coefs(lmer_model)
# A tibble: 36 × 7
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
# A tibble: 3 × 7
  group    effect    variance    sd sd_10 sd_90 var_prop
  <chr>    <chr>        <dbl> <dbl> <dbl> <dbl>    <dbl>
1 Subject  Intercept    793.  28.2  18.7   38.3    0.527
2 Subject  Days          42.2  6.50  4.73   8.1    0.028
3 Residual <NA>         669.  25.9  23.6   28.0    0.445

summarize_model(lmer_model, cor_re = TRUE, digits = 1)
Computing profile confidence intervals ...

Variance Components:
    Group    Effect Variance   SD SD_2.5 SD_97.5 Var_prop
  Subject Intercept    612.1 24.7   14.4    37.7      0.5
  Subject      Days     35.1  5.9    3.8     8.8      0.0
 Residual              654.9 25.6   22.9    28.9      0.5

Fixed Effects:
      Term Value  SE    t P_value Lower_2.5 Upper_97.5
 Intercept 251.4 6.8 36.8     0.0     238.0      264.8
      Days  10.5 1.5  6.8     0.0       7.4       13.5

find_typical(gam_model, probs = c(.25, .50, .75))
# A tibble: 6 × 8
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
# A tibble: 15 × 8
   model group    effect      variance    sd sd_2.5 sd_97.5 var_prop
 * <chr> <chr>    <chr>          <dbl> <dbl>  <dbl>   <dbl>    <dbl>
 1 tmb   Subject  "Intercept"    566.  23.8   15.0    37.7     0.451
 2 tmb   Subject  "Days"          32.7  5.72   3.80    8.59    0.026
 3 tmb   Residual  <NA>          655.  25.6   NA      NA       0.523
 4 lmer  Subject  "Intercept"    612.  24.7   14.4    37.7     0.47 
 5 lmer  Subject  "Days"          35.1  5.92   3.80    8.75    0.027
 6 lmer  Residual ""             655.  25.6   22.9    28.9     0.503
 7 brm   Subject  "Intercept"    793.  28.2   15.8    46.3     0.527
 8 brm   Subject  "Days"          42.2  6.50   4.32    9.28    0.028
 9 brm   Residual  <NA>          669.  25.9   22.5    29.6     0.445
10 stan  Subject  "Intercept"    585.  24.2   12.3    36.3     0.447
11 stan  Subject  "Days"          44.0  6.64   4.00    9.98    0.034
12 stan  Residual  <NA>          680.  26.1   NA      NA       0.519
13 gam   Subject  "Intercept"    628.  25.1   16.1    39.0     0.477
14 gam   Subject  "Days"          35.9  5.99   4.03    8.91    0.027
15 gam   Residual  <NA>          654.  25.6   22.8    28.7     0.496
```

## Code of Conduct

Please note that the ‘mixedup’ project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md).

By contributing to this project, you agree to abide by its terms.
