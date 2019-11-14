
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

##### Extract Random Effects

  - \[X\] lme4
  - \[X\] glmmTMB
  - \[X\] nlme
  - \[X\] brms

##### Extract Fixed Effects

  - \[X\] lme4
  - \[X\] glmmTMB
  - \[X\] nlme
  - \[X\] brms

##### Extract Random Coefficients

  - \[X\] lme4
  - \[X\] glmmTMB
  - \[X\] nlme
  - \[X\] brms

##### Extract Heterogeneous Variances

  - \[ \] glmmTMB
  - \[X\] nlme

##### Find Typical

  - \[X\] lme4
  - \[X\] glmmTMB
  - \[X\] nlme
  - \[X\] brms

Just a note, <span class="pack" style="">nlme</span> has pretty much
been superseded by <span class="pack" style="">glmmTMB</span>,
<span class="pack" style="">brms</span>, and others, so support here is
pretty minimal.

## Examples

### Setup

``` r
library(lme4)
Loading required package: Matrix

lmer_1 <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)

lmer_2 <- lmer(Reaction ~ Days + (1 + Days| Subject), data = sleepstudy)

library(glmmTMB)

tmb_1 <- glmmTMB(Reaction ~ Days + (1 | Subject), data = sleepstudy)

tmb_2 <- glmmTMB(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)

library(nlme)

Attaching package: 'nlme'
The following object is masked from 'package:lme4':

    lmList

nlme_1 <-  nlme(
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

brm_1 = brm(
  Reaction ~ Days + (1 + Days| Subject), 
  data = sleepstudy, 
  refresh = -1,
  cores = 4
)
Compiling the C++ model
Start sampling
```

### Extract Random Effects

Extract the random effects with their (not necessarily valid) standard
errors from model objects.

``` r
library(mixedup)

extract_random_effects(lmer_1)
# A tibble: 18 x 7
   group_var effect    group  value    se lower_2.5 upper_97.5
   <chr>     <chr>     <fct>  <dbl> <dbl>     <dbl>      <dbl>
 1 Subject   Intercept 308    40.8   9.48    22.2        59.4 
 2 Subject   Intercept 309   -77.8   9.48   -96.4       -59.3 
 3 Subject   Intercept 310   -63.1   9.48   -81.7       -44.5 
 4 Subject   Intercept 330     4.41  9.48   -14.2        23.0 
 5 Subject   Intercept 331    10.2   9.48    -8.36       28.8 
 6 Subject   Intercept 332     8.22  9.48   -10.4        26.8 
 7 Subject   Intercept 333    16.5   9.48    -2.07       35.1 
 8 Subject   Intercept 334    -3.00  9.48   -21.6        15.6 
 9 Subject   Intercept 335   -45.3   9.48   -63.9       -26.7 
10 Subject   Intercept 337    72.2   9.48    53.6        90.8 
11 Subject   Intercept 349   -21.2   9.48   -39.8        -2.62
12 Subject   Intercept 350    14.1   9.48    -4.46       32.7 
13 Subject   Intercept 351    -7.86  9.48   -26.4        10.7 
14 Subject   Intercept 352    36.4   9.48    17.8        55.0 
15 Subject   Intercept 369     7.04  9.48   -11.5        25.6 
16 Subject   Intercept 370    -6.36  9.48   -24.9        12.2 
17 Subject   Intercept 371    -3.29  9.48   -21.9        15.3 
18 Subject   Intercept 372    18.1   9.48    -0.456      36.7 

extract_random_effects(brm_1, ci_level = .8)
# A tibble: 36 x 7
   group_var effect    group   value    se lower_10 upper_90
   <chr>     <chr>     <chr>   <dbl> <dbl>    <dbl>    <dbl>
 1 Subject   Intercept 308     3.54   13.9  -14.1       21.2
 2 Subject   Intercept 309   -39.6    14.0  -57.6      -22.1
 3 Subject   Intercept 310   -38.0    14.4  -56.8      -19.9
 4 Subject   Intercept 330    23.2    14.0    5.63      41.3
 5 Subject   Intercept 331    21.7    13.9    3.94      39.7
 6 Subject   Intercept 332     9.29   13.1   -6.96      26.0
 7 Subject   Intercept 333    16.7    13.2    0.208     34.0
 8 Subject   Intercept 334    -6.70   13.9  -24.7       10.6
 9 Subject   Intercept 335    -0.682  14.1  -19.0       17.1
10 Subject   Intercept 337    35.2    14.1   17.2       53.5
# … with 26 more rows
```

### Extract Fixed Effects

``` r
extract_fixed_effects(brm_1)
# A tibble: 2 x 5
  term      value    se lower_2.5 upper_97.5
  <chr>     <dbl> <dbl>     <dbl>      <dbl>
1 Intercept 251.   7.06    237.        266. 
2 Days       10.4  1.73      6.99       13.7


extract_fixed_effects(nlme_1)
# A tibble: 3 x 7
  term   value    se     t p_value lower_2.5 upper_97.5
  <chr>  <dbl> <dbl> <dbl>   <dbl>     <dbl>      <dbl>
1 Asym  101.   2.46   41.2       0     96.6      106.  
2 R0     -8.63 0.318 -27.1       0     -9.25      -8.00
3 lrc    -3.23 0.034 -94.4       0     -3.30      -3.17
```

### Extract Random Coefficients

Extract the random coefficients with their standard errors (if
available).

``` r
extract_random_coefs(lmer_1)
# A tibble: 18 x 7
   group_var effect    group  coef    se lower_2.5 upper_97.5
   <chr>     <chr>     <fct> <dbl> <dbl>     <dbl>      <dbl>
 1 Subject   Intercept 308    292.  19.2      255.       330.
 2 Subject   Intercept 309    174.  19.2      136.       211.
 3 Subject   Intercept 310    188.  19.2      151.       226.
 4 Subject   Intercept 330    256.  19.2      218.       293.
 5 Subject   Intercept 331    262.  19.2      224.       299.
 6 Subject   Intercept 332    260.  19.2      222.       297.
 7 Subject   Intercept 333    268.  19.2      230.       306.
 8 Subject   Intercept 334    248.  19.2      211.       286.
 9 Subject   Intercept 335    206.  19.2      168.       244.
10 Subject   Intercept 337    324.  19.2      286.       361.
11 Subject   Intercept 349    230.  19.2      193.       268.
12 Subject   Intercept 350    266.  19.2      228.       303.
13 Subject   Intercept 351    244.  19.2      206.       281.
14 Subject   Intercept 352    288.  19.2      250.       325.
15 Subject   Intercept 369    258.  19.2      221.       296.
16 Subject   Intercept 370    245.  19.2      207.       283.
17 Subject   Intercept 371    248.  19.2      210.       286.
18 Subject   Intercept 372    270.  19.2      232.       307.


extract_random_coefs(tmb_2)
# A tibble: 36 x 7
   group_var effect    group  coef    se lower_2.5 upper_97.5
   <chr>     <chr>     <fct> <dbl> <dbl>     <dbl>      <dbl>
 1 Subject   Intercept 308    254.  20.3      214.       294.
 2 Subject   Intercept 309    211.  20.5      171.       251.
 3 Subject   Intercept 310    213.  20.4      173.       253.
 4 Subject   Intercept 330    274.  20.6      234.       315.
 5 Subject   Intercept 331    273.  20.2      233.       313.
 6 Subject   Intercept 332    260.  19.5      222.       299.
 7 Subject   Intercept 333    268.  19.7      229.       307.
 8 Subject   Intercept 334    244.  19.5      206.       283.
 9 Subject   Intercept 335    250.  20.7      210.       291.
10 Subject   Intercept 337    286.  20.3      246.       326.
# … with 26 more rows
```

### Extract Variance Components

``` r
extract_vc(lmer_2)
Computing profile confidence intervals ...
     group    effect variance     sd sd_2.5 sd_97.5 var_prop
1  Subject Intercept  611.898 24.737 14.382  37.714    0.470
2  Subject      Days   35.081  5.923 -0.481   0.685    0.027
3 Residual            654.941 25.592 22.898  28.858    0.503


extract_vc(lmer_2, ci_scale = 'var', show_cor = TRUE, digits = 2)
Computing profile confidence intervals ...
$`Variance Components`
     group    effect variance    sd var_2.5 var_97.5 var_prop
1  Subject Intercept   611.90 24.74  206.84  1422.33     0.47
2  Subject      Days    35.08  5.92    0.23     0.47     0.03
3 Residual             654.94 25.59  524.33   832.78     0.50

$Cor
$Cor$Subject
          Intercept Days
Intercept      1.00 0.07
Days           0.07 1.00


extract_vc(nlme_1)
     group effect variance    sd sd_2.5 sd_97.5 var_prop
1     Seed   Asym   13.327 3.651  2.479   5.375    0.963
2 Residual           0.517 0.719  0.609   0.849    0.037


extract_vc(brm_1)
     group    effect variance     sd sd_2.5 sd_97.5 var_prop
1  Subject Intercept  711.310 26.670 15.428  42.031    0.499
2  Subject      Days   42.656  6.531  4.088  10.012    0.030
3 Residual            672.297 25.929 23.075  29.175    0.471
```

### Extract Heterogeneous Variances

Extract heterogeneous variances from nlme (and eventually others), which
only reports the relative standard deviation values by default.

``` r
library(nlme)

model <- lme(
  distance ~ age + Sex, 
  data = Orthodont, 
  random = ~ 1|Subject,
  weights = varIdent(form = ~ 1 | Sex)
)

summary(model)
Linear mixed-effects model fit by REML
 Data: Orthodont 
       AIC      BIC    logLik
  432.3567 448.2805 -210.1784

Random effects:
 Formula: ~1 | Subject
        (Intercept) Residual
StdDev:    1.839463 1.761758

Variance function:
 Structure: Different standard deviations per stratum
 Formula: ~1 | Sex 
 Parameter estimates:
     Male    Female 
1.0000000 0.4541324 
Fixed effects: distance ~ age + Sex 
                Value Std.Error DF   t-value p-value
(Intercept) 18.919992 0.7285568 80 25.969138  0.0000
age          0.549887 0.0473096 80 11.623168  0.0000
SexFemale   -2.321023 0.7629703 25 -3.042088  0.0055
 Correlation: 
          (Intr) age   
age       -0.714       
SexFemale -0.468  0.000

Standardized Within-Group Residuals:
        Min          Q1         Med          Q3         Max 
-3.25494115 -0.53013324 -0.02554914  0.51114273  3.03915818 

Number of Observations: 108
Number of Groups: 27 

extract_het_var(model)
     Male    Female
1 3.10379 0.6401141
```

### Find typical values

``` r
find_typical(lmer_1)
# A tibble: 1 x 7
  group_var effect    group value    se lower_2.5 upper_97.5
  <chr>     <chr>     <fct> <dbl> <dbl>     <dbl>      <dbl>
1 Subject   Intercept 334   -3.00  9.48     -21.6       15.6

find_typical(tmb_2, probs = c(.25, .5, .75))
# A tibble: 6 x 8
  group_var effect    group   value    se lower_2.5 upper_97.5 probs
  <chr>     <chr>     <fct>   <dbl> <dbl>     <dbl>      <dbl> <chr>
1 Subject   Days      351    -2.96   2.62     -8.08       2.17 25%  
2 Subject   Days      333    -0.159  2.62     -5.30       4.99 50%  
3 Subject   Days      352     3.56   2.62     -1.58       8.70 75%  
4 Subject   Intercept 350   -12.3   13.7     -39.3       14.6  25%  
5 Subject   Intercept 308     2.82  13.7     -23.9       29.6  50%  
6 Subject   Intercept 333    16.4   13.1      -9.23      42.1  75%  
```

## Other stuff

### Code of Conduct

Please note that the ‘mixedup’ project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md).

By contributing to this project, you agree to abide by its terms.
