
  - [mixedup](#mixedup)
      - [Installation](#installation)
      - [Feature list](#feature-list)
          - [Extract Variance Components](#extract-variance-components)
          - [Extract Random Effects](#extract-random-effects)
          - [Extract Fixed Effects](#extract-fixed-effects)
          - [Extract Random Coefficients](#extract-random-coefficients)
          - [Extract Heterogeneous
            Variances](#extract-heterogeneous-variances)
          - [Find Typical](#find-typical)
      - [Examples](#examples)
          - [Setup](#setup)
          - [Extract Random Effects](#extract-random-effects-1)
          - [Extract Fixed Effects](#extract-fixed-effects-1)
          - [Extract Random
            Coefficients](#extract-random-coefficients-1)
          - [Extract Variance
            Components](#extract-variance-components-1)
          - [Extract Heterogeneous
            Variances](#extract-heterogeneous-variances-1)
          - [Find typical values](#find-typical-values)
      - [Other stuff](#other-stuff)
          - [Code of Conduct](#code-of-conduct)

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
notable post processing to get some basic output even with broom\* tidy
tools, often which isn’t applicable if I switch to another package.
These functions attempt to fill my specific niche.

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

  - [x] lme4
  - [x] glmmTMB
  - [x] nlme
  - [x] brms

##### Extract Random Effects

  - [x] lme4
  - [x] glmmTMB
  - [x] nlme
  - [x] brms

##### Extract Fixed Effects

  - [x] lme4
  - [x] glmmTMB
  - [x] nlme
  - [x] brms

##### Extract Random Coefficients

  - [x] lme4
  - [x] glmmTMB
  - [x] nlme
  - [x] brms

##### Extract Heterogeneous Variances

  - [ ] glmmTMB
  - [x] nlme

##### Find Typical

  - [x] lme4
  - [x] glmmTMB
  - [x] nlme
  - [x] brms

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
   group_var    effect group   value    se lower_2.5 upper_97.5
1    Subject Intercept   308  40.784 9.476    22.212     59.356
2    Subject Intercept   309 -77.850 9.476   -96.422    -59.278
3    Subject Intercept   310 -63.109 9.476   -81.681    -44.537
4    Subject Intercept   330   4.406 9.476   -14.166     22.978
5    Subject Intercept   331  10.216 9.476    -8.356     28.788
6    Subject Intercept   332   8.221 9.476   -10.351     26.793
7    Subject Intercept   333  16.500 9.476    -2.071     35.072
8    Subject Intercept   334  -2.997 9.476   -21.569     15.575
9    Subject Intercept   335 -45.282 9.476   -63.854    -26.710
10   Subject Intercept   337  72.183 9.476    53.611     90.755
11   Subject Intercept   349 -21.196 9.476   -39.768     -2.624
12   Subject Intercept   350  14.111 9.476    -4.461     32.683
13   Subject Intercept   351  -7.862 9.476   -26.434     10.710
14   Subject Intercept   352  36.378 9.476    17.806     54.950
15   Subject Intercept   369   7.036 9.476   -11.536     25.608
16   Subject Intercept   370  -6.363 9.476   -24.935     12.209
17   Subject Intercept   371  -3.294 9.476   -21.866     15.278
18   Subject Intercept   372  18.116 9.476    -0.456     36.688

extract_random_effects(brm_1, ci_level = .8)
      effect group_var group   value     se lower_10 upper_90
1  Intercept   Subject   308   2.791 14.030  -15.476   20.637
2  Intercept   Subject   309 -40.035 14.387  -58.663  -21.944
3  Intercept   Subject   310 -38.257 14.579  -56.638  -19.915
4  Intercept   Subject   330  23.177 14.634    4.910   41.803
5  Intercept   Subject   331  21.752 14.431    3.666   40.406
6  Intercept   Subject   332   9.112 13.467   -7.974   26.533
7  Intercept   Subject   333  16.896 14.045   -0.700   35.296
8  Intercept   Subject   334  -6.693 13.740  -23.867   10.444
9  Intercept   Subject   335  -0.609 14.431  -18.491   18.101
10 Intercept   Subject   337  34.708 14.562   16.407   53.096
11 Intercept   Subject   349 -24.672 14.163  -43.462   -6.943
12 Intercept   Subject   350 -12.100 14.074  -30.727    5.606
13 Intercept   Subject   351   4.683 13.454  -12.434   21.991
14 Intercept   Subject   352  20.897 13.614    3.780   38.361
15 Intercept   Subject   369   3.480 13.293  -13.402   20.108
16 Intercept   Subject   370 -24.581 14.502  -44.714   -6.399
17 Intercept   Subject   371   0.804 13.511  -16.569   18.059
18 Intercept   Subject   372  12.559 13.387   -4.332   30.182
19      Days   Subject   308   9.184  2.941    5.505   13.103
20      Days   Subject   309  -8.584  2.946  -12.302   -4.798
21      Days   Subject   310  -5.456  2.833   -8.972   -1.877
22      Days   Subject   330  -4.598  2.946   -8.354   -0.880
23      Days   Subject   331  -2.947  2.923   -6.724    0.772
24      Days   Subject   332  -0.214  2.745   -3.757    3.220
25      Days   Subject   333  -0.111  2.859   -3.761    3.449
26      Days   Subject   334   1.089  2.812   -2.456    4.763
27      Days   Subject   335 -10.626  2.927  -14.434   -6.938
28      Days   Subject   337   8.767  2.897    5.135   12.431
29      Days   Subject   349   1.162  2.843   -2.390    4.816
30      Days   Subject   350   6.541  2.884    2.917   10.258
31      Days   Subject   351  -2.969  2.766   -6.443    0.453
32      Days   Subject   352   3.611  2.726    0.179    7.043
33      Days   Subject   369   0.925  2.749   -2.563    4.441
34      Days   Subject   370   4.790  2.967    1.128    8.643
35      Days   Subject   371  -0.885  2.746   -4.370    2.567
36      Days   Subject   372   1.351  2.698   -2.034    4.900
```

### Extract Fixed Effects

``` r
extract_fixed_effects(brm_1)
# A tibble: 2 x 5
  term      value    se lower_2.5 upper_97.5
  <chr>     <dbl> <dbl>     <dbl>      <dbl>
1 Intercept 251.   7.60    236.        266. 
2 Days       10.4  1.74      6.79       13.7


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
   group_var    effect group    coef     se lower_2.5 upper_97.5
1    Subject Intercept   308 292.189 19.223   254.513    329.865
2    Subject Intercept   309 173.555 19.223   135.879    211.231
3    Subject Intercept   310 188.296 19.223   150.620    225.972
4    Subject Intercept   330 255.811 19.223   218.135    293.487
5    Subject Intercept   331 261.621 19.223   223.945    299.297
6    Subject Intercept   332 259.626 19.223   221.950    297.302
7    Subject Intercept   333 267.905 19.223   230.229    305.581
8    Subject Intercept   334 248.408 19.223   210.732    286.084
9    Subject Intercept   335 206.123 19.223   168.447    243.799
10   Subject Intercept   337 323.588 19.223   285.912    361.264
11   Subject Intercept   349 230.209 19.223   192.533    267.885
12   Subject Intercept   350 265.516 19.223   227.840    303.192
13   Subject Intercept   351 243.543 19.223   205.867    281.219
14   Subject Intercept   352 287.783 19.223   250.107    325.459
15   Subject Intercept   369 258.441 19.223   220.765    296.117
16   Subject Intercept   370 245.042 19.223   207.366    282.718
17   Subject Intercept   371 248.111 19.223   210.435    285.787
18   Subject Intercept   372 269.521 19.223   231.845    307.197


extract_random_coefs(tmb_2)
   group_var    effect group    coef     se lower_2.5 upper_97.5
1    Subject Intercept   308 254.221 20.286   214.461    293.981
2    Subject Intercept   309 211.357 20.461   171.254    251.460
3    Subject Intercept   310 212.972 20.365   173.057    252.887
4    Subject Intercept   330 274.237 20.581   233.899    314.575
5    Subject Intercept   331 272.955 20.233   233.299    312.611
6    Subject Intercept   332 260.221 19.543   221.917    298.525
7    Subject Intercept   333 267.847 19.728   229.181    306.513
8    Subject Intercept   334 244.408 19.547   206.097    282.719
9    Subject Intercept   335 250.368 20.652   209.891    290.845
10   Subject Intercept   337 286.071 20.269   246.344    325.798
11   Subject Intercept   349 226.847 20.139   187.375    266.319
12   Subject Intercept   350 239.070 20.369   199.147    278.993
13   Subject Intercept   351 255.679 19.626   217.213    294.145
14   Subject Intercept   352 272.027 19.723   233.371    310.683
15   Subject Intercept   369 254.664 19.468   216.507    292.821
16   Subject Intercept   370 226.695 20.691   186.141    267.249
17   Subject Intercept   371 252.128 19.474   213.960    290.296
18   Subject Intercept   372 263.524 19.558   225.191    301.857
19   Subject      Days   308  19.543  4.243    11.227     27.859
20   Subject      Days   309   1.823  4.237    -6.481     10.127
21   Subject      Days   310   4.954  4.215    -3.307     13.215
22   Subject      Days   330   5.808  4.271    -2.563     14.179
23   Subject      Days   331   7.522  4.211    -0.731     15.775
24   Subject      Days   332  10.232  4.100     2.196     18.268
25   Subject      Days   333  10.308  4.127     2.219     18.397
26   Subject      Days   334  11.500  4.102     3.460     19.540
27   Subject      Days   335  -0.132  4.311    -8.581      8.317
28   Subject      Days   337  19.099  4.213    10.842     27.356
29   Subject      Days   349  11.531  4.190     3.319     19.743
30   Subject      Days   350  16.939  4.247     8.615     25.263
31   Subject      Days   351   7.512  4.117    -0.557     15.581
32   Subject      Days   352  14.029  4.125     5.944     22.114
33   Subject      Days   369  11.339  4.089     3.325     19.353
34   Subject      Days   370  15.127  4.288     6.723     23.531
35   Subject      Days   371   9.496  4.090     1.480     17.512
36   Subject      Days   372  11.778  4.101     3.740     19.816
```

### Extract Variance Components

``` r
extract_vc(lmer_2)
Computing profile confidence intervals ...
     group coefficient variance     sd sd_2.5 sd_97.5 var_prop
1  Subject   Intercept  611.898 24.737 14.382  37.716    0.470
2  Subject        Days   35.081  5.923 -0.482   0.685    0.027
3 Residual              654.941 25.592 22.898  28.858    0.503


extract_vc(lmer_2, ci_scale = 'var', show_cor = TRUE, digits = 2)
Computing profile confidence intervals ...
$`Variance Components`
     group coefficient variance    sd var_2.5 var_97.5 var_prop
1  Subject   Intercept   611.90 24.74  206.83  1422.50     0.47
2  Subject        Days    35.08  5.92    0.23     0.47     0.03
3 Residual               654.94 25.59  524.33   832.78     0.50

$Cor
$Cor$Subject
          Intercept Days
Intercept      1.00 0.07
Days           0.07 1.00


extract_vc(nlme_1)
     group coefficient variance    sd sd_2.5 sd_97.5 var_prop
1     Seed        Asym   13.327 3.651  2.479   5.375    0.963
2 Residual                0.517 0.719  0.609   0.849    0.037


extract_vc(brm_1)
     group coefficient variance     sd sd_2.5 sd_97.5 var_prop
1  Subject   Intercept  721.821 26.867 15.640  42.015    0.502
2  Subject        Days   42.974  6.555  4.140  10.049    0.030
3 Residual              673.624 25.954 23.081  29.269    0.468
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
