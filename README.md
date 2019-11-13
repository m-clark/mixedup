
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

Provides extended functionality for mixed models. The goal of mixedup is
to solve little problems that slip through the cracks from the various
packages, broom, and others. Basically the idea is to create (tidy)
objects that are easy to use and mostly ready for presentation, and
consistent across packages.

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

extract_random_effects(lmer_1, re = 'Subject')
   group_var    effect group   value    sd   lower   upper
1    Subject Intercept   308  40.784 9.476  22.211  59.356
2    Subject Intercept   309 -77.850 9.476 -96.422 -59.277
3    Subject Intercept   310 -63.109 9.476 -81.681 -44.536
4    Subject Intercept   330   4.406 9.476 -14.166  22.979
5    Subject Intercept   331  10.216 9.476  -8.356  28.788
6    Subject Intercept   332   8.221 9.476 -10.351  26.794
7    Subject Intercept   333  16.500 9.476  -2.072  35.073
8    Subject Intercept   334  -2.997 9.476 -21.569  15.575
9    Subject Intercept   335 -45.282 9.476 -63.854 -26.710
10   Subject Intercept   337  72.183 9.476  53.610  90.755
11   Subject Intercept   349 -21.196 9.476 -39.769  -2.624
12   Subject Intercept   350  14.111 9.476  -4.461  32.684
13   Subject Intercept   351  -7.862 9.476 -26.435  10.710
14   Subject Intercept   352  36.378 9.476  17.806  54.951
15   Subject Intercept   369   7.036 9.476 -11.536  25.609
16   Subject Intercept   370  -6.363 9.476 -24.935  12.210
17   Subject Intercept   371  -3.294 9.476 -21.867  15.278
18   Subject Intercept   372  18.116 9.476  -0.457  36.688


extract_random_effects(tmb_2, re = 'Subject')
   group_var    effect group   value     sd   lower   upper
1    Subject Intercept   308   2.816 13.654 -23.946  29.577
2    Subject Intercept   309 -40.048 13.829 -67.153 -12.944
3    Subject Intercept   310 -38.433 13.733 -65.351 -11.516
4    Subject Intercept   330  22.832 13.949  -4.507  50.172
5    Subject Intercept   331  21.550 13.601  -5.108  48.208
6    Subject Intercept   332   8.816 12.911 -16.490  34.122
7    Subject Intercept   333  16.442 13.096  -9.226  42.110
8    Subject Intercept   334  -6.997 12.915 -32.311  18.317
9    Subject Intercept   335  -1.037 14.020 -28.516  26.441
10   Subject Intercept   337  34.666 13.637   7.937  61.396
11   Subject Intercept   349 -24.558 13.507 -51.031   1.915
12   Subject Intercept   350 -12.335 13.737 -39.259  14.590
13   Subject Intercept   351   4.274 12.994 -21.195  29.743
14   Subject Intercept   352  20.622 13.091  -5.037  46.281
15   Subject Intercept   369   3.259 12.836 -21.899  28.416
16   Subject Intercept   370 -24.710 14.059 -52.266   2.845
17   Subject Intercept   371   0.723 12.842 -24.447  25.893
18   Subject Intercept   372  12.119 12.926 -13.216  37.454
19   Subject      Days   308   9.076  2.741   3.702  14.449
20   Subject      Days   309  -8.644  2.735 -14.005  -3.283
21   Subject      Days   310  -5.513  2.713 -10.831  -0.196
22   Subject      Days   330  -4.659  2.769 -10.087   0.769
23   Subject      Days   331  -2.945  2.709  -8.253   2.364
24   Subject      Days   332  -0.235  2.598  -5.328   4.857
25   Subject      Days   333  -0.159  2.625  -5.304   4.987
26   Subject      Days   334   1.033  2.600  -4.063   6.128
27   Subject      Days   335 -10.599  2.809 -16.106  -5.093
28   Subject      Days   337   8.632  2.711   3.319  13.946
29   Subject      Days   349   1.064  2.688  -4.204   6.333
30   Subject      Days   350   6.472  2.745   1.092  11.852
31   Subject      Days   351  -2.955  2.615  -8.081   2.171
32   Subject      Days   352   3.562  2.623  -1.580   8.703
33   Subject      Days   369   0.872  2.587  -4.199   5.942
34   Subject      Days   370   4.660  2.786  -0.802  10.121
35   Subject      Days   371  -0.971  2.588  -6.044   4.102
36   Subject      Days   372   1.311  2.599  -3.784   6.406


extract_random_effects(brm_1, re = 'Subject')
      effect group_var group   value     se   lower   upper
1  Intercept   Subject   308   2.994 14.452 -25.673  30.430
2  Intercept   Subject   309 -39.986 14.336 -68.384 -12.060
3  Intercept   Subject   310 -38.310 14.397 -68.408 -11.539
4  Intercept   Subject   330  23.121 14.556  -4.077  51.483
5  Intercept   Subject   331  21.819 14.390  -4.613  50.929
6  Intercept   Subject   332   9.222 13.544 -17.685  36.956
7  Intercept   Subject   333  16.454 13.716 -10.112  45.365
8  Intercept   Subject   334  -6.951 13.680 -33.715  19.416
9  Intercept   Subject   335  -0.576 14.664 -28.763  28.527
10 Intercept   Subject   337  34.881 14.329   7.672  63.840
11 Intercept   Subject   349 -24.532 13.894 -52.896   1.642
12 Intercept   Subject   350 -12.272 14.062 -40.760  13.713
13 Intercept   Subject   351   4.526 13.941 -22.523  32.782
14 Intercept   Subject   352  20.505 13.720  -5.698  48.567
15 Intercept   Subject   369   3.430 13.498 -23.516  30.238
16 Intercept   Subject   370 -25.013 14.261 -53.459   2.570
17 Intercept   Subject   371   0.835 13.508 -25.664  27.600
18 Intercept   Subject   372  12.485 13.734 -14.106  40.298
19      Days   Subject   308   9.123  2.898   3.475  14.848
20      Days   Subject   309  -8.750  2.815 -14.387  -3.256
21      Days   Subject   310  -5.543  2.853 -11.224  -0.007
22      Days   Subject   330  -4.715  2.935 -10.697   0.774
23      Days   Subject   331  -2.917  2.898  -8.996   2.474
24      Days   Subject   332  -0.284  2.728  -5.734   4.991
25      Days   Subject   333  -0.136  2.802  -5.773   5.264
26      Days   Subject   334   1.038  2.746  -4.343   6.382
27      Days   Subject   335 -10.702  2.953 -16.929  -5.118
28      Days   Subject   337   8.649  2.843   2.942  14.124
29      Days   Subject   349   1.060  2.792  -4.390   6.615
30      Days   Subject   350   6.504  2.841   1.093  12.123
31      Days   Subject   351  -3.007  2.752  -8.582   2.209
32      Days   Subject   352   3.555  2.739  -1.935   8.924
33      Days   Subject   369   0.845  2.707  -4.539   6.056
34      Days   Subject   370   4.686  2.866  -0.792  10.389
35      Days   Subject   371  -0.974  2.743  -6.454   4.527
36      Days   Subject   372   1.272  2.784  -4.293   6.727
```

### Extract Fixed Effects

``` r
extract_fixed_effects(brm_1, re = 'Subject')
# A tibble: 2 x 5
  term      value    se lower_2.5 upper_97.5
  <chr>     <dbl> <dbl>     <dbl>      <dbl>
1 Intercept 251.   7.43    236.        266. 
2 Days       10.5  1.69      7.24       14.0


extract_fixed_effects(nlme_1,  re = 'Subject')
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
extract_random_coefs(lmer_1, re = 'Subject')
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


extract_random_coefs(tmb_2,  re = 'Subject')
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
1  Subject   Intercept  611.898 24.737 14.382  37.714    0.470
2  Subject        Days   35.081  5.923 -0.481   0.685    0.027
3 Residual              654.941 25.592 22.898  28.858    0.503


extract_vc(lmer_2, ci_scale = 'var', show_cor = TRUE, digits = 2)
Computing profile confidence intervals ...
$`Variance Components`
     group coefficient variance    sd var_2.5 var_97.5 var_prop
1  Subject   Intercept   611.90 24.74  206.84  1422.33     0.47
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
1  Subject   Intercept  732.832 27.071 15.729  42.237    0.506
2  Subject        Days   42.586  6.526  4.169   9.995    0.029
3 Residual              671.906 25.921 23.159  29.151    0.464
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
  group_var effect    group value    sd lower upper
  <chr>     <chr>     <fct> <dbl> <dbl> <dbl> <dbl>
1 Subject   Intercept 334   -3.00  9.48 -21.6  15.6

find_typical(tmb_2, probs = c(.25, .5, .75))
# A tibble: 6 x 8
  group_var effect    group   value    sd  lower upper probs
  <chr>     <chr>     <fct>   <dbl> <dbl>  <dbl> <dbl> <chr>
1 Subject   Days      351    -2.96   2.62  -8.08  2.17 25%  
2 Subject   Days      333    -0.159  2.62  -5.30  4.99 50%  
3 Subject   Days      352     3.56   2.62  -1.58  8.70 75%  
4 Subject   Intercept 350   -12.3   13.7  -39.3  14.6  25%  
5 Subject   Intercept 308     2.82  13.7  -23.9  29.6  50%  
6 Subject   Intercept 333    16.4   13.1   -9.23 42.1  75%  
```

## Other stuff

### Code of Conduct

Please note that the ‘mixedup’ project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md).

By contributing to this project, you agree to abide by its terms.
