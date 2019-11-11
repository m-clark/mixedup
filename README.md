
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

##### Extract Random Coefficients

  - \[X\] lme4
  - \[X\] glmmTMB
  - \[X\] nlme

##### Extract Heterogeneous Variances

  - \[ \] glmmTMB
  - \[X\] nlme

##### Find Typical

  - \[ \] lme4
  - \[ \] glmmTMB
  - \[ \] nlme

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

### Extract random effects

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
   group Intercept se_Intercept q_2.5_Intercept q_97.5_Intercept    Days
1    308     2.942       14.174         -25.308           31.186   9.154
2    309   -39.672       14.633         -68.648          -11.597  -8.724
3    310   -38.388       14.513         -67.818          -10.363  -5.493
4    330    23.526       14.253          -3.316           52.913  -4.737
5    331    22.132       14.338          -4.495           51.447  -2.971
6    332     9.074       13.350         -17.324           35.351  -0.260
7    333    16.509       13.794          -9.753           44.217  -0.116
8    334    -6.999       13.712         -36.293           19.392   1.072
9    335    -0.748       14.568         -29.083           28.453 -10.642
10   337    34.734       14.119           7.258           63.325   8.722
11   349   -24.188       14.008         -52.171            2.658   1.070
12   350   -12.284       14.376         -42.039           14.511   6.514
13   351     4.507       13.681         -21.656           32.234  -2.983
14   352    20.881       13.644          -5.690           47.834   3.638
15   369     3.248       13.658         -23.613           30.698   0.909
16   370   -24.614       14.392         -53.750            2.891   4.717
17   371     0.761       13.193         -24.813           26.749  -0.928
18   372    12.228       13.504         -14.323           40.135   1.308
   se_Days q_2.5_Days q_97.5_Days
1    2.885      3.530      15.055
2    2.920    -14.527      -3.065
3    2.872    -11.303       0.064
4    2.916    -10.536       0.885
5    2.925     -8.943       2.636
6    2.729     -5.588       5.152
7    2.794     -5.597       5.327
8    2.724     -4.275       6.443
9    2.943    -16.626      -5.246
10   2.843      3.201      14.402
11   2.860     -4.481       6.856
12   2.897      1.112      12.462
13   2.781     -8.456       2.325
14   2.816     -1.800       9.146
15   2.736     -4.683       6.262
16   2.866     -0.717      10.229
17   2.754     -6.343       4.432
18   2.728     -4.018       6.668
```

### Extract random coefficients

Extract the random coefficients with their standard errors (if
available).

``` r
extract_random_coef(lmer_1, re = 'Subject')
   group Intercept se_Intercept
1    308   292.189       13.594
2    309   173.556       13.594
3    310   188.297       13.594
4    330   255.812       13.594
5    331   261.621       13.594
6    332   259.626       13.594
7    333   267.906       13.594
8    334   248.408       13.594
9    335   206.123       13.594
10   337   323.588       13.594
11   349   230.209       13.594
12   350   265.516       13.594
13   351   243.543       13.594
14   352   287.784       13.594
15   369   258.441       13.594
16   370   245.042       13.594
17   371   248.111       13.594
18   372   269.521       13.594


extract_random_coef(tmb_2,  re = 'Subject')
   group Intercept   Days se_Intercept se_Days
1    308   254.221 19.543       15.179   3.126
2    309   211.357  1.823       15.337   3.120
3    310   212.972  4.954       15.251   3.101
4    330   274.237  5.809       15.445   3.151
5    331   272.955  7.523       15.132   3.097
6    332   260.221 10.232       14.515   3.001
7    333   267.847 10.308       14.680   3.025
8    334   244.408 11.500       14.519   3.003
9    335   250.368 -0.132       15.509   3.186
10   337   286.071 19.100       15.165   3.099
11   349   226.847 11.532       15.047   3.079
12   350   239.070 16.939       15.254   3.129
13   351   255.679  7.512       14.589   3.016
14   352   272.027 14.029       14.675   3.023
15   369   254.664 11.339       14.448   2.992
16   370   226.695 15.127       15.545   3.166
17   371   252.128  9.496       14.453   2.993
18   372   263.524 11.778       14.528   3.002
```

### Extract variance components

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
1  Subject   Intercept  731.400 27.044 15.676  43.123    0.505
2  Subject        Days   43.354  6.584  4.169  10.080    0.030
3 Residual              672.758 25.938 23.124  29.158    0.465
```

### Extract heterogeneous variances

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

find_typical(tmb_2)
# A tibble: 2 x 7
  group_var effect    group  value    sd  lower upper
  <chr>     <chr>     <fct>  <dbl> <dbl>  <dbl> <dbl>
1 Subject   Days      333   -0.159  2.62  -5.30  4.99
2 Subject   Intercept 371    0.723 12.8  -24.4  25.9 
```

## Other stuff

### Code of Conduct

Please note that the ‘mixedup’ project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md).

By contributing to this project, you agree to abide by its terms.
