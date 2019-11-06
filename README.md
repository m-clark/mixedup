
  - [mixedup](#mixedup)
      - [Installation](#installation)
      - [Feature list](#feature-list)
          - [Extract Variance Components](#extract-variance-components)
          - [Extract Random Effects](#extract-random-effects)
          - [Extract Random Coefficients](#extract-random-coefficients)
          - [Extract Heterogeneous
            Variances](#extract-heterogeneous-variances)
          - [Find Typical](#find-typical)
      - [Examples](#examples)
          - [Setup](#setup)
          - [Extract random effects](#extract-random-effects-1)
          - [Extract random
            coefficients](#extract-random-coefficients-1)
          - [Extract variance
            components](#extract-variance-components-1)
          - [Extract heterogeneous
            variances](#extract-heterogeneous-variances-1)
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

Provides extended functionality for mixed models. The goal of mixedup is
to solve little problems that slip through the cracks from the various
packages, broom, and others. Basically the idea is to create objects
that are easy to use and mostly ready for presentation.

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

##### Extract Random Coefficients

  - [x] lme4
  - [x] glmmTMB
  - [x] nlme

##### Extract Heterogeneous Variances

  - [ ] glmmTMB
  - [x] nlme

##### Find Typical

  - [ ] lme4
  - [ ] glmmTMB
  - [ ] nlme

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

brm_1 = brm(Reaction ~ Days + (1 + Days| Subject), data = sleepstudy, refresh = -1)
Compiling the C++ model
Start sampling
Chain 1: 
Chain 1: Gradient evaluation took 0 seconds
Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
Chain 1: Adjust your expectations accordingly!
Chain 1: 
Chain 1: 
Chain 1: 
Chain 1:  Elapsed Time: 1.973 seconds (Warm-up)
Chain 1:                0.691 seconds (Sampling)
Chain 1:                2.664 seconds (Total)
Chain 1: 
Chain 2: 
Chain 2: Gradient evaluation took 0 seconds
Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
Chain 2: Adjust your expectations accordingly!
Chain 2: 
Chain 2: 
Chain 2: 
Chain 2:  Elapsed Time: 1.878 seconds (Warm-up)
Chain 2:                0.861 seconds (Sampling)
Chain 2:                2.739 seconds (Total)
Chain 2: 
Chain 3: 
Chain 3: Gradient evaluation took 0 seconds
Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
Chain 3: Adjust your expectations accordingly!
Chain 3: 
Chain 3: 
Chain 3: 
Chain 3:  Elapsed Time: 1.882 seconds (Warm-up)
Chain 3:                1.003 seconds (Sampling)
Chain 3:                2.885 seconds (Total)
Chain 3: 
Chain 4: 
Chain 4: Gradient evaluation took 0 seconds
Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
Chain 4: Adjust your expectations accordingly!
Chain 4: 
Chain 4: 
Chain 4: 
Chain 4:  Elapsed Time: 2.127 seconds (Warm-up)
Chain 4:                0.736 seconds (Sampling)
Chain 4:                2.863 seconds (Total)
Chain 4: 
```

### Extract random effects

Extract the random effects with their (not necessarily valid) standard
errors from model objects.

``` r
library(mixedup)

extract_random_effects(lmer_1, re = 'Subject')
   group Intercept se_Intercept
1    308    40.784       89.788
2    309   -77.850       89.788
3    310   -63.109       89.788
4    330     4.406       89.788
5    331    10.216       89.788
6    332     8.221       89.788
7    333    16.500       89.788
8    334    -2.997       89.788
9    335   -45.282       89.788
10   337    72.183       89.788
11   349   -21.196       89.788
12   350    14.111       89.788
13   351    -7.862       89.788
14   352    36.378       89.788
15   369     7.036       89.788
16   370    -6.363       89.788
17   371    -3.294       89.788
18   372    18.116       89.788


extract_random_effects(lmer_2, re = 'Subject')
   group Intercept    Days se_Intercept se_Days
1    308     2.258   9.199      145.694   5.312
2    309   -40.394  -8.621      145.694   5.312
3    310   -38.956  -5.450      145.694   5.312
4    330    23.689  -4.814      145.694   5.312
5    331    22.259  -3.070      145.694   5.312
6    332     9.039  -0.272      145.694   5.312
7    333    16.839  -0.223      145.694   5.312
8    334    -7.232   1.075      145.694   5.312
9    335    -0.333 -10.752      145.694   5.312
10   337    34.887   8.629      145.694   5.312
11   349   -25.208   1.173      145.694   5.312
12   350   -13.069   6.614      145.694   5.312
13   351     4.578  -3.015      145.694   5.312
14   352    20.861   3.536      145.694   5.312
15   369     3.275   0.872      145.694   5.312
16   370   -25.611   4.822      145.694   5.312
17   371     0.807  -0.988      145.694   5.312
18   372    12.313   1.284      145.694   5.312
```

### Extract random coefficients

Extract the random coefficients with their standard errors model
objects.

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
     group coefficient variance    sd sd_lower sd_upper var_prop
1     Seed        Asym   13.327 3.651    2.479    5.375    0.963
2 Residual                0.517 0.719    0.609    0.849    0.037


extract_vc(brm_1)
     group coefficient variance     sd sd_2.5 sd_97.5 var_prop
1  Subject   Intercept  725.797 26.941 15.815  42.461    0.504
2  Subject        Days   42.382  6.510  4.167   9.880    0.029
3 Residual              672.173 25.926 23.055  29.275    0.467
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

extract_nlme_variances(model)
     Male    Female 
3.1037904 0.6401141 
```

## Other stuff

### Code of Conduct

Please note that the ‘mixedup’ project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md).

By contributing to this project, you agree to abide by its terms.
