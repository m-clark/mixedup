
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
packages, broom, and others.

## Installation

You can install mixedup from GitHub with `devtools`:

``` r
devtools::install_github('m-clark/mixedup')
```

## Feature list

##### Extract Variance Components

  - [x] lme4
  - [x] glmmTMB
  - [ ] nlme

##### Extract Random Effects

  - [x] lme4
  - [ ] glmmTMB
  - [ ] nlme

##### Extract Variance Components

  - [x] lme4
  - [x] glmmTMB
  - [ ] nlme

##### Extract Heterogeneous Variances

  - [ ] glmmTMB
  - [x] nlme

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
```

### Extract random effects

Extract the random effects with their standard errors from `lme4` or
`glmmTMB` objects.

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

Extract the random coefficients with their standard errors from `lme4`
or `glmmTMB` objects.

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


extract_random_coef(lmer_2, re = 'Subject')
   group Intercept   Days se_Intercept se_Days
1    308   253.663 19.667       13.866   2.775
2    309   211.011  1.847       13.866   2.775
3    310   212.449  5.018       13.866   2.775
4    330   275.094  5.653       13.866   2.775
5    331   273.664  7.398       13.866   2.775
6    332   260.444 10.195       13.866   2.775
7    333   268.244 10.244       13.866   2.775
8    334   244.173 11.542       13.866   2.775
9    335   251.072 -0.285       13.866   2.775
10   337   286.292 19.096       13.866   2.775
11   349   226.197 11.640       13.866   2.775
12   350   238.336 17.082       13.866   2.775
13   351   255.983  7.452       13.866   2.775
14   352   272.267 14.004       13.866   2.775
15   369   254.680 11.340       13.866   2.775
16   370   225.794 15.290       13.866   2.775
17   371   252.212  9.479       13.866   2.775
18   372   263.718 11.752       13.866   2.775


extract_random_coef(tmb_1,  re = 'Subject')
   group Intercept se_Intercept
1    308   292.040       15.732
2    309   173.839       15.824
3    310   188.526       15.781
4    330   255.795       15.697
5    331   261.584       15.699
6    332   259.596       15.698
7    333   267.845       15.702
8    334   248.419       15.697
9    335   206.288       15.740
10   337   323.325       15.806
11   349   230.286       15.706
12   350   265.465       15.701
13   351   243.572       15.698
14   352   287.651       15.725
15   369   258.416       15.698
16   370   245.066       15.698
17   371   248.123       15.697
18   372   269.455       15.704


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
```

### Extract heterogeneous variances from nlme

Extract heterogeneous variances from nlme, which only reports the
relative standard deviation values by default.

``` r
library(nlme)

Attaching package: 'nlme'
The following object is masked from 'package:lme4':

    lmList

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
