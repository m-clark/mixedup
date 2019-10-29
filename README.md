
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

## Example

Extract the random coefficients with their standard errors from `lme4`
or `glmmTMB` objects.

``` r
library(lme4)
Loading required package: Matrix

lmer_1 <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
lmer_2 <- lmer(Reaction ~ Days + (1 + Days| Subject), data = sleepstudy)

library(glmmTMB)

tmb_1 <- glmmTMB(Reaction ~ Days + (1 | Subject), data = sleepstudy)
tmb_2 <- glmmTMB(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
```

``` r
library(mixedup)
extract_random_coef(lmer_1, re = 'Subject')
   group Intercept se_Intercept
1    308  292.1888     13.59363
2    309  173.5556     13.59363
3    310  188.2965     13.59363
4    330  255.8115     13.59363
5    331  261.6213     13.59363
6    332  259.6263     13.59363
7    333  267.9056     13.59363
8    334  248.4081     13.59363
9    335  206.1230     13.59363
10   337  323.5878     13.59363
11   349  230.2089     13.59363
12   350  265.5165     13.59363
13   351  243.5429     13.59363
14   352  287.7835     13.59363
15   369  258.4415     13.59363
16   370  245.0424     13.59363
17   371  248.1108     13.59363
18   372  269.5209     13.59363


extract_random_coef(lmer_2, re = 'Subject')
   group Intercept       Days se_Intercept  se_Days
1    308  253.6626 19.6665597     13.86572 2.775269
2    309  211.0108  1.8467699     13.86572 2.775269
3    310  212.4488  5.0177063     13.86572 2.775269
4    330  275.0940  5.6531411     13.86572 2.775269
5    331  273.6636  7.3976093     13.86572 2.775269
6    332  260.4439 10.1952325     13.86572 2.775269
7    333  268.2441 10.2438881     13.86572 2.775269
8    334  244.1731 11.5417935     13.86572 2.775269
9    335  251.0724 -0.2851939     13.86572 2.775269
10   337  286.2916 19.0963068     13.86572 2.775269
11   349  226.1971 11.6403856     13.86572 2.775269
12   350  238.3357 17.0815045     13.86572 2.775269
13   351  255.9828  7.4520035     13.86572 2.775269
14   352  272.2666 14.0036922     13.86572 2.775269
15   369  254.6802 11.3395736     13.86572 2.775269
16   370  225.7940 15.2895377     13.86572 2.775269
17   371  252.2122  9.4791130     13.86572 2.775269
18   372  263.7185 11.7515240     13.86572 2.775269


extract_random_coef(tmb_1,  re = 'Subject')
   group Intercept se_Intercept
1    308  292.0402     15.73179
2    309  173.8392     15.82431
3    310  188.5265     15.78066
4    330  255.7955     15.69706
5    331  261.5840     15.69886
6    332  259.5964     15.69808
7    333  267.8455     15.70241
8    334  248.4190     15.69684
9    335  206.2880     15.73996
10   337  323.3247     15.80646
11   349  230.2861     15.70615
12   350  265.4650     15.70086
13   351  243.5715     15.69796
14   352  287.6509     15.72462
15   369  258.4158     15.69770
16   370  245.0656     15.69751
17   371  248.1228     15.69688
18   372  269.4548     15.70359


extract_random_coef(tmb_2,  re = 'Subject')
   group Intercept       Days se_Intercept  se_Days
1    308  254.2208 19.5428220     15.17932 3.126006
2    309  211.3566  1.8232199     15.33703 3.120443
3    310  212.9719  4.9539090     15.25110 3.101256
4    330  274.2374  5.8085333     15.44505 3.150622
5    331  272.9551  7.5227641     15.13186 3.097337
6    332  260.2207 10.2320764     14.51503 3.001257
7    333  267.8471 10.3084613     14.67960 3.024610
8    334  244.4084 11.5000233     14.51872 3.002611
9    335  250.3677 -0.1321596     15.50940 3.185673
10   337  286.0714 19.0996630     15.16467 3.099331
11   349  226.8469 11.5316893     15.04704 3.079282
12   350  239.0705 16.9389922     15.25422 3.129111
13   351  255.6792  7.5119417     14.58899 3.016100
14   352  272.0273 14.0289898     14.67544 3.022977
15   369  254.6636 11.3389965     14.44783 2.991643
16   370  226.6948 15.1270247     15.54490 3.165635
17   371  252.1284  9.4962301     14.45344 2.992800
18   372  263.5240 11.7779766     14.52830 3.002347
```

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
