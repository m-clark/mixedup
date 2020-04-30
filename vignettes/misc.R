## ----setup, include=FALSE, cache=FALSE----------------------------------------
knitr::opts_chunk$set(
  echo = T,
  message = F,
  warning = F,
  error = F,
  collapse = TRUE,
  comment = NA,
  R.options = list(width = 220),
  dev.args = list(bg = 'transparent'),
  dev = 'png',
  fig.align = 'center',
  out.width = '75%',
  fig.asp = .75,
  cache.rebuild = F,
  cache = F
)                                                    

## ----typical--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(lme4)

lmer_model <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)

library(mixedup)

find_typical(lmer_model)  # closest RE to zero


find_typical(lmer_model, probs = c(.1, .9))

## ----het-var--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(nlme)

lme_1 <- lme(
  distance ~ scale(age) + Sex,
  data = Orthodont,
  random = ~ 1 | Subject,
  weights = varIdent(form = ~ 1 | Sex)
)

extract_het_var(lme_1)

## ----cor-struct-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
base_model <-
  lme(Reaction ~ Days,
      random = ~ 1 + Days | Subject,
      data = sleepstudy)

lme_corSymm <-
  update(
    base_model,
    corr = corSymm(form = ~ 1 | Subject),
    data = dplyr::filter(sleepstudy, Days < 5)
  )

lme_corAR <-
  update(
    base_model,
    corr = corAR1(form = ~ Days),
    data = dplyr::filter(sleepstudy, Days < 5)
  )


extract_cor_structure(lme_corSymm)


extract_cor_structure(lme_corAR)

## ----model-data-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
extract_model_data(lmer_model)

