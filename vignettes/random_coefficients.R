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

## ----loadbrms, echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
brms_model <- mixedup:::brms_model

## ----mods-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(lme4)
library(glmmTMB)
library(nlme)
library(brms)
library(mgcv)


lmer_model <-
  lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)

lme_model  <-
  lme(Reaction ~ Days, random = ~ 1 + Days | Subject, data = sleepstudy)

tmb_model  <-
  glmmTMB(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)

# brms_model <-
#   brm(Reaction ~ Days + (1 + Days | Subject), 
#       data = sleepstudy,
#       cores = 4,
#       refresh = -1,
#       verbose = FALSE
#   )

# this is akin to (1 | Subject) + (0 + Days | Subject) in lme4
mgcv_model <-
  gam(
    Reaction ~  Days +
      s(Subject, bs = 're') +
      s(Days, Subject, bs = 're'),
    data = lme4::sleepstudy,
    method = 'REML'
  )

## ----results--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(mixedup)

extract_random_coefs(lmer_model)


extract_random_coefs(lme_model)


extract_random_coefs(tmb_model)


extract_random_coefs(brms_model)


extract_random_coefs(mgcv_model)

## ----options--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
extract_random_coefs(
  lmer_model,
  ci_level = .9,
  digits = 2
)

