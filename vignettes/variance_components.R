## ----setup, include=FALSE, cache=FALSE----------------------------------------
knitr::opts_chunk$set(
  echo      = TRUE,
  message   = FALSE,
  warning   = FALSE,
  error     = FALSE,
  collapse  = TRUE,
  comment   = NA,
  R.options = list(width = 220),
  dev.args  = list(bg = 'transparent'),
  dev       = 'png',
  fig.align = 'center',
  out.width = '75%',
  fig.asp   = .75,
  cache.rebuild = FALSE,
  cache         = FALSE
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

extract_vc(lmer_model)


extract_vc(lme_model)


extract_vc(tmb_model)


extract_vc(brms_model)


extract_vc(mgcv_model)

## ----options--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tmb_model <-
  glmmTMB(y ~ service + (1 | d) + (1 | s), data = InstEval[1:5000,])

extract_vc(
  tmb_model,
  ci_level = .9,
  ci_scale = 'var',
  digits = 2
)

