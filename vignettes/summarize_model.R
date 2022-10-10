## ----setup, include=FALSE, cache=FALSE----------------------------------------
knitr::opts_chunk$set(
  echo      = TRUE,
  message   = TRUE,
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

## ----mods, message=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
    data   = lme4::sleepstudy,
    method = 'REML'
  )

## ----results--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(mixedup)

summarize_model(lmer_model)


summarize_model(lme_model)


summarize_model(tmb_model)


summarize_model(brms_model)


summarize_model(mgcv_model)

## ----options--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summarize_model(
  lmer_model,
  ci     = FALSE,
  cor_re = TRUE,
  cor_fe = TRUE,
  digits = 3
)


summarise_model(lmer_model, ci = FALSE)

## ----convergence, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# for some reason this example apparently only works interactively, not when knit
ss2 = sleepstudy
ss2$Days = ss2$Days * 10

lmer_not_converged <- lmer(
  Reaction ~ Days + (Days|Subject),
  data = ss2
)

# lmer_not_converged

lmer_converged <- converge_it(lmer_not_converged) # final result is a converged model

lmer_converged

