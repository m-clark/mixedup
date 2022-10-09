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

## ----loadbayes, echo=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
brms_model <- mixedup:::brms_model
rstanarm_model <- mixedup:::rstanarm_model

## ----mods-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)

library(lme4)
library(glmmTMB)
library(nlme)
library(brms)
library(rstanarm)
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

# rstanarm_model <-
#   stan_glmer(Reaction ~ Days + (1 + Days | Subject), 
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

extract_random_effects(lmer_model)


extract_random_effects(lme_model)


extract_random_effects(tmb_model)

extract_random_effects(brms_model)


extract_random_effects(rstanarm_model)


extract_random_effects(mgcv_model)

## ----options--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
lmer_model <-
  lmer(y ~ service + (1 | d) + (1 | s), data = InstEval[1:5000,])

extract_random_effects(
  lmer_model,
  ci_level = .9,
  digits   = 2
) %>% 
  head()

## ----tmb_zip--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tmb_zip <- glmmTMB(
  count ~ spp + mined + (1 | site),
  zi =  ~ spp + mined + (1 | site),
  family = truncated_poisson,
  data   = Salamanders
)

extract_random_effects(
  tmb_zip,
  component = 'zi'
) %>% 
  head()

