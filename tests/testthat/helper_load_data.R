
# Run lme4 models ---------------------------------------------------------

library(lme4)

lmer_1 <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
lmer_2 <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
lmer_3 <- lmer(y ~ service + (1 | s) + (1 | d), data = InstEval[1:1000, ])

# unusually complex models just for testing
suppressWarnings({
  lmer_4 <- lmer(y ~ service + (1 + as.numeric(lectage)| s) + (1 + as.numeric(studage)| d) + (1|dept), data = InstEval[1:5000, ])
  lmer_5 <- lmer(y ~ service + (1 + as.numeric(lectage) + as.numeric(studage) + service| d) , data = InstEval[1:5000, ])
})

# Run TMB models ----------------------------------------------------------

library(glmmTMB)

tmb_1 <- glmmTMB(Reaction ~ Days + (1 | Subject), data = sleepstudy)
tmb_2 <- glmmTMB(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
tmb_3 <- glmmTMB(y ~ service + (1 | s) + (1 | d), data = InstEval[1:1000, ])

suppressWarnings({
  tmb_4 <- glmmTMB(y ~ service + (1 + as.numeric(lectage)| s) + (1 + as.numeric(studage)| d) + (1|dept),
                   data = InstEval[1:5000, ])
})

tmb_zip <- glmmTMB(
  count ~ spp + mined + (1 | site),
  zi =  ~ spp + mined + (1 | site),
  family = truncated_poisson,
  data = Salamanders
)

# Run nlme models ---------------------------------------------------------

library(nlme)

lme_1 <- lme(Reaction ~ Days, random = ~ 1 | Subject, data = sleepstudy)
lme_2 <- lme(Reaction ~ Days, random = ~ 1 + Days | Subject, data = sleepstudy)
lme_3 <- lme(y ~ service,
             random = list(d = ~ 1, s = ~ 1),
             data = droplevels(InstEval[1:1000, ]))
lme_4 <- lme(y ~ service,
             random = list(d = ~ 1 + service, s = ~ 1 ),
             data = droplevels(InstEval[1:3000, ]))

# won't converge
# lme_5 <- lme(y ~ service,
#              random = list(d = ~ 1 + as.numeric(lectage) + as.numeric(studage) + service) ,
#              data = droplevels(InstEval[1:5000, ]))

nlme_1 <-  nlme(height ~ SSasymp(age, Asym, R0, lrc),
                data = Loblolly,
                fixed = Asym + R0 + lrc ~ 1,
                random = Asym ~ 1,
                start = c(Asym = 103, R0 = -8.5, lrc = -3.3))

# Run brms models ---------------------------------------------------------


load('brm_results.RData')

# library(brms)
#
# bprior1 <- prior(student_t(5,0,10), class = b) +
#   prior(cauchy(0,2), class = sd)
#
# brm_glm <- brm(
#   count ~ zAge + zBase * Trt + (1 | patient),
#   data = epilepsy,
#   family = poisson(),
#   prior = bprior1,
#   cores = 4,
#   thin  = 40
# )
#
# pr = prior(normal(0, 10), class = b)
#
# brm_1 <-
#   brm(
#     Reaction ~ Days + (1 | Subject),
#     data = lme4::sleepstudy,
#     prior = pr,
#     cores = 4,
#     thin  = 40
#   )
#
# brm_2 <-
#   brm(
#     Reaction ~ Days + (1 + Days | Subject),
#     data = lme4::sleepstudy,
#     prior = pr,
#     cores = 4,
#     thin  = 40
#   )
#
#
#
# # more complex models just for testing (changed from others to minimize nparameters/object size)
# brm_3 <-
#   brm(
#     y ~ service + (1 | s) + (1 | dept),
#     data = lme4::InstEval[1:500,],
#     prior = pr,
#     cores = 4,
#     thin  = 40
#   )
#
# brm_4 <-
#   brm(
#     giniPercap ~ math + year + (1 + year | country) + (1 + year | continent),
#     data = dplyr::mutate(noiris::pisa, year = year-min(year), math = scale(math)[,1]),
#     prior = pr,
#     cores = 4,
#     thin  = 40
#   )
#
# brm_5 <-
#   brm(
#     giniPercap ~ math + year + (1 + year + math | country) ,
#     data = dplyr::mutate(noiris::pisa, year = year-min(year), math = scale(math)[,1]),
#     prior = pr,
#     cores = 4,
#     thin  = 40
#   )
#
#
# save(
#   brm_glm,
#   brm_1,
#   brm_2,
#   brm_3,
#   brm_4,
#   brm_5,
#   file = 'tests/testthat/brm_results.RData'
# )
