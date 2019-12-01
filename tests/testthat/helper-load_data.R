
# for testing
# all_mods = list.files('tests/testthat/', pattern = 'RData', full.names = T)
# sapply(all_mods, load, envir = .GlobalEnv)

# Run lme4 models ---------------------------------------------------------

load('lme4_results.RData')

data("sleepstudy", package = 'lme4')

# library(lme4)
#
# lmer_0 <- lmer(Reaction ~ (1|Subject), data = sleepstudy)
# lmer_1 <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
# lmer_2 <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
# lmer_3 <- lmer(y ~ service + (1 | s) + (1 | d), data = InstEval[1:1000, ])
#
# # unusually complex models just for testing
# suppressWarnings({
#   lmer_4 <- lmer(y ~ service + (1 + as.numeric(lectage)| s) + (1 + as.numeric(studage)| d) + (1|dept), data = InstEval[1:5000, ])
#   lmer_5 <- lmer(y ~ service + (1 + as.numeric(lectage) + as.numeric(studage) + service| d) , data = InstEval[1:5000, ])
# })
#
# glmer_1 <- glmer(
#   count ~ spp + mined + (1 | site),
#   family = poisson,
#   data = glmmTMB::Salamanders
# )
#
#
# save(
#   lmer_0,
#   lmer_1,
#   lmer_2,
#   lmer_3,
#   lmer_4,
#   lmer_5,
#   glmer_1,
#   file = 'tests/testthat/lme4_results.RData'
# )

# Run TMB models ----------------------------------------------------------

load('tmb_results.RData')

# library(glmmTMB)
#
# tmb_0 <- glmmTMB(Reaction ~ (1 | Subject), data = sleepstudy)
# tmb_1 <- glmmTMB(Reaction ~ Days + (1 | Subject), data = sleepstudy)
# tmb_2 <- glmmTMB(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
# tmb_3 <- glmmTMB(y ~ service + (1 | s) + (1 | d), data = InstEval[1:1000, ])
#
# suppressWarnings({
#   tmb_4 <- glmmTMB(y ~ service + (1 + as.numeric(lectage)| s) + (1 + as.numeric(studage)| d) + (1|dept),
#                    data = InstEval[1:5000, ])
# })
#
# tmb_zip <- glmmTMB(
#   count ~ spp + mined + (1 | site),
#   zi =  ~ spp + mined + (1 | site),
#   family = truncated_poisson,
#   data = Salamanders
# )
#
# save(
#   tmb_0,
#   tmb_1,
#   tmb_2,
#   tmb_3,
#   tmb_4,
#   tmb_zip,
#   file = 'tests/testthat/tmb_results.RData'
# )

# Run nlme models ---------------------------------------------------------

load('nlme_results.RData')
#
# library(nlme)
#
# lme_0 <- lme(Reaction ~ 1, random = ~ 1 | Subject, data = sleepstudy)
# lme_1 <- lme(Reaction ~ Days, random = ~ 1 | Subject, data = sleepstudy)
# lme_2 <- lme(Reaction ~ Days, random = ~ 1 + Days | Subject, data = sleepstudy)
# lme_3 <- lme(y ~ service,
#              random = list(d = ~ 1, s = ~ 1),
#              data = droplevels(InstEval[1:1000, ]))
# lme_4 <- lme(y ~ service,
#              random = list(d = ~ 1 + service, s = ~ 1 ),
#              data = droplevels(InstEval[1:3000, ]))
#
# # won't converge
# # lme_5 <- lme(y ~ service,
# #              random = list(d = ~ 1 + as.numeric(lectage) + as.numeric(studage) + service) ,
# #              data = droplevels(InstEval[1:5000, ]))
#
# nlme_1 <-  nlme(height ~ SSasymp(age, Asym, R0, lrc),
#                 data = Loblolly,
#                 fixed = Asym + R0 + lrc ~ 1,
#                 random = Asym ~ 1,
#                 start = c(Asym = 103, R0 = -8.5, lrc = -3.3))
#
# Just run these at test time?
#
# lme_het_var <- lme(
#   distance ~ scale(age) + Sex,
#   data = Orthodont,
#   random = ~ 1 | Subject,
#   weights = varIdent(form = ~ 1 | Sex)
# )
#
# lme_corCompSymm <-
#   update(
#     lme_2,
#     corr = corCompSymm(form = ~ Days),
#     data = sleepstudy %>% dplyr::filter(Days < 5)
#   )
#
# lme_corSymm <-
#   update(
#     lme_2,
#     corr = corSymm(form = ~ 1 | Subject),
#     data = sleepstudy %>% dplyr::filter(Days < 5)
#   )
#
# lme_corAR <-
#   update(
#     lme_2,
#     corr = corAR1(form = ~ Days),
#     data = sleepstudy %>% dplyr::filter(Days < 5)
#   )
#
# lme_corARMA <-
#   update(
#     lme_2,
#     corr = corARMA(form = ~ Days, p = 1, q = 1),
#     data = sleepstudy %>% dplyr::filter(Days < 5)
#   )
#
# lme_corCAR <-
#   update(
#     lme_2,
#     corr = corCAR1(form = ~ Days),
#     data = sleepstudy %>% dplyr::filter(Days < 5)
#   )
#
# save(
#   lme_0,
#   lme_1,
#   lme_2,
#   lme_3,
#   lme_4,
#   nlme_1,
#   lme_het_var,
#   lme_corCompSymm,
#   lme_corSymm,
#   lme_corAR,
#   lme_corARMA,
#   lme_corCAR,
#   file = 'tests/testthat/nlme_results.RData'
# )

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
# brm_0 <-
#   brm(
#     Reaction ~ Days + (1 | Subject),
#     data = lme4::sleepstudy,
#     prior = pr,
#     cores = 4,
#     thin  = 40
#   )
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
#     data = lme4::InstEval[1:1000,],
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
#   brm_0,
#   brm_1,
#   brm_2,
#   brm_3,
#   brm_4,
#   brm_5,
#   file = 'tests/testthat/brm_results.RData'
# )

# Run rstanarm models ---------------------------------------------------------


load('rstanarm_results.RData')

# library(rstanarm)
#
#
#
# stan_glmer_glm <- stan_glmer(
#   count ~ zAge + zBase * Trt + (1 | patient),
#   data = epilepsy,
#   family = poisson(),
#   prior = student_t(5,0,10),
#   cores = 4,
#   thin  = 40
# )
#
# stan_glmer_0 <-
#   stan_glmer(
#     Reaction ~ (1 | Subject),
#     data = lme4::sleepstudy,
#     prior = normal(0, 10),
#     cores = 4,
#     thin  = 40
#   )
#
# stan_glmer_1 <-
#   stan_glmer(
#     Reaction ~ Days + (1 | Subject),
#     data = lme4::sleepstudy,
#     prior = normal(0, 10),
#     cores = 4,
#     thin  = 40
#   )
#
# stan_glmer_2 <-
#   stan_glmer(
#     Reaction ~ Days + (1 + Days | Subject),
#     data = lme4::sleepstudy,
#     prior = normal(0, 10),
#     cores = 4,
#     thin  = 40
#   )
#
#
#
# # more complex models just for testing (changed from others to minimize nparameters/object size)
# stan_glmer_3 <-
#   stan_glmer(
#     y ~ service + (1 | s) + (1 | dept),
#     data = lme4::InstEval[1:1000,],
#     prior = normal(0, 10),
#     cores = 4,
#     thin  = 40
#   )
#
# stan_glmer_4 <-
#   stan_glmer(
#     giniPercap ~ math + year + (1 + year | country) + (1 + year | continent),
#     data = dplyr::mutate(noiris::pisa, year = year-min(year), math = scale(math)[,1]),
#     prior = normal(0, 10),
#     cores = 4,
#     thin  = 40
#   )
#
# stan_glmer_5 <-
#   stan_glmer(
#     giniPercap ~ math + year + (1 + year + math | country) ,
#     data = dplyr::mutate(noiris::pisa, year = year-min(year), math = scale(math)[,1]),
#     prior = normal(0, 10),
#     cores = 4,
#     thin  = 40
#   )
#
#
# save(
#   stan_glmer_glm,
#   stan_glmer_0,
#   stan_glmer_1,
#   stan_glmer_2,
#   stan_glmer_3,
#   stan_glmer_4,
#   stan_glmer_5,
#   file = 'tests/testthat/rstanarm_results.RData'
# )

# Run mgcv models ---------------------------------------------------------

load('mgcv_results.RData')

# library(mgcv)
#
# gam_0 = gam(Reaction ~  s(Subject, bs = 're'),
#                   data = lme4::sleepstudy,
#                   method = 'REML')
#
# gam_1 = gam(Reaction ~  Days + s(Subject, bs = 're'),
#                   data = lme4::sleepstudy,
#                   method = 'REML')
#
# gam_2 = gam(
#   Reaction ~  Days +
#     s(Subject, bs = 're') +
#     s(Days, Subject, bs = 're'),
#   data = lme4::sleepstudy,
#   method = 'REML'
# )
#
# gam_3 <- gam(
#   y ~ service +
#     s(s, bs = 're') +
#     s(dept, bs = 're'),
#   method = 'REML',
#   data = lme4::InstEval[1:1000, ]
# )
#
# gam_glm = gam(
#   count ~ spp + mined + (1 | site),
#   family = poisson,
#   data = glmmTMB::Salamanders
# )
#
#
# bam objects are very large to save even for small models
# bam_1 = bam(
#   y ~ service +
#     s(d, bs = 're') +
#     s(dept, bs = 're'),
#   data = lme4::InstEval[1:1000, ],
#   nthreads = 10
#   )
#
#
# save(
#   gam_0,
#   gam_1,
#   gam_2,
#   gam_3,
#   gam_glm,
#   bam_1,
#   file = 'tests/testthat/mgcv_results.RData'
# )
