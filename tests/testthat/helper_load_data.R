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
