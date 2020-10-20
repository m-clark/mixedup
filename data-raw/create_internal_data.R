
# for quicker loading vignettes

library(brms)

brms_model <-
  brm(Reaction ~ Days + (1 + Days | Subject),
      data = lme4::sleepstudy,
      cores = 4,
      refresh = -1,
      verbose = FALSE,
      thin = 40
  )


library(rstanarm)

rstanarm_model <-
  stan_glmer(Reaction ~ Days + (1 + Days | Subject),
             data = lme4::sleepstudy,
             cores = 4,
             refresh = -1,
             verbose = FALSE,
             thin = 40
  )


usethis::use_data(
  brms_model,
  rstanarm_model,
  internal = TRUE,
  overwrite = TRUE,
  version = 3
)
