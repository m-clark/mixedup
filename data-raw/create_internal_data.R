
# for quicker loading vignettes

brms_model <-
  brms::brm(Reaction ~ Days + (1 + Days | Subject),
      data = lme4::sleepstudy,
      cores = 4,
      refresh = -1,
      verbose = FALSE,
      thin = 40
  )


rstanarm_model <-
  rstanarm::stan_glmer(Reaction ~ Days + (1 + Days | Subject),
             data = lme4::sleepstudy,
             cores = 4,
             refresh = -1,
             verbose = FALSE,
             thin = 40
  )


usethis::use_data(brms_model,
                  rstanarm_model,
                  internal = TRUE,
                  overwrite = TRUE)
