context('test summarize_model')


# Test initial checks -----------------------------------------------------

# initial checks are done through extract_vc and extract_fixed, but eyeball
# inspection should also be done due to limited nature of testing printed output


# lme4 --------------------------------------------------------------------

test_that('summarize_model.merMod basic functionality: no covariates', {
  expect_output(suppressMessages(summarize_model(
    lmer_0,
    ci     = FALSE,
    show_cor_re = TRUE,
    show_cor_fe = TRUE
  )))
})

test_that('summarize_model.merMod basic functionality: random intercept only', {
  expect_output(suppressMessages(summarize_model(lmer_1, ci = FALSE)))
})

test_that('summarize_model.merMod basic functionality: random slopes', {
  expect_output(suppressMessages(summarize_model(lmer_2, ci = FALSE)))
})

test_that('summarize_model.merMod basic functionality: multiple grouping factors', {
  expect_output(suppressMessages(summarize_model(lmer_3, ci = FALSE)))
})

test_that('summarize_model.merMod basic functionality: ints/slopes with multiple grouping factors', {
  expect_output(suppressMessages(summarize_model(lmer_4, ci = FALSE)))
})

test_that('summarize_model.merMod invisibly returns results', {
  invisible(
    utils::capture.output(
      init <- suppressMessages(summarize_model(lmer_2, ci = FALSE))
    )
  )

  expect_type(init, 'list')
  expect_s3_class(init$vc, 'data.frame')
  expect_s3_class(init$fe, 'data.frame')
})

test_that('summarize_model.merMod does ci', {
  expect_output(suppressMessages(summarize_model(lmer_2)))
})

test_that('summarize_model.merMod does exponentiate', {
  expect_output(summarize_model(glmer_1, ci = FALSE, exponentiate = TRUE))
  invisible(
    utils::capture.output(
      init <- suppressMessages(summarize_model(lmer_2, ci = FALSE))
    )
  )


})

test_that('summarize_model.merMod does show_cor_re', {
  expect_output(suppressMessages(summarize_model(
    lmer_1, ci = FALSE, show_cor_re = TRUE
  )))
  expect_output(suppressMessages(summarize_model(
    lmer_2, ci = FALSE, show_cor_re = TRUE
  )))
  expect_output(suppressMessages(summarize_model(
    lmer_4, ci = FALSE, show_cor_re = TRUE
  )))
})

test_that('summarize_model.merMod does show_cor_fe', {
  expect_output(suppressMessages(summarize_model(
    lmer_0, ci = FALSE, show_cor_fe = TRUE
  )))
  expect_output(suppressMessages(summarize_model(
    lmer_1, ci = FALSE, show_cor_fe = TRUE
  )))
})



# glmmTMB -----------------------------------------------------------------

test_that('summarize_model.glmmTMB basic functionality: no covariates', {
  expect_output(suppressMessages(summarize_model(
    tmb_0,
    ci = FALSE,
    show_cor_re = TRUE,
    show_cor_fe = TRUE
  )))
})

test_that('summarize_model.glmmTMB basic functionality: random intercept only', {
  expect_output(suppressMessages(summarize_model(tmb_1, ci = FALSE)))
})

test_that('summarize_model.glmmTMB basic functionality: random slopes', {
  expect_output(suppressMessages(summarize_model(tmb_2, ci = FALSE)))
})

test_that('summarize_model.glmmTMB basic functionality: multiple grouping factors', {
  expect_output(suppressMessages(summarize_model(tmb_3, ci = FALSE)))
})

test_that('summarize_model.glmmTMB basic functionality: ints/slopes with multiple grouping factors', {
  expect_output(suppressWarnings(suppressMessages(summarize_model(tmb_4, ci = FALSE))))
})

test_that('summarize_model.glmmTMB invisibly returns results', {
  invisible(
    utils::capture.output(
      init <- suppressMessages(summarize_model(tmb_2, ci = FALSE))
    )
  )
  expect_type(init, 'list')
  expect_s3_class(init$vc, 'data.frame')
  expect_s3_class(init$fe, 'data.frame')
})


test_that('summarize_model.glmmTMB does ci', {
  expect_output(suppressMessages(summarize_model(tmb_2)))
})

test_that('summarize_model.glmmTMB does exponentiate', {
  invisible(
    utils::capture.output(
      init <- suppressMessages(summarize_model(tmb_zip, exponentiate = TRUE))
    )
  )

  expect_output(summarize_model(tmb_zip, exponentiate = TRUE))
  expect_true(all(init$fe$Value > 0))
})


test_that('summarize_model.glmmTMB does show_cor_re', {
  expect_output(suppressMessages(summarize_model(
    tmb_1, ci = FALSE, show_cor_re = TRUE
  )))
  expect_output(suppressMessages(summarize_model(
    tmb_2, ci = FALSE, show_cor_re = TRUE
  )))
})

test_that('summarize_model.glmmTMB does show_cor_fe', {
  expect_output(suppressMessages(summarize_model(
    tmb_0, ci = FALSE, show_cor_fe = TRUE
  )))
  expect_output(suppressMessages(summarize_model(
    tmb_1, ci = FALSE, show_cor_fe = TRUE
  )))
})

test_that('summarize_model.glmmTMB does zip component', {
  expect_output(suppressWarnings(suppressMessages(
    summarize_model(tmb_zip, ci = FALSE, component = 'zi')
  )))
})



# nlme --------------------------------------------------------------------

test_that('summarize_model.lme basic functionality: no covariates', {
  expect_output(suppressMessages(summarize_model(
    lme_0,
    ci = FALSE,
    show_cor_re = TRUE,
    show_cor_fe = TRUE
  )))
})

test_that('summarize_model.lme basic functionality: random intercept only', {
  expect_output(suppressMessages(summarize_model(lme_1, ci = FALSE)))
})

test_that('summarize_model.lme basic functionality: random slopes', {
  expect_output(suppressMessages(summarize_model(lme_2, ci = FALSE)))
})

test_that('summarize_model.lme basic functionality: multiple grouping factors', {
  expect_output(suppressMessages(summarize_model(lme_3, ci = FALSE)))
})

test_that('summarize_model.lme basic functionality: ints/slopes with multiple grouping factors', {
  expect_output(suppressMessages(summarize_model(lme_4, ci = FALSE)))
})

test_that('summarize_model.lme basic functionality: nlme', {
  expect_output(suppressMessages(summarize_model(nlme_1, ci = FALSE)))
})

test_that('summarize_model.lme invisibly returns results', {
  invisible(
    utils::capture.output(
      init <- suppressMessages(summarize_model(lme_2, ci = FALSE))
    )
  )

  expect_type(init, 'list')
  expect_s3_class(init$vc, 'data.frame')
  expect_s3_class(init$fe, 'data.frame')
})


test_that('summarize_model.lme does ci', {
  expect_output(suppressMessages(summarize_model(lme_2)))
})

test_that('summarize_model.lme does show_cor_re', {
  expect_output(suppressMessages(summarize_model(
    lme_1, ci = FALSE, show_cor_re = TRUE
  )))
  expect_output(suppressMessages(summarize_model(
    lme_2, ci = FALSE, show_cor_re = TRUE
  )))
})

test_that('summarize_model.lme does show_cor_fe', {
  expect_output(suppressMessages(summarize_model(
    lme_0, ci = FALSE, show_cor_fe = TRUE
  )))

  expect_output(suppressMessages(summarize_model(
    lme_1, ci = FALSE, show_cor_fe = TRUE
  )))
})



# brms --------------------------------------------------------------------

test_that('summarize_model.brmsfit basic functionality: no covariates', {
  expect_output(suppressMessages(summarize_model(
    brm_0,
    ci = FALSE,
    show_cor_re = TRUE,
    show_cor_fe = TRUE
  )))
})

test_that('summarize_model.brmsfit basic functionality: random intercept only', {
  expect_output(suppressMessages(summarize_model(brm_1, ci = FALSE)))
})

test_that('summarize_model.brmsfit basic functionality: random slopes', {
  expect_output(suppressMessages(summarize_model(brm_2, ci = FALSE)))
})

test_that('summarize_model.brmsfit basic functionality: multiple grouping factors', {
  expect_output(suppressMessages(summarize_model(brm_3, ci = FALSE)))
})

test_that('summarize_model.brmsfit basic functionality: ints/slopes with multiple grouping factors', {
  expect_output(suppressMessages(summarize_model(brm_4, ci = FALSE)))
})

test_that('summarize_model.brmsfit basic functionality: ints/slopes with multiple grouping factors', {
  expect_output(suppressMessages(summarize_model(brm_glm)))
})

test_that('summarize_model.brmsfit invisibly returns results', {
  invisible(
    utils::capture.output(
      init <- suppressMessages(summarize_model(brm_2, ci = FALSE))
    )
  )

  expect_type(init, 'list')
  expect_s3_class(init$vc, 'data.frame')
  expect_s3_class(init$fe, 'data.frame')
})

test_that('summarize_model.brmsfit does ci', {
  invisible(
    utils::capture.output(
      init <- suppressMessages(summarize_model(brm_2, ci = TRUE))
    )
  )

  cn = colnames(init$vc)
  expect_true(all(c("SD_2.5", "SD_97.5") %in% cn))
})

test_that('summarize_model.brmsfit does exponentiate', {

  invisible(
    utils::capture.output(
      init <- suppressMessages(summarize_model(brm_glm, exponentiate = TRUE))
    )
  )

  expect_output(suppressMessages(summarize_model(brm_glm, exponentiate = TRUE)))
  expect_true(all(init$fe$Value > 0))
})


test_that('summarize_model.brmsfit does show_cor_re', {
  expect_output(suppressMessages(summarize_model(
    brm_1, ci = FALSE, show_cor_re = TRUE
  )))
  expect_output(suppressMessages(summarize_model(
    brm_2, ci = FALSE, show_cor_re = TRUE
  )))
})

test_that('summarize_model.brmsfit does show_cor_fe', {
  expect_output(suppressMessages(summarize_model(
    brm_0, ci = FALSE, show_cor_fe = TRUE
  )))
  expect_output(suppressMessages(summarize_model(
    brm_1, ci = FALSE, show_cor_fe = TRUE
  )))
})



# rstanarm ----------------------------------------------------------------

test_that('summarize_model.stanreg basic functionality: no covariates', {
  expect_output(suppressMessages(summarize_model(
    stan_glmer_0,
    ci = FALSE,
    show_cor_re = TRUE,
    show_cor_fe = TRUE
  )))
})

test_that('summarize_model.stanreg basic functionality: random intercept only', {
  expect_output(suppressMessages(summarize_model(stan_glmer_1, ci = FALSE)))
})

test_that('summarize_model.stanreg basic functionality: random slopes', {
  expect_output(suppressMessages(summarize_model(stan_glmer_2, ci = FALSE)))
})

test_that('summarize_model.stanreg basic functionality: multiple grouping factors', {
  expect_output(suppressMessages(summarize_model(stan_glmer_3, ci = FALSE)))
})

test_that('summarize_model.stanreg basic functionality: ints/slopes with multiple grouping factors', {
  expect_output(suppressMessages(summarize_model(stan_glmer_4, ci = FALSE)))
})

test_that('summarize_model.stanreg basic functionality: ints/slopes with multiple grouping factors', {
  expect_output(suppressMessages(summarize_model(stan_glmer_glm)))
})

test_that('summarize_model.stanreg invisibly returns results', {
  invisible(
    utils::capture.output(
      init <- suppressMessages(summarize_model(stan_glmer_2, ci = FALSE))
    )
  )

  expect_type(init, 'list')
  expect_s3_class(init$vc, 'data.frame')
  expect_s3_class(init$fe, 'data.frame')
})

test_that('summarize_model.stanreg does ci', {
  expect_output(suppressMessages(summarize_model(stan_glmer_2, ci = TRUE)))
})

test_that('summarize_model.stanreg does exponentiate', {

  invisible(
    utils::capture.output(
      init <- suppressMessages(summarize_model(stan_glmer_glm, exponentiate = TRUE))
    )
  )

  expect_output(suppressMessages(summarize_model(stan_glmer_glm, exponentiate = TRUE)))
  expect_true(all(init$fe$Value > 0))

})

test_that('summarize_model.stanreg does mv', {
  expect_output(suppressMessages(summarize_model(stan_glmer_mv)))
})

test_that('summarize_model.stanreg does jm', {
  expect_output(suppressMessages(summarize_model(stan_glmer_jm)))
})



# mgcv --------------------------------------------------------------------


test_that('summarize_model.gam basic functionality: no covariates', {
  expect_output(suppressMessages(summarize_model(
    gam_0,
    ci = FALSE,
    show_cor_re = TRUE,
    show_cor_fe = TRUE
  )))
})

test_that('summarize_model.gam basic functionality: random intercept only', {
  expect_output(suppressMessages(summarize_model(gam_1, ci = FALSE)))
})

test_that('summarize_model.gam basic functionality: random slopes', {
  expect_output(suppressMessages(summarize_model(gam_2, ci = FALSE)))
})

test_that('summarize_model.gam basic functionality: multiple grouping factors', {
  expect_output(suppressMessages(summarize_model(gam_3, ci = FALSE)))
})


test_that('summarize_model.gam invisibly returns results', {
  invisible(
    utils::capture.output(
      init <- suppressMessages(summarize_model(gam_2, ci = FALSE))
    )
  )

  expect_type(init, 'list')
  expect_s3_class(init$vc, 'data.frame')
  expect_s3_class(init$fe, 'data.frame')
})

test_that('summarize_model.gam handles cor', {
  expect_output(
    suppressMessages(summarize_model(
      gam_2,
      ci = FALSE,
      show_cor_re = TRUE,
      show_cor_fe = TRUE
    ))
  )
})


# Problem::removes Term names somehow
test_that('summarize_model.gam does exponentiate', {

  invisible(
    utils::capture.output(
      init <- suppressMessages(summarize_model(gam_glm, exponentiate = TRUE))
    )
  )

  expect_output(suppressMessages(summarize_model(gam_glm, exponentiate = TRUE)))
  expect_true(all(init$fe$Value > 0))

})
