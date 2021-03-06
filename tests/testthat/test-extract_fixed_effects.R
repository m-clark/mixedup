context('test extract_fixed_effects')


# Test initial checks -----------------------------------------------------



test_that('extract_fixed_effects errors with wrong model', {
  mod = lm(mpg ~ vs, mtcars)
  expect_error(extract_fixed_effects(mod))
})

test_that('extract_fixed_effects errors with wrong ci_level', {
  expect_error(extract_fixed_effects(lmer_1, ci_level = 2))
})



# Test lme4 ---------------------------------------------------------------

context('test extract_fixed_effects.merMod')

test_that('extract_fixed_effects.merMod basic functionality: random intercept only', {
  expect_s3_class(extract_fixed_effects(lmer_1), 'data.frame')
})

test_that('extract_fixed_effects.merMod basic functionality: random slopes', {
  expect_s3_class(extract_fixed_effects(lmer_2), 'data.frame')
})

test_that('extract_fixed_effects.merMod basic functionality: multiple grouping factors', {
  expect_s3_class(extract_fixed_effects(lmer_3), 'data.frame')
})

test_that('extract_fixed_effects.merMod basic functionality: ints/slopes with multiple grouping factors', {
  expect_s3_class(extract_fixed_effects(lmer_4), 'data.frame')
})

test_that('extract_fixed_effects.merMod handles no ci', {
  init = extract_fixed_effects(lmer_2, ci_level = 0)
  expect_s3_class(init, 'data.frame')
  expect_equal(init$term, c('Intercept', 'Days'))  # test that term names are not lost
})

test_that('extract_fixed_effects.merMod handles ci args', {
  expect_s3_class(extract_fixed_effects(lmer_2, ci_args = list(method = 'profile')), 'data.frame')
})

test_that('extract_fixed_effects.merMod handles digits', {
  expect_s3_class(extract_fixed_effects(lmer_2, digits = 2), 'data.frame')
})

test_that('extract_fixed_effects.merMod handles glmer', {
  expect_equal(ncol(extract_fixed_effects(glmer_1, ci_level = 0)),  5)  # has z and p-value need to update
})

test_that('extract_fixed_effects.merMod exponentiates', {
  exp_res = extract_fixed_effects(
    glmer_1,
    ci_level = .95,
    digits = 5,
    exponentiate = TRUE
  )
  noexp_res = extract_fixed_effects(glmer_1, ci_level = .95, digits = 5)

  expect_equal(exp_res$value[1],  exp(noexp_res$value[1]), tolerance = 1e-4)
  expect_equal(noexp_res$term[1], 'Intercept')  # test that term names are not lost
})


test_that('extract_fixed_effects.merMod handles p_value', {

  expect_equal(extract_fixed_effects(lmer_2)$p_value,  c(0, 0))
  expect_equal(extract_fixed_effects(lmer_2, p_value = 'KR')$p_value,  c(0, 0))
})

test_that('extract_fixed_effects.merMod warns/corrects on wrong p_value input', {
  expect_warning(extract_fixed_effects(lmer_2, p_value = 'KR20'))
})

test_that('extract_fixed_effects.merMod is null if no covariates', {
  expect_null(extract_fixed_effects(lme4::lmer(Reaction ~ -1 + (1|Subject), lme4::sleepstudy)))
})

test_that('extract_fixed_effects.merMod fails with lmerTest model', {
  expect_error(extract_fixed_effects(lmerTest::lmer(Reaction ~ Days + (1|Subject), lme4::sleepstudy)))
})


# Test glmmTMB ------------------------------------------------------------

context('test extract_fixed_effects.glmmTMB')


test_that('extract_fixed_effects.glmmTMB basic functionality: random intercept only', {
  expect_s3_class(extract_fixed_effects(tmb_1), 'data.frame')
})


test_that('extract_fixed_effects.glmmTMB basic functionality: random slopes', {
  expect_s3_class(extract_fixed_effects(tmb_2), 'data.frame')
})


test_that('extract_fixed_effects.glmmTMB basic functionality: multiple grouping factors', {
  expect_s3_class(extract_fixed_effects(tmb_3), 'data.frame')
})


test_that('extract_fixed_effects.glmmTMB basic functionality: ints/slopes with multiple grouping factors', {
  expect_warning(extract_fixed_effects(tmb_4))
})


test_that('extract_fixed_effects.glmmTMB handles no ci', {
  init = extract_fixed_effects(tmb_2, ci_level = 0)
  expect_s3_class(init, 'data.frame')
  expect_equal(init$term, c('Intercept', 'Days'))
})


test_that('extract_fixed_effects.glmmTMB handles ci args', {
  expect_s3_class(extract_fixed_effects(tmb_2, ci_args = list(method = 'profile')), 'data.frame')
})


test_that('extract_fixed_effects.glmmTMB handles digits', {
  expect_s3_class(extract_fixed_effects(tmb_2, digits = 2), 'data.frame')
})


test_that('extract_fixed_effects.glmmTMB errors with wrong cond', {
  expect_error(extract_fixed_effects(tmb_zip, component = 'zip'))
})


test_that('extract_fixed_effects.glmmTMB handles other cond', {
  expect_s3_class(extract_fixed_effects(tmb_zip, component = 'zi'), 'data.frame')
})


test_that('extract_fixed_effects.glmmTMB exponentiates', {
  exp_res = extract_fixed_effects(
    tmb_zip,
    digits = 5,
    ci_level = .95,
    exponentiate = TRUE
  )
  noexp_res = extract_fixed_effects(tmb_zip, digits = 5, ci_level = .95)

  expect_equal(exp_res$value[1],  exp(noexp_res$value[1]), tolerance = 1e-4)
  expect_equal(exp_res$term[1], 'Intercept')
})


# Test nlme ---------------------------------------------------------------

context('test extract_fixed_effects.lme')


test_that('extract_fixed_effects.lme basic functionality: random intercept only', {
  expect_s3_class(extract_fixed_effects(lme_1), 'data.frame')
})

test_that('extract_fixed_effects.lme basic functionality: random slopes', {
  expect_s3_class(extract_fixed_effects(lme_2), 'data.frame')
})

test_that('extract_fixed_effects.lme basic functionality: multiple grouping factors', {
  expect_s3_class(extract_fixed_effects(lme_3), 'data.frame')
})


test_that('extract_fixed_effects.lme basic functionality: ints/slopes with multiple grouping factors', {
  expect_s3_class(extract_fixed_effects(lme_4), 'data.frame')
})


test_that('extract_fixed_effects.lme basic functionality: nlme', {
  expect_s3_class(extract_fixed_effects(nlme_1), 'data.frame')
})


test_that('extract_fixed_effects.lme handles no ci', {
  init = extract_fixed_effects(lme_2, ci_level = 0)
  expect_s3_class(init, 'data.frame')
  expect_equal(init$term, c('Intercept', 'Days'))
})


test_that('extract_fixed_effects.merMod handles digits', {
  expect_s3_class(extract_fixed_effects(lme_2, digits = 2), 'data.frame')
})


test_that('extract_fixed_effects.lme exponentiates', {
  exp_res = extract_fixed_effects(nlme_1, digits = 5, ci_level = .95, exponentiate = TRUE)
  noexp_res = extract_fixed_effects(nlme_1, digits = 5, ci_level = .95)

  expect_equal(exp_res$value[2],  exp(noexp_res$value[2]), tolerance = 1e-4)
  expect_equal(exp_res$term[1], 'Asym')
})



# Test brms ---------------------------------------------------------------

context('test extract_fixed_effects.brmsfit')

test_that('extract_fixed_effects.brmsfit basic functionality: random intercept only', {
  expect_s3_class(extract_fixed_effects(brm_1), 'data.frame')
})

test_that('extract_fixed_effects.brmsfit basic functionality: random slopes', {
  expect_s3_class(extract_fixed_effects(brm_2), 'data.frame')
})

test_that('extract_fixed_effects.brmsfit basic functionality: multiple grouping factors', {
  expect_s3_class(extract_fixed_effects(brm_3), 'data.frame')
})

test_that('extract_fixed_effects.brmsfit basic functionality: non-gaussian', {
  expect_s3_class(extract_fixed_effects(brm_glm), 'data.frame')
})

test_that('extract_fixed_effects.brmsfit handles digits', {
  expect_s3_class(extract_fixed_effects(brm_3, digits = 2), 'data.frame')
})

test_that('extract_fixed_effects.brmsfit will always provide ci', {
  expect_message(extract_fixed_effects(brm_3, ci_level = 0))
})


test_that('extract_fixed_effects.brmsfit exponentiates', {
  exp_res = extract_fixed_effects(
    brm_glm,
    digits = 5,
    ci_level = .95,
    exponentiate = TRUE
  )
  noexp_res = extract_fixed_effects(brm_glm, digits = 5, ci_level = .95)

  expect_equal(exp_res$value[1],  exp(noexp_res$value[1]), tolerance = 1e-4)
  expect_equal(exp_res$term[1],  'Intercept')
})


test_that('extract_fixed_effects.brmsfit basic functionality: multivariate model', {
  init = extract_fixed_effects(brm_mv, component = 'back', ci_level = .8, digits = 2)
  expect_match(init$term, 'back')
})

test_that('extract_fixed_effects.brmsfit basic functionality: autocor model', {
  expect_s3_class(extract_fixed_effects(brm_corAR, ci_level = .8, digits = 2),
                  'data.frame')
})

test_that('extract_fixed_effects.brmsfit basic functionality: zi model', {
  init = extract_fixed_effects(brm_zi, component = 'zi', ci_level = .8, digits = 2)
  expect_match(init$term, 'zi')
})


# Test rstnarm ---------------------------------------------------------------


context('test extract_fixed_effects.stanreg')

test_that('extract_fixed_effects.stanreg basic functionality: random intercept only', {
  expect_s3_class(extract_fixed_effects(stan_glmer_1), 'data.frame')
})

test_that('extract_fixed_effects.stanreg basic functionality: random slopes', {
  expect_s3_class(extract_fixed_effects(stan_glmer_2), 'data.frame')
})

test_that('extract_fixed_effects.stanreg basic functionality: multiple grouping factors', {
  expect_s3_class(extract_fixed_effects(stan_glmer_3), 'data.frame')
})

test_that('extract_fixed_effects.stanreg basic functionality: non-gaussian', {
  expect_s3_class(extract_fixed_effects(stan_glmer_glm), 'data.frame')
})

test_that('extract_fixed_effects.stanreg handles digits', {
  expect_s3_class(extract_fixed_effects(stan_glmer_3, digits = 2), 'data.frame')
})

test_that('extract_fixed_effects.stanreg will always provide ci', {
  expect_message(extract_fixed_effects(stan_glmer_3, ci_level = 0))
})


test_that('extract_fixed_effects.stanreg exponentiates', {
  exp_res = extract_fixed_effects(stan_glmer_glm, digits = 5, ci_level = .95, exponentiate = TRUE)
  noexp_res = extract_fixed_effects(stan_glmer_glm, digits = 5, ci_level = .95)

  expect_equal(exp_res$value[1],  exp(noexp_res$value[1]), tolerance = 1e-4)

  expect_equal(exp_res$term[1],  'Intercept')
})

# Not yet implemented. Check error if attempted, and warning about component,
# which will eventually be used to pull out a specific outcome.
# test_that('extract_fixed_effects.stanreg basic functionality: multivariate model', {
#   init = extract_fixed_effects(stan_glmer_mv, component = 'back', ci_level = .8, digits = 2)
#   expect_match(init$term, 'back')
# })

test_that('extract_fixed_effects.stanreg basic functionality: multivariate model', {
  expect_error(extract_fixed_effects(stan_glmer_mv))
})

test_that('extract_fixed_effects.stanreg basic functionality: multivariate model', {
  expect_warning(extract_fixed_effects(stan_glmer_2, component = 'flag'))
})





# Test mgcv ---------------------------------------------------------------

context('test extract_fixed_effects.gam')



test_that('extract_fixed_effects.gam basic functionality: random intercept only', {
  expect_s3_class(extract_fixed_effects(gam_1), 'data.frame')
})


test_that('extract_fixed_effects.gam basic functionality: random slopes', {
  expect_s3_class(extract_fixed_effects(gam_2), 'data.frame')
})


test_that('extract_fixed_effects.gam basic functionality: multiple grouping factors', {
  expect_s3_class(extract_fixed_effects(gam_3), 'data.frame')
})


test_that('extract_vc.gam basic functionality: bam', {
  expect_s3_class(extract_vc(bam_1), 'data.frame')
})


test_that('extract_fixed_effects.gam handles no ci', {
  init = extract_fixed_effects(gam_2, ci_level = 0)

  expect_s3_class(init, 'data.frame')
  expect_equal(init$term, c('Intercept', 'Days'))  # test that term names are not lost
})


test_that('extract_fixed_effects.gam handles ci args', {
  expect_s3_class(extract_fixed_effects(gam_2, ci_args = list(method = 'profile')), 'data.frame')
})


test_that('extract_fixed_effects.gam handles digits', {
  expect_s3_class(extract_fixed_effects(gam_2, digits = 2), 'data.frame')
})

test_that('extract_fixed_effects.brmsfit exponentiates', {
  exp_res = extract_fixed_effects(
    gam_glm,
    digits = 5,
    ci_level = .95,
    exponentiate = TRUE
  )
  noexp_res = extract_fixed_effects(gam_glm, digits = 5, ci_level = .95)

  expect_equal(exp_res$value[1],  exp(noexp_res$value[1]), tolerance = 1e-4)
  expect_equal(noexp_res$term[1], 'Intercept')  # test that term names are not lost
})

