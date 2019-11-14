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
  expect_s3_class(extract_fixed_effects(lmer_2, ci_level = 0), 'data.frame')
})

test_that('extract_fixed_effects.merMod handles ci args', {
  expect_s3_class(extract_fixed_effects(lmer_2, ci_args = list(method = 'profile')), 'data.frame')
})

test_that('extract_fixed_effects.merMod handles digits', {
  expect_s3_class(extract_fixed_effects(lmer_2, digits = 2), 'data.frame')
})

test_that('extract_fixed_effects.merMod handles glmer', {
  expect_equal(ncol(extract_fixed_effects(glmer_1, ci_level = 0)),  5)  # has z and p-value
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
  expect_s3_class(extract_fixed_effects(tmb_2, ci_level = 0), 'data.frame')
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
  expect_s3_class(extract_fixed_effects(nlme_1, ci_level = 0), 'data.frame')
})


test_that('extract_fixed_effects.merMod handles digits', {
  expect_s3_class(extract_fixed_effects(lme_2, digits = 2), 'data.frame')
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

# TODO: test distributional models
