context('test extract_fixed')


# Test initial checks -----------------------------------------------------



test_that('extract_fixed errors with wrong model', {
  mod = lm(mpg ~ vs, mtcars)
  expect_error(extract_fixed(mod))
})

test_that('extract_fixed errors with wrong ci_level', {
  expect_error(extract_fixed(lmer_1, ci_level = 2))
})



# Test lme4 ---------------------------------------------------------------

context('test extract_fixed.merMod')

test_that('extract_fixed.merMod basic functionality: random intercept only', {
  expect_s3_class(extract_fixed(lmer_1), 'data.frame')
})

test_that('extract_fixed.merMod basic functionality: random slopes', {
  expect_s3_class(extract_fixed(lmer_2), 'data.frame')
})

test_that('extract_fixed.merMod basic functionality: multiple grouping factors', {
  expect_s3_class(extract_fixed(lmer_3), 'data.frame')
})

test_that('extract_fixed.merMod basic functionality: ints/slopes with multiple grouping factors', {
  expect_s3_class(extract_fixed(lmer_4), 'data.frame')
})

test_that('extract_fixed.merMod handles no ci', {
  expect_s3_class(extract_fixed(lmer_2, ci_level = 0), 'data.frame')
})

test_that('extract_fixed.merMod handles ci args', {
  expect_s3_class(extract_fixed(lmer_2, ci_args = list(method = 'profile')), 'data.frame')
})

test_that('extract_fixed.merMod handles digits', {
  expect_s3_class(extract_fixed(lmer_2, digits = 2), 'data.frame')
})

test_that('extract_fixed.merMod handles glmer', {
  expect_s3_class(extract_fixed(glmer_1), 'data.frame')
})




# Test glmmTMB ------------------------------------------------------------

context('test extract_fixed.glmmTMB')


test_that('extract_fixed.glmmTMB basic functionality: random intercept only', {
  expect_s3_class(extract_fixed(tmb_1), 'data.frame')
})

test_that('extract_fixed.glmmTMB basic functionality: random slopes', {
  expect_s3_class(extract_fixed(tmb_2), 'data.frame')
})

test_that('extract_fixed.glmmTMB basic functionality: multiple grouping factors', {
  expect_s3_class(extract_fixed(tmb_3), 'data.frame')
})

test_that('extract_fixed.glmmTMB basic functionality: ints/slopes with multiple grouping factors', {
  expect_warning(extract_fixed(tmb_4))
})

test_that('extract_fixed.glmmTMB handles no ci', {
  expect_s3_class(extract_fixed(tmb_2, ci_level = 0), 'data.frame')
})

test_that('extract_fixed.glmmTMB handles ci args', {
  expect_s3_class(extract_fixed(tmb_2, ci_args = list(method = 'profile')), 'data.frame')
})

test_that('extract_fixed.glmmTMB handles digits', {
  expect_s3_class(extract_fixed(tmb_2, digits = 2), 'data.frame')
})

test_that('extract_fixed.glmmTMB errors with wrong cond', {
  expect_error(extract_fixed(tmb_zip, component = 'zip'))
})

test_that('extract_fixed.glmmTMB handles other cond', {
  expect_s3_class(extract_fixed(tmb_zip, component = 'zi'), 'data.frame')
})


# Test nlme ---------------------------------------------------------------

context('test extract_fixed.lme')


test_that('extract_fixed.lme basic functionality: random intercept only', {
  expect_s3_class(extract_fixed(lme_1), 'data.frame')
})

test_that('extract_fixed.lme basic functionality: random slopes', {
  expect_s3_class(extract_fixed(lme_2), 'data.frame')
})

test_that('extract_fixed.lme basic functionality: multiple grouping factors', {
  expect_s3_class(extract_fixed(lme_3), 'data.frame')
})

test_that('extract_fixed.lme basic functionality: ints/slopes with multiple grouping factors', {
  expect_s3_class(extract_fixed(lme_4), 'data.frame')
})

test_that('extract_fixed.lme basic functionality: nlme', {
  expect_s3_class(extract_fixed(nlme_1), 'data.frame')
})


test_that('extract_fixed.lme handles no ci', {
  expect_s3_class(extract_fixed(nlme_1, ci_level = 0), 'data.frame')
})


test_that('extract_fixed.merMod handles digits', {
  expect_s3_class(extract_fixed(lme_2, digits = 2), 'data.frame')
})


# Test brms ---------------------------------------------------------------


context('test extract_fixed.brmsfit')

test_that('extract_fixed.brmsfit basic functionality: random intercept only', {
  expect_s3_class(extract_fixed(brm_1), 'data.frame')
})

test_that('extract_fixed.brmsfit basic functionality: random slopes', {
  expect_s3_class(extract_fixed(brm_2), 'data.frame')
})

test_that('extract_fixed.brmsfit basic functionality: multiple grouping factors', {
  expect_s3_class(extract_fixed(brm_3), 'data.frame')
})

test_that('extract_fixed.brmsfit basic functionality: non-gaussian', {
  expect_s3_class(extract_fixed(brm_glm), 'data.frame')
})

test_that('extract_fixed.brmsfit handles digits', {
  expect_s3_class(extract_fixed(brm_3, digits = 2), 'data.frame')
})


# TODO: test distributional models
