context('test extract_model_data')


# Test initial checks -----------------------------------------------------


test_that('extract_fixed_effects errors with wrong model', {
  mod = lm(mpg ~ vs, mtcars)
  expect_error(extract_model_data(mod))
})


# standard models ---------------------------------------------------------

# these have a model.frame method

test_that('extract_fixed_effects works', {
  expect_s3_class(extract_model_data(lmer_1), 'data.frame')
})
test_that('extract_fixed_effects works', {
  expect_s3_class(extract_model_data(brm_1), 'data.frame')
})
test_that('extract_fixed_effects works', {
  expect_s3_class(extract_model_data(stan_glmer_1), 'data.frame')
})
test_that('extract_fixed_effects works', {
  expect_s3_class(extract_model_data(tmb_1), 'data.frame')
})
test_that('extract_fixed_effects works', {
  expect_s3_class(extract_model_data(gam_1), 'data.frame')
})

# Non-standard models -----------------------------------------------------

# these don't
test_that('extract_fixed_effects works with lme', {
  expect_s3_class(extract_model_data(nlme_1), 'data.frame')
})
