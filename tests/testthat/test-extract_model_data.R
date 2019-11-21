context('test extract_model_data')


# Test initial checks -----------------------------------------------------


test_that('extract_fixed_effects errors with wrong model', {
  mod = lm(mpg ~ vs, mtcars)
  expect_error(extract_model_data(mod))
})

test_that('extract_fixed_effects works', {
  expect_s3_class(extract_model_data(lmer_1), 'data.frame')
})

test_that('extract_fixed_effects works with lme', {
  expect_s3_class(extract_model_data(nlme_1), 'data.frame')
})
