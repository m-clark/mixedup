context('test extract_het_var')

library(nlme)

test_that('extract_het_var fails with wrong class object', {
  expect_error(extract_het_var(lm(rnorm(10)~ 1)))
})

context('test extract_het_var.lme')

test_that('extract_het_var returns a vector', {
  expect_s3_class(extract_het_var(lme_het_var), 'data.frame')
})

test_that('extract_het_var returns a vector of appropriate length', {
  expect_equal(ncol(extract_het_var(lme_het_var)), 2)
})
