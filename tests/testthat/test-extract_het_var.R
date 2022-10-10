context('test extract_het_var')


test_that('extract_het_var fails with wrong class object', {
  expect_error(extract_het_var(lm(rnorm(10)~ 1)))
})



# nlme --------------------------------------------------------------------

library(nlme)

context('test extract_het_var.lme')

test_that('extract_het_var.lme returns a data.frame', {
  expect_s3_class(extract_het_var(lme_het_var), 'data.frame')
})

test_that('extract_het_var.lme errors with no varstruct', {
  expect_error(extract_het_var(lme_2))
})

test_that('extract_het_var.lme returns a data.frame of appropriate length', {
  expect_equal(ncol(extract_het_var(lme_het_var)), 2)
})

test_that('extract_het_var.nlme can do variance scale', {
  initsd  = extract_het_var(lme_het_var, scale = 'sd')
  initvar = extract_het_var(lme_het_var, scale = 'var')

  expect_equal(round(initvar$variance, 2), round(initsd$sd^2, 2))
})


# glmmTMB -----------------------------------------------------------------

# the rest of the tests are at extract_cor_structure

context('test extract_het_var.glmmTMB')

test_that('extract_het_var.glmmTMB returns a data.frame', {
  expect_s3_class(extract_het_var(tmb_diag), 'data.frame')
})

test_that('extract_het_var.glmmTMB errors with no varstruct', {
  expect_error(extract_het_var(tmb_2))
})


test_that('extract_het_var.glmTMB can do variance scale', {
  initsd  = extract_het_var(tmb_diag, scale = 'sd')$sd
  initvar = extract_het_var(tmb_diag, scale = 'var')$variance


  expect_equal(round(initvar, 2), round(initsd^2, 2))
})

test_that('extract_het_var.glmTMB gives correct result', {
  inittmb   = attr(VarCorr(tmb_diag)$cond$group, 'stddev')^2 + glmmTMB::sigma(tmb_diag)^2
  initmixed = extract_het_var(tmb_diag, scale = 'var')

  expect_equal(round(data.frame(inittmb)$inittmb, 2), round(initmixed$variance, 2), tolerance = .01)
})



# brms -----------------------------------------------------------------

context('test extract_het_var.brmsfit')

test_that('extract_het_var.brmsfit returns a data.frame', {
  expect_s3_class(extract_het_var(brm_sigma_simple), 'data.frame')
})


test_that('extract_het_var.lme errors with no varstruct', {
  expect_error(extract_het_var(brm_glm))
})


test_that('extract_het_var.brmsfit returns appropriate values', {
  expect_equal(extract_het_var(brm_sigma_simple)$group, c('treat', 'placebo'))
})

test_that('extract_het_var.brmsfit errors with wrong ci_level', {
  expect_error(extract_het_var(brm_sigma_simple, ci_level = 2))
})

test_that('extract_het_var.brmsfit can return all fits', {
  expect_equal(
    nrow(extract_het_var(brm_sigma_simple, return_all = TRUE)),
    60
  )
})

test_that('extract_het_var.brmsfit can do variance scale', {
  initsd  = extract_het_var(brm_sigma_simple, scale = 'sd')
  initvar = extract_het_var(brm_sigma_simple, scale = 'var')

  expect_equal(round(initvar$value, 2), round(initsd$value^2, 2))
})

test_that('extract_het_var.brmsfit takes ci_level', {
  cn = colnames(extract_het_var(brm_sigma, ci_level = .8))
  expect_identical(
    c("lower_10", "upper_90"),
    grep(cn, pattern = '[0-9]+', value =T))
})
