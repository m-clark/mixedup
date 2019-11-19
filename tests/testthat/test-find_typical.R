context('test find_typical')



# Overall -----------------------------------------------------------------

test_that('find_typical fails with nonsensical probs', {
  expect_error(find_typical(lmer_1, probs = c(1,2)))
})

# lme4 --------------------------------------------------------------------

context('test find_typical.merMod')


test_that('find_typical.merMod basic functionality', {
  expect_s3_class(find_typical(lmer_1), 'data.frame')
})

test_that('find_typical.merMod basic functionality', {
  expect_s3_class(find_typical(lmer_2), 'data.frame')
})

test_that('find_typical.merMod basic functionality', {
  expect_s3_class(find_typical(lmer_3, re = 's'), 'data.frame')
})

test_that('find_typical.merMod probs', {
  # should have two results each for intercept and Days
  expect_equal(nrow(find_typical(lmer_2, re = 'Subject', probs = c(.25, .75))), 4)
})



# glmmTMB -----------------------------------------------------------------

context('test find_typical.glmmTMB')

test_that('find_typical.merMod basic functionality', {
  expect_s3_class(find_typical(tmb_1, re = 'Subject'), 'data.frame')
})

test_that('find_typical.merMod basic functionality', {
  expect_s3_class(find_typical(tmb_2, re = 'Subject'), 'data.frame')
})

test_that('find_typical.merMod basic functionality', {
  expect_s3_class(find_typical(tmb_3, re = 's'), 'data.frame')
})

test_that('find_typical.merMod probs', {
  # should have two results each for intercept and Days
  expect_equal(nrow(find_typical(tmb_2, re = 'Subject', probs = c(.25, .75))), 4)
})

# nlme --------------------------------------------------------------------

context('test find_typical.lme')

test_that('find_typical.merMod basic functionality', {
  expect_s3_class(find_typical(lme_1, re = 'Subject'), 'data.frame')
})

test_that('find_typical.merMod basic functionality', {
  expect_s3_class(find_typical(lme_2, re = 'Subject'), 'data.frame')
})

test_that('find_typical.merMod basic functionality', {
  expect_s3_class(find_typical(lme_3, re = 's'), 'data.frame')
})

test_that('find_typical.merMod probs', {
  # should have two results each for intercept and Days
  expect_equal(nrow(find_typical(lme_2, re = 'Subject', probs = c(.25, .75))), 4)
})

# brms --------------------------------------------------------------------

context('test find_typical.brm')

library(brms)

# brms results are autoloaded

test_that('find_typicalbrmsfit basic functionality', {
  expect_s3_class(find_typical(brm_1), 'data.frame')
})

test_that('find_typicalbrmsfit basic functionality', {
  expect_equal(dplyr::n_distinct(find_typical(brm_2)$effect), 2)
})

test_that('find_typicalbrmsfit basic functionality', {
  expect_s3_class(find_typical(brm_3, re = 's'), 'data.frame')
})

test_that('find_typicalbrmsfit probs', {
  # should have two results each for intercept and Days
  expect_equal(nrow(find_typical(brm_2, re = 'Subject', probs = c(.25, .75))), 4)
})


# rstanarm ----------------------------------------------------------------


context('test find_typical.stanreg')


test_that('find_typical.stanreg basic functionality', {
  expect_s3_class(find_typical(stan_glmer_1), 'data.frame')
})

test_that('find_typical.stanreg basic functionality', {
  expect_s3_class(find_typical(stan_glmer_2), 'data.frame')
})

test_that('find_typical.stanreg basic functionality', {
  expect_s3_class(find_typical(stan_glmer_3, re = 's'), 'data.frame')
})

test_that('find_typical.stanreg probs', {
  # should have two results each for intercept and Days
  expect_equal(nrow(find_typical(stan_glmer_2, re = 'Subject', probs = c(.25, .75))), 4)
})


# mgcv --------------------------------------------------------------------

context('test find_typical.gam')

library(mgcv)

test_that('find_typicalgamsfit basic functionality', {
  expect_s3_class(find_typical(gam_1), 'data.frame')
})

test_that('find_typicalgamsfit basic functionality', {
  expect_equal(dplyr::n_distinct(find_typical(gam_2)$effect), 2)
})

test_that('find_typicalgamsfit basic functionality', {
  expect_s3_class(find_typical(gam_3, re = 's'), 'data.frame')
})

test_that('find_typicalgamsfit probs', {
  # should have two results each for intercept and Days
  expect_equal(nrow(find_typical(gam_2, re = 'Subject', probs = c(.25, .75))), 4)
})
