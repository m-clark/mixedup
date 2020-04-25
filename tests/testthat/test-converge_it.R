context('test converge_it')



# Overall -----------------------------------------------------------------

test_that('converge_it errors with wrong type of model', {
  mod = lm(mpg ~ vs, mtcars)
  expect_error(converge_it(mod))
})



# lme4 lmm ----------------------------------------------------------------

test_that('converge_it is okay with a non-problematic model', {
  expect_s4_class(converge_it(lmer_1), 'merMod')
})

test_that('converge_it is okay with a problematic but converged model', {
  expect_s4_class(converge_it(lmer_4), 'merMod')
})

test_that('converge_it works', {
  # this still won't be enough for convergence, but at least shouldn't error
  suppressWarnings({
    expect_s4_class(converge_it(lmer_conv), 'merMod')
  })
})

test_that('converge_it will stop', {
  expect_warning(converge_it(lmer_conv_ridiculous))
})

# lme4 glmm ---------------------------------------------------------------

test_that('converge_it works', {
  expect_s4_class(converge_it(glmer_conv), 'merMod')
})

test_that('converge_it stops after 10 runs', {
  expect_s4_class(converge_it(glmer_conv), 'merMod')
})
