context('test count_grps')



# Overall -----------------------------------------------------------------

test_that('count_grps works', {
  expect_s3_class(count_grps(lmer_2, 'Subject'), 'data.frame')
})

test_that('count_grps errors on incorrect model', {
  mod = lm(mpg ~ vs, mtcars)
  expect_error(count_grps(mod, 'Subject'))
})

test_that('count_grps errors on wrong group name', {
  mod = lm(mpg ~ vs, mtcars)
  expect_error(count_grps(lmer_2, 'subject'))
})



# Specific Models ---------------------------------------------------------


# This is to test more complex/non-standard models.  Standard models and even
# some of these will be tested in `extract_random_effects`


# nlme --------------------------------------------------------------------

test_that('count_grps error for nlme', {
  # who knows why, but nlme class objects don't have a model frame.
  expect_s3_class(count_grps(nlme_1, 'Seed'), 'data.frame')
})




# glmmTMB -----------------------------------------------------------------

test_that('count_grps works tmb ar', {
  expect_s3_class(count_grps(tmb_ar_2grp, 'group2'), 'data.frame')
})

test_that('count_grps works tmb zip', {
  expect_s3_class(count_grps(tmb_zip, 'site'), 'data.frame')
})







# brms --------------------------------------------------------------------

test_that('count_grps works brms zi', {
  expect_s3_class(count_grps(brm_zi, 'group'), 'data.frame')
})

test_that('count_grps works brms mv', {
  expect_s3_class(count_grps(brm_mv, 'fosternest'), 'data.frame')
})

