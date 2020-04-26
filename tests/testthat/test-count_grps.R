context('test count_grps')



# Overall -----------------------------------------------------------------

test_that('count_grps works', {
  expect_s3_class(mixedup:::count_grps(lmer_2, 'Subject'), 'data.frame')
})



# Specific Models ---------------------------------------------------------


test_that('count_grps lme4 multiple effects', {
  expect_s3_class(extract_random_effects(lmer_4, add_group_N = TRUE), 'data.frame')
})

test_that('count_grps glmmTMB for zip', {
  expect_s3_class(extract_random_effects(tmb_zip, add_group_N = TRUE), 'data.frame')
})
