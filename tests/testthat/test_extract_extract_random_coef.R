context('test extract_random_coef')


# Overall -----------------------------------------------------------------

test_that('extract_random_coef errors with wrong type of model', {
  mod = lm(mpg ~ vs, mtcars)
  expect_error(extract_random_coef(mod))
})


# lme4 --------------------------------------------------------------------

context('test extract_random_coef.merMod')

test_that('extract_random_coef basic functionality', {
  expect_s3_class(extract_random_coef(lmer_1, re = 'Subject'), 'data.frame')
})

test_that('extract_random_coef basic functionality', {
  expect_s3_class(extract_random_coef(lmer_2, re = 'Subject'), 'data.frame')
})

test_that('extract_random_coef correct output', {
  expect_equal(
    nrow(extract_random_coef(lmer_2, re = 'Subject')),
    nlevels(sleepstudy$Subject)
  )
})

test_that('extract_random_coef warns with no group input', {
  expect_warning(extract_random_coef(lmer_1))
})



# glmmTMB -----------------------------------------------------------------

context('test extract_random_coef.glmmTMB')

test_that('extract_random_coef basic functionality', {
  expect_s3_class(extract_random_coef(tmb_1, re = 'Subject'), 'data.frame')
})

test_that('extract_random_coef basic functionality', {
  expect_s3_class(extract_random_coef(tmb_2, re = 'Subject'), 'data.frame')
})

test_that('extract_random_coef correct output', {
  expect_equal(
    nrow(extract_random_coef(tmb_2, re = 'Subject')),
    nlevels(sleepstudy$Subject)
  )
})

test_that('extract_random_coef warns with no group input', {
  expect_warning(extract_random_coef(tmb_1))
})



# nlme --------------------------------------------------------------------


context('test extract_random_coef.lme')

test_that('extract_random_coef basic functionality', {
  expect_s3_class(extract_random_coef(lme_1, re = 'Subject'), 'data.frame')
})

test_that('extract_random_coef basic functionality', {
  expect_s3_class(extract_random_coef(lme_2, re = 'Subject'), 'data.frame')
})

test_that('extract_random_coef basic functionality', {
  expect_s3_class(extract_random_coef(lme_3, re = 'd'), 'data.frame')
})

test_that('extract_random_coef basic functionality', {
  expect_s3_class(extract_random_coef(nlme_1, re = 'Seed'), 'data.frame')
})

test_that('extract_random_coef correct output', {
  expect_equal(
    nrow(extract_random_coef(lme_3, re = 'd')),
    nlevels(droplevels(lme_3$data$d))
  )
})
