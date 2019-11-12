context('test extract_random_coef')


# Overall -----------------------------------------------------------------

test_that('extract_random_coef errors with wrong type of model', {
  mod = lm(mpg ~ vs, mtcars)
  expect_error(extract_random_coef(mod))
})


# lme4 --------------------------------------------------------------------

context('test extract_random_coef.merMod')

test_that('extract_random_coef.merMod basic functionality', {
  expect_s3_class(extract_random_coef(lmer_1), 'data.frame')
})

test_that('extract_random_coef.merMod basic functionality', {
  expect_s3_class(extract_random_coef(lmer_2), 'data.frame')
})

test_that('extract_random_coef.merMod basic functionality', {
  expect_s3_class(extract_random_coef(lmer_4), 'data.frame')
})

test_that('extract_random_coef.merMod basic functionality', {
  expect_s3_class(extract_random_coef(glmer_1), 'data.frame')
})

test_that('extract_random_coef.merMod correct output', {
  expect_equal(
    nrow(extract_random_coef(lmer_2)),
    nlevels(sleepstudy$Subject)*2
  )
})

test_that('extract_random_coef.merMod takes re', {
  expect_equal(
    nrow(extract_random_coef(lmer_4, re = 'dept')),
    nlevels(lmer_4@frame$dept)
  )
})

test_that('extract_random_coef.merMod takes ci_level', {
  cn = colnames(extract_random_coef(lmer_1, ci_level = .8))
  expect_identical(
    c("lower_10", "upper_90"),
    grep(cn, pattern = '[0-9]+', value =T))
})


# glmmTMB -----------------------------------------------------------------

context('test extract_random_coef.glmmTMB')

test_that('extract_random_coef.glmmTMB basic functionality', {
  expect_s3_class(extract_random_coef(tmb_1), 'data.frame')
})

test_that('extract_random_coef.glmmTMB basic functionality', {
  expect_s3_class(extract_random_coef(tmb_2), 'data.frame')
})

test_that('extract_random_coef.glmmTMB basic functionality', {
  expect_s3_class(extract_random_coef(tmb_3), 'data.frame')
})


test_that('extract_random_coef.glmmTMB correct output', {
  expect_equal(
    nrow(extract_random_coef(tmb_2)),
    nlevels(sleepstudy$Subject)*2
  )
})

test_that('extract_random_coef.glmmTMB warns if interval problem', {
  expect_warning(extract_random_coef(tmb_4, re = 'dept'))
})

test_that('extract_random_coef.glmmTMB takes re', {
  expect_equal(
    nrow(suppressWarnings(extract_random_coef(tmb_4, re = 'dept'))),
    nlevels(tmb_4$frame$dept)
  )
})

test_that('extract_random_coef.glmmTMB takes ci_level', {
  cn = colnames(extract_random_coef(tmb_1, ci_level = .8))
  expect_identical(
    c("lower_10", "upper_90"),
    grep(cn, pattern = '[0-9]+', value =T))
})


test_that('extract_random_coef.glmmTMB takes component', {
  expect_equal(
    sort(levels(extract_random_coef(tmb_zip, component = 'zi')$group)),
    sort(levels(tmb_zip$frame$site))
  )
})




# nlme --------------------------------------------------------------------


context('test extract_random_coef.lme')

test_that('extract_random_coef basic functionality', {
  expect_s3_class(extract_random_coef(lme_1), 'data.frame')
})

test_that('extract_random_coef basic functionality', {
  expect_s3_class(extract_random_coef(lme_2), 'data.frame')
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
