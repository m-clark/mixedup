context('test extract_random_coefs')


# Overall -----------------------------------------------------------------

test_that('extract_random_coefs errors with wrong type of model', {
  mod = lm(mpg ~ vs, mtcars)
  expect_error(extract_random_coefs(mod))
})


# lme4 --------------------------------------------------------------------

context('test extract_random_coefs.merMod')

test_that('extract_random_coefs.merMod basic functionality', {
  expect_s3_class(extract_random_coefs(lmer_1), 'data.frame')
})

test_that('extract_random_coefs.merMod basic functionality', {
  expect_s3_class(extract_random_coefs(lmer_2), 'data.frame')
})

test_that('extract_random_coefs.merMod basic functionality', {
  expect_s3_class(extract_random_coefs(lmer_4), 'data.frame')
})

test_that('extract_random_coefs.merMod basic functionality', {
  expect_s3_class(extract_random_coefs(glmer_1), 'data.frame')
})

test_that('extract_random_coefs.merMod correct output', {
  expect_equal(
    nrow(extract_random_coefs(lmer_2)),
    nlevels(sleepstudy$Subject)*2
  )
})

test_that('extract_random_coefs.merMod takes re', {
  expect_equal(
    nrow(extract_random_coefs(lmer_4, re = 'dept')),
    nlevels(lmer_4@frame$dept)
  )
})

test_that('extract_random_coefs.merMod takes ci_level', {
  cn = colnames(extract_random_coefs(lmer_1, ci_level = .8))
  expect_identical(
    c("lower_10", "upper_90"),
    grep(cn, pattern = '[0-9]+', value =T))
})


# glmmTMB -----------------------------------------------------------------

context('test extract_random_coefs.glmmTMB')

test_that('extract_random_coefs.glmmTMB basic functionality', {
  expect_s3_class(extract_random_coefs(tmb_1), 'data.frame')
})

test_that('extract_random_coefs.glmmTMB basic functionality', {
  expect_s3_class(extract_random_coefs(tmb_2), 'data.frame')
})

test_that('extract_random_coefs.glmmTMB basic functionality', {
  expect_s3_class(extract_random_coefs(tmb_3), 'data.frame')
})


test_that('extract_random_coefs.glmmTMB correct output', {
  expect_equal(
    nrow(extract_random_coefs(tmb_2)),
    nlevels(sleepstudy$Subject)*2
  )
})

test_that('extract_random_coefs.glmmTMB warns if interval problem', {
  expect_warning(extract_random_coefs(tmb_4, re = 'dept'))
})

test_that('extract_random_coefs.glmmTMB takes re', {
  expect_equal(
    nrow(suppressWarnings(extract_random_coefs(tmb_4, re = 'dept'))),
    nlevels(tmb_4$frame$dept)
  )
})

test_that('extract_random_coefs.glmmTMB takes ci_level', {
  cn = colnames(extract_random_coefs(tmb_1, ci_level = .8))
  expect_identical(
    c("lower_10", "upper_90"),
    grep(cn, pattern = '[0-9]+', value =T))
})


test_that('extract_random_coefs.glmmTMB takes component', {
  expect_equal(
    sort(levels(extract_random_coefs(tmb_zip, component = 'zi')$group)),
    sort(levels(tmb_zip$frame$site))
  )
})




# nlme --------------------------------------------------------------------


context('test extract_random_coefs.lme')

test_that('extract_random_coefs basic functionality', {
  expect_s3_class(extract_random_coefs(lme_1), 'data.frame')
})

test_that('extract_random_coefs basic functionality', {
  expect_s3_class(extract_random_coefs(lme_2), 'data.frame')
})

test_that('extract_random_coefs basic functionality', {
  expect_s3_class(extract_random_coefs(lme_3, re = 'd'), 'data.frame')
})

test_that('extract_random_coefs basic functionality', {
  expect_s3_class(extract_random_coefs(nlme_1, re = 'Seed'), 'data.frame')
})

test_that('extract_random_coefs correct output', {
  expect_equal(
    nrow(extract_random_coefs(lme_3, re = 'd')),
    nlevels(droplevels(lme_3$data$d))
  )
})


# brms --------------------------------------------------------------------

context('test extract_random_coefbrmsfit')

test_that('extract_random_coefbrmsfit basic functionality', {
  expect_s3_class(extract_random_coefs(brm_1), 'data.frame')
})

test_that('extract_random_coefbrmsfit basic functionality', {
  expect_s3_class(extract_random_coefs(brm_2), 'data.frame')
})

test_that('extract_random_coefbrmsfit basic functionality', {
  expect_s3_class(extract_random_coefs(brm_4), 'data.frame')
})

test_that('extract_random_coefbrmsfit basic functionality', {
  expect_s3_class(extract_random_coefs(brm_1), 'data.frame')
})

test_that('extract_random_coefbrmsfit correct output', {
  expect_equal(
    nrow(extract_random_coefs(brm_2)),
    nlevels(sleepstudy$Subject)*2
  )
})

test_that('extract_random_coefbrmsfit takes re', {
  expect_equal(
    nrow(extract_random_coefs(brm_4, re = 'continent')),
    nlevels(factor(brm_4$data$continent))*2
  )
})

test_that('extract_random_coefbrmsfit takes ci_level', {
  cn = colnames(extract_random_coefs(brm_1, ci_level = .8))
  expect_identical(
    c("lower_10", "upper_90"),
    grep(cn, pattern = '[0-9]+', value =T))
})
