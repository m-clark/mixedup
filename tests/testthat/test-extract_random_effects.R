context('test extract_random_effects')



# Overall -----------------------------------------------------------------

test_that('extract_random_effects errors with wrong type of model', {
  mod = lm(mpg ~ vs, mtcars)
  expect_error(extract_random_effects(mod))
})

test_that('extract_random_effects errors with silly ci level', {
  expect_error(extract_random_effects(lmer_1, ci_level = 2))
})

# lme4 --------------------------------------------------------------------


context('test extract_random_effects.merMod')

test_that("lme4 installation is checked", {
  with_mock(
    'mixedup::is_package_installed' = function() FALSE,
    expect_error(extract_random_effects(lmer_1))
  )
})

test_that('extract_random_effects.merMod basic functionality', {
  expect_s3_class(extract_random_effects(lmer_1), 'data.frame')
})

test_that('extract_random_effects.merMod basic functionality', {
  expect_s3_class(extract_random_effects(lmer_2), 'data.frame')
})


test_that('extract_random_effects.merMod works with multiple re', {
  expect_equal(
    nrow(extract_random_effects(lmer_3, re = 's')),
    nlevels(lmer_3@frame$s)
  )
})

test_that('extract_random_effects.merMod errors with bad re name', {
  expect_error(extract_random_effects(lmer_2, re = 'subject'))
})

# glmmTMB -----------------------------------------------------------------

context('test extract_random_effects.glmmTMB')

test_that("glmmTMB installation is checked", {
  with_mock(
    'mixedup::is_package_installed' = function() FALSE,
    expect_error(extract_random_effects(tmb_1, re = 'Subject'))
  )
})

test_that('extract_random_effects basic functionality', {
  expect_s3_class(extract_random_effects(tmb_1, re = 'Subject'), 'data.frame')
})

test_that('extract_random_effects basic functionality', {
  expect_s3_class(extract_random_effects(tmb_2, re = 'Subject'), 'data.frame')
})

test_that('extract_random_effects correct output', {
  expect_equal(
    nrow(extract_random_effects(tmb_4, re = 'dept')),
    nlevels(tmb_4$frame$dept)
  )
})


test_that('extract_random_effects can do zip', {
  expect_s3_class(
    extract_random_effects(tmb_zip, re = 'site', component = 'zi'),
    'data.frame'
    )
})

test_that('extract_random_effects errors with bad re name', {
  expect_error(extract_random_effects(tmb_zip, re = 'Site'))
})


# nlme --------------------------------------------------------------------


context('test extract_random_effects.lme')

# no check on package install as it is a base/recommended package

test_that('extract_random_effects.lme basic functionality', {
  expect_s3_class(extract_random_effects(lme_1), 'data.frame')
})

test_that('extract_random_effects.lme basic functionality', {
  expect_s3_class(extract_random_effects(lme_2), 'data.frame')
})


test_that('extract_random_effects.lme works with multiple re', {
  expect_equal(
    nrow(extract_random_effects(lme_3, re = 'd')),
    nlevels(droplevels(lme_3$data)$d)
  )
})

test_that('extract_random_effects.lme errors with bad re name', {
  expect_error(extract_random_effects(lme_3, re = 'subject'))
})


test_that('extract_random_effects.lme works with nlme', {
  expect_equal(
    nrow(extract_random_effects(nlme_1, re = 'Seed')),
    nlevels(Loblolly$Seed)
  )
})


# brms --------------------------------------------------------------------

context('test extract_random_effects.brmsfit')

test_that("brms installation is checked", {
  with_mock(
    'mixedup::is_package_installed' = function() FALSE,
    expect_error(extract_random_effects(brm_1, re = 'Subject'))
  )
})

test_that('extract_random_effects.brmsfit basic functionality', {
  expect_s3_class(extract_random_effects(brm_1, re = 'Subject'), 'data.frame')
})

test_that('extract_random_effects.brmsfit basic functionality', {
  expect_s3_class(extract_random_effects(brm_2, re = 'Subject'), 'data.frame')
})

test_that('extract_random_effects.brmsfit correct output', {
  expect_equal(
    nrow(extract_random_effects(brm_2, re = 'Subject')),
    nlevels(sleepstudy$Subject)*2
  )
})

test_that('extract_random_effects.brmsfit works with multiple re', {
  expect_equal(
    nrow(extract_random_effects(brm_3, re = 's')),
    nlevels(brm_3$data$s)
  )
})

test_that('extract_random_effects.brmsfit errors with bad re name', {
  expect_error(extract_random_effects(brm_2, re = 'subject'))
})

test_that('extract_random_effects.brmsfit errors with bad re name', {
  expect_error(extract_random_effects(brm_2, re = 'subject'))
})

