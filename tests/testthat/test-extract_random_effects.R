context('test extract_random_effects')



# Overall -----------------------------------------------------------------

test_that('extract_random_effects errors with wrong type of model', {
  mod = lm(mpg ~ vs, mtcars)
  expect_error(extract_random_effects(mod))
})


# lme4 --------------------------------------------------------------------


library(lme4)

lmer_1 <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
lmer_2 <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
lmer_3 <- lmer(y ~ service + (1 | s) + (1 | d), data = InstEval[1:1000, ])




context('test extract_random_effects.merMod')

test_that('extract_random_effects.merMod basic functionality', {
  expect_s3_class(extract_random_effects(lmer_1, re = 'Subject'), 'data.frame')
})

test_that('extract_random_effects.merMod basic functionality', {
  expect_s3_class(extract_random_effects(lmer_2, re = 'Subject'), 'data.frame')
})

test_that('extract_random_effects.merMod correct output', {
  expect_equal(
    nrow(extract_random_effects(lmer_2, re = 'Subject')),
    nlevels(sleepstudy$Subject)
  )
})

test_that('extract_random_effects.merMod warns with no group input', {
  expect_warning(extract_random_effects(lmer_1))
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

library(glmmTMB)

tmb_1 <- glmmTMB(Reaction ~ Days + (1 | Subject), data = sleepstudy)
tmb_2 <- glmmTMB(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
tmb_3 <- glmmTMB(y ~ service + (1 | s) + (1 | d), data = InstEval[1:1000, ])
tmb_zip <- glmmTMB(
  count ~ spp + mined + (1 | site),
  zi =  ~ spp + mined + (1 | site),
  family = truncated_poisson, Salamanders
)

test_that('extract_random_effects basic functionality', {
  expect_s3_class(extract_random_effects(tmb_1, re = 'Subject'), 'data.frame')
})

test_that('extract_random_effects basic functionality', {
  expect_s3_class(extract_random_effects(tmb_2, re = 'Subject'), 'data.frame')
})

test_that('extract_random_effects correct output', {
  expect_equal(
    nrow(extract_random_effects(tmb_2, re = 'Subject')),
    nlevels(sleepstudy$Subject)
  )
})

test_that('extract_random_effects warns with no group input', {
  expect_warning(extract_random_effects(tmb_1))
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

library(nlme)

lme_1 <- lme(Reaction ~ Days, random = ~ 1 | Subject, data = sleepstudy)
lme_2 <- lme(Reaction ~ Days, random = ~ 1 + Days | Subject, data = sleepstudy)
lme_3 <- lme(y ~ service, random = list(s = ~ 1, d = ~ 1), data = InstEval[1:1000, ])

nlme_1 <-  nlme(height ~ SSasymp(age, Asym, R0, lrc),
                data = Loblolly,
                fixed = Asym + R0 + lrc ~ 1,
                random = Asym ~ 1,
                start = c(Asym = 103, R0 = -8.5, lrc = -3.3))

context('test extract_random_effects.lme')

test_that('extract_random_effects.lme basic functionality', {
  expect_s3_class(extract_random_effects(lme_1, re = 'Subject'), 'data.frame')
})

test_that('extract_random_effects.lme basic functionality', {
  expect_s3_class(extract_random_effects(lme_2, re = 'Subject'), 'data.frame')
})

test_that('extract_random_effects.lme correct output', {
  expect_equal(
    nrow(extract_random_effects(lme_2, re = 'Subject')),
    nlevels(sleepstudy$Subject)
  )
})

test_that('extract_random_effects.lme warns with no group input', {
  expect_warning(extract_random_effects(lme_1))
})

test_that('extract_random_effects.lme works with multiple re', {
  expect_equal(
    nrow(extract_random_effects(lme_3, re = 's')),
    nlevels(droplevels(lme_3$data)$s)
  )
})

test_that('extract_random_effects.lme errors with bad re name', {
  expect_error(extract_random_effects(lme_2, re = 'subject'))
})


test_that('extract_random_effects.lme works with nlme', {
  expect_equal(
    nrow(extract_random_effects(nlme_1, re = 'Seed')),
    nlevels(Loblolly$Seed)
  )
})
