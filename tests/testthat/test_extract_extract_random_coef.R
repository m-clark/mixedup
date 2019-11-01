context('test extract_random_coef')


# Test data ---------------------------------------------------------------

library(lme4)

lmer_1 <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
lmer_2 <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
lmer_3 <- lmer(y ~ service + (1 | s) + (1 | d), data = InstEval[1:1000, ])

library(glmmTMB)

tmb_1 <- glmmTMB(Reaction ~ Days + (1 | Subject), data = sleepstudy)
tmb_2 <- glmmTMB(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
tmb_3 <- glmmTMB(y ~ service + (1 | s) + (1 | d), data = InstEval[1:1000, ])
tmb_zip <- glmmTMB(
  count ~ spp + mined + (1 | site),
  zi =  ~ spp + mined,
  family = truncated_poisson, Salamanders
)



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

library(nlme)

lme_1 <- lme(Reaction ~ Days, random = ~ 1 | Subject, data = sleepstudy)
lme_2 <- lme(Reaction ~ Days, random = ~ 1 + Days | Subject, data = sleepstudy)
lme_3 <- lme(y ~ service, random = list(s = ~ 1, d = ~ 1), data = InstEval[1:1000, ])

nlme_1 <-  nlme(height ~ SSasymp(age, Asym, R0, lrc),
                data = Loblolly,
                fixed = Asym + R0 + lrc ~ 1,
                random = Asym ~ 1,
                start = c(Asym = 103, R0 = -8.5, lrc = -3.3))


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
    nrow(extract_random_coef(lme_2, re = 'Subject')),
    nlevels(sleepstudy$Subject)
  )
})

test_that('extract_random_coef warns with no group input', {
  expect_warning(extract_random_coef(lme_1))
})
