context('test extract_random_effects')


# Test data ---------------------------------------------------------------

library(lme4)

lmer_1 <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
lmer_2 <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
lmer_3 <- lmer(y ~ service + (1 | s) + (1 | d), data = InstEval[1:1000, ])

# library(glmmTMB)
#
# tmb_1 <- glmmTMB(Reaction ~ Days + (1 | Subject), data = sleepstudy)
# tmb_2 <- glmmTMB(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
# tmb_3 <- glmmTMB(y ~ service + (1 | s) + (1 | d), data = InstEval[1:1000, ])
# tmb_zip <- glmmTMB(
#   count ~ spp + mined + (1 | site),
#   zi =  ~ spp + mined,
#   family = truncated_poisson, Salamanders
# )



# Overall -----------------------------------------------------------------

test_that('extract_random_effects errors with wrong type of model', {
  mod = lm(mpg ~ vs, mtcars)
  expect_error(extract_random_effects(mod))
})


# lme4 --------------------------------------------------------------------

context('test extract_random_effects.merMod')

test_that('extract_random_effects basic functionality', {
  expect_s3_class(extract_random_effects(lmer_1, re = 'Subject'), 'data.frame')
})

test_that('extract_random_effects basic functionality', {
  expect_s3_class(extract_random_effects(lmer_2, re = 'Subject'), 'data.frame')
})

test_that('extract_random_effects correct output', {
  expect_equal(
    nrow(extract_random_effects(lmer_2, re = 'Subject')),
    nlevels(sleepstudy$Subject)
  )
})

test_that('extract_random_effects warns with no group input', {
  expect_warning(extract_random_effects(lmer_1))
})

test_that('extract_random_effects works with multiple re', {
  expect_equal(
    nrow(extract_random_effects(lmer_3, re = 's')),
    nlevels(lmer_3@frame$s)
  )
})
