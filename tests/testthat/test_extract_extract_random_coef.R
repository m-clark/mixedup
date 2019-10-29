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
