context('test extract_vc')

# Test data ---------------------------------------------------------------

library(lme4)

lmer_1 <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
lmer_2 <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
lmer_3 <- lmer(y ~ service + (1 | s) + (1 | d), data = InstEval[1:1000, ])
# nonsensically complex models
lmer_4 <- lmer(y ~ service + (1 + as.numeric(lectage)| s) + (1 + as.numeric(studage)| d) + (1|dept), data = InstEval[1:5000, ])
lmer_5 <- lmer(y ~ service + (1 + as.numeric(lectage) + as.numeric(studage) + service| d) , data = InstEval[1:5000, ])
#
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
#
#


# Test lme4 ---------------------------------------------------------------


test_that('extract_vc basic functionality: random intercept only', {
  expect_s3_class(extract_vc(lmer_1, ci_level = 0), 'data.frame')
})
test_that('extract_vc basic functionality: random slopes', {
  expect_s3_class(extract_vc(lmer_2, ci_level = 0), 'data.frame')
})

test_that('extract_vc basic functionality: multiple grouping factors', {
  expect_s3_class(extract_vc(lmer_3, ci_level = 0), 'data.frame')
})
test_that('extract_vc basic functionality: ints/slopes with multiple grouping factors', {
  expect_s3_class(extract_vc(lmer_4, ci_level = 0), 'data.frame')
})

test_that('extract_vc returns correlation', {
  init = extract_vc(lmer_2, ci_level = 0, show_cor = TRUE)$Cor[[1]]
  expect_equal(dim(init),
               c(2, 2))
})

test_that('extract_vc returns correlation', {
  init = extract_vc(lmer_4, ci_level = 0, show_cor = TRUE)$Cor

  expect_type(init, 'list')

  dims = lapply(init, dim)

  expect_equal(dims$d, c(2, 2))
  expect_equal(dims$s, c(2, 2))
  expect_equal(dims$dept, c(1, 1))
})

test_that('extract_vc errors with wrong model', {
  mod = lm(mpg ~ vs, mtcars)
  expect_error(extract_vc(mod))
})

test_that('extract_vc errors with wrong ci_level', {
  mod = lm(mpg ~ vs, mtcars)
  expect_error(extract_vc(lmer_1, ci_level = 2))
})

test_that('extract_vc errors with wrong ci_scale', {

  expect_error(extract_vc(lmer_1, ci_scale = 'varience'))
})

test_that('extract_vc works with ci_scale = var', {

  expect_type(extract_vc(lmer_1, ci_scale = 'var')$var_2.5, 'double')
  expect_type(extract_vc(lmer_1, ci_scale = 'sd')$sd_2.5, 'double')
})













# Test glmmTMB ------------------------------------------------------------



