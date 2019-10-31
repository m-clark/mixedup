context('test extract_vc')

# Test data ---------------------------------------------------------------

library(lme4)

lmer_1 <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
lmer_2 <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
lmer_3 <- lmer(y ~ service + (1 | s) + (1 | d), data = InstEval[1:1000, ])
# unusually complex models just for testing
lmer_4 <- lmer(y ~ service + (1 + as.numeric(lectage)| s) + (1 + as.numeric(studage)| d) + (1|dept), data = InstEval[1:5000, ])
lmer_5 <- lmer(y ~ service + (1 + as.numeric(lectage) + as.numeric(studage) + service| d) , data = InstEval[1:5000, ])


# Test lme4 ---------------------------------------------------------------

context('test extract_vc.merMod')

test_that('extract_vc.merMod basic functionality: random intercept only', {
  expect_s3_class(extract_vc(lmer_1, ci_level = 0), 'data.frame')
})
test_that('extract_vc.merMod basic functionality: random slopes', {
  expect_s3_class(extract_vc(lmer_2, ci_level = 0), 'data.frame')
})

test_that('extract_vc.merMod basic functionality: multiple grouping factors', {
  expect_s3_class(extract_vc(lmer_3, ci_level = 0), 'data.frame')
})
test_that('extract_vc.merMod basic functionality: ints/slopes with multiple grouping factors', {
  expect_s3_class(extract_vc(lmer_4, ci_level = 0), 'data.frame')
})

test_that('extract_vc.merMod returns correlation', {
  init = extract_vc(lmer_2, ci_level = 0, show_cor = TRUE)$Cor[[1]]
  expect_equal(dim(init),
               c(2, 2))
})

test_that('extract_vc.merMod returns correlation', {
  init = extract_vc(lmer_4, ci_level = 0, show_cor = TRUE)$Cor

  expect_type(init, 'list')

  dims = lapply(init, dim)

  expect_equal(dims$d, c(2, 2))
  expect_equal(dims$s, c(2, 2))
  expect_equal(dims$dept, c(1, 1))
})

test_that('extract_vc.merMod errors with wrong model', {
  mod = lm(mpg ~ vs, mtcars)
  expect_error(extract_vc(mod))
})

test_that('extract_vc.merMod errors with wrong ci_level', {
  mod = lm(mpg ~ vs, mtcars)
  expect_error(extract_vc(lmer_1, ci_level = 2))
})

test_that('extract_vc.merMod errors with wrong ci_scale', {

  expect_error(extract_vc(lmer_1, ci_scale = 'varience'))
})

test_that('extract_vc.merMod works with ci_scale = var', {

  expect_type(extract_vc(lmer_1, ci_scale = 'var')$var_2.5, 'double')
  expect_type(extract_vc(lmer_1, ci_scale = 'sd')$sd_2.5, 'double')
})






# Test glmmTMB ------------------------------------------------------------

context('test extract_vc.glmmTMB')

library(glmmTMB)

tmb_1 <- glmmTMB(Reaction ~ Days + (1 | Subject), data = sleepstudy)
tmb_2 <- glmmTMB(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
tmb_3 <- glmmTMB(y ~ service + (1 | s) + (1 | d), data = InstEval[1:1000, ])

suppressWarnings({
tmb_4 <- glmmTMB(y ~ service + (1 + as.numeric(lectage)| s) + (1 + as.numeric(studage)| d) + (1|dept),
                 data = InstEval[1:5000, ])
})

tmb_zip <- glmmTMB(
  count ~ spp + mined + (1 | site),
  zi =  ~ spp + mined + (1 | site),
  family = truncated_poisson,
  data = Salamanders
)


test_that('extract_vc basic functionality: random intercept only', {
  expect_s3_class(extract_vc(tmb_1, ci_level = 0), 'data.frame')
})
test_that('extract_vc basic functionality: random slopes', {
  expect_s3_class(extract_vc(tmb_2, ci_level = 0), 'data.frame')
})

test_that('extract_vc basic functionality: multiple grouping factors', {
  expect_s3_class(extract_vc(tmb_3, ci_level = 0), 'data.frame')
})
test_that('extract_vc basic functionality: ints/slopes with multiple grouping factors', {
  expect_s3_class(extract_vc(tmb_4, ci_level = 0), 'data.frame')
})

test_that('extract_vc basic functionality: ints/slopes with multiple grouping factors', {
  expect_s3_class(extract_vc(tmb_zip, component = 'zi'), 'data.frame')
})


test_that('extract_vc.glmmTMB returns correlation', {
  init = extract_vc(tmb_2, ci_level = 0, show_cor = TRUE)$Cor[[1]]
  expect_equal(dim(init), c(2, 2))
})

test_that('extract_vc.glmmTMB returns correlation', {
  init = extract_vc(tmb_4, ci_level = 0, show_cor = TRUE)$Cor

  expect_type(init, 'list')

  dims = lapply(init, dim)

  expect_equal(dims$d, c(2, 2))
  expect_equal(dims$s, c(2, 2))
  expect_equal(dims$dept, c(1, 1))
})

test_that('extract_vc.glmmTMB errors with wrong model', {
  mod = lm(mpg ~ vs, mtcars)
  expect_error(extract_vc(mod))
})

test_that('extract_vc.glmmTMB errors with wrong ci_level', {
  mod = lm(mpg ~ vs, mtcars)
  expect_error(extract_vc(tmb_1, ci_level = 2))
})

test_that('extract_vc.glmmTMB errors with wrong ci_scale', {

  expect_error(extract_vc(tmb_1, ci_scale = 'varience'))
})

test_that('extract_vc.glmmTMB works with ci_scale = var', {

  expect_type(extract_vc(tmb_1, ci_scale = 'var')$var_2.5, 'double')
  expect_type(extract_vc(tmb_1, ci_scale = 'sd')$sd_2.5, 'double')
})


# Test nlme ---------------------------------------------------------------

# library(nlme)
#
# nlme_1 <- lme(Reaction ~ Days, random = ~ 1 | Subject, data = sleepstudy)
# nlme_2 <- lme(Reaction ~ Days, random = ~ 1 + Days | Subject, data = sleepstudy)


