context('test extract_vc')

# Test data ---------------------------------------------------------------

library(lme4)

lmer_1 <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
lmer_2 <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
lmer_3 <- lmer(y ~ service + (1 | s) + (1 | d), data = InstEval[1:1000, ])
# unusually complex models just for testing
lmer_4 <- lmer(y ~ service + (1 + as.numeric(lectage)| s) + (1 + as.numeric(studage)| d) + (1|dept), data = InstEval[1:5000, ])
lmer_5 <- lmer(y ~ service + (1 + as.numeric(lectage) + as.numeric(studage) + service| d) , data = InstEval[1:5000, ])




# Test initial checks -----------------------------------------------------



test_that('extract_vc errors with wrong model', {
  mod = lm(mpg ~ vs, mtcars)
  expect_error(extract_vc(mod))
})

test_that('extract_vc errors with wrong ci_level', {
  expect_error(extract_vc(lmer_1, ci_level = 2))
})

test_that('extract_vc errors with wrong ci_scale', {
  expect_error(extract_vc(lmer_1, ci_scale = 'varience'))
})



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


test_that('extract_vc.glmmTMB works with ci_scale = var', {

  expect_type(extract_vc(tmb_1, ci_scale = 'var')$var_2.5, 'double')
  expect_type(extract_vc(tmb_1, ci_scale = 'sd')$sd_2.5, 'double')
})


# Test nlme ---------------------------------------------------------------

context('test extract_vc.lme')

library(nlme)

lme_1 <- lme(Reaction ~ Days, random = ~ 1 | Subject, data = sleepstudy)
lme_2 <- lme(Reaction ~ Days, random = ~ 1 + Days | Subject, data = sleepstudy)
lme_3 <- lme(y ~ service,
             random = list(d = ~ 1, s = ~ 1),
             data = droplevels(InstEval[1:1000, ]))
lme_4 <- lme(y ~ service,
             random = list(d = ~ 1 + service, s = ~ 1 ),
             data = droplevels(InstEval[1:3000, ]))

# won't converge
# lme_5 <- lme(y ~ service,
#              random = list(d = ~ 1 + as.numeric(lectage) + as.numeric(studage) + service) ,
#              data = droplevels(InstEval[1:5000, ]))

nlme_1 <-  nlme(height ~ SSasymp(age, Asym, R0, lrc),
                data = Loblolly,
                fixed = Asym + R0 + lrc ~ 1,
                random = Asym ~ 1,
                start = c(Asym = 103, R0 = -8.5, lrc = -3.3))

test_that('extract_vc.lme basic functionality: random intercept only', {
  expect_s3_class(extract_vc(lme_1, ci_level = 0), 'data.frame')
})

test_that('extract_vc.lme basic functionality: random slopes', {
  expect_s3_class(extract_vc(lme_2, ci_level = 0), 'data.frame')
})

test_that('extract_vc.lme basic functionality: multiple grouping factors', {
  expect_s3_class(extract_vc(lme_3, ci_level = 0), 'data.frame')
})

test_that('extract_vc.lme basic functionality: ints/slopes with multiple grouping factors', {
  expect_s3_class(extract_vc(lme_4, ci_level = 0), 'data.frame')
})

test_that('extract_vc.lme basic functionality: nlme', {
  expect_s3_class(extract_vc(nlme_1, ci_level = 0), 'data.frame')
})

test_that('extract_vc.lme returns correlation', {
  init = extract_vc(lme_2, ci_level = 0, show_cor = TRUE)$Cor[[1]]
  expect_equal(dim(init), c(2, 2))
})

test_that('extract_vc.lme returns correlation', {
  init = extract_vc(lme_4, ci_level = 0, show_cor = TRUE)$Cor

  expect_type(init, 'list')

  dims = lapply(init, dim)

  expect_equal(dims$d, c(2, 2))
  expect_equal(dims$s, c(1, 1))
})


test_that('extract_vc.lme warns with no ci', {
  expect_warning(extract_vc(lme_4))
})


# maybe change names for consistency to other objects in the future
test_that('extract_vc.lme works with ci_scale = var', {
  expect_type(extract_vc(lme_1, ci_scale = 'var')$var_2.5, 'double')
  expect_type(extract_vc(lme_1, ci_scale = 'sd')$sd_2.5, 'double')
})



# Test brms ---------------------------------------------------------------


context('test extract_vc.brmsfit')

test_that('extract_vc.brmsfit basic functionality: random intercept only', {
  expect_s3_class(extract_vc(brm_1), 'data.frame')
})

test_that('extract_vc.brmsfit basic functionality: random slopes', {
  expect_s3_class(extract_vc(brm_2), 'data.frame')
})

test_that('extract_vc.brmsfit basic functionality: multiple grouping factors', {
  expect_s3_class(extract_vc(brm_3), 'data.frame')
})

test_that('extract_vc.brmsfit basic functionality: ints/slopes with multiple grouping factors', {
  expect_equivalent(unique(extract_vc(brm_4)$group), c('continent', 'country', 'Residual'))
  expect_equivalent(unique(extract_vc(brm_4)$coefficient), c('Intercept', 'year', ''))
})

test_that('extract_vc.brmsfit returns correlation', {
  init = extract_vc(brm_2, ci_level = 0, show_cor = TRUE)$Cor[[1]]
  expect_equal(dim(init),
               c(2, 2))
})

test_that('extract_vc.brmsfit returns correlation', {
  init = extract_vc(brm_4, ci_level = 0, show_cor = TRUE)$Cor

  expect_type(init, 'list')

  dims = lapply(init, dim)

  expect_equal(dims$continent, c(2, 2))
  expect_equal(dims$country, c(2, 2))
})


test_that('extract_vc.brmsfit works with ci_scale = var', {

  expect_type(extract_vc(brm_1, ci_scale = 'var')$var_2.5, 'double')
  expect_type(extract_vc(brm_1, ci_scale = 'sd')$sd_2.5, 'double')
})


test_that('extract_vc.brmsfit basic functionality: non-gaussian', {
  expect_equal(nrow(extract_vc(brm_glm)), 1)  # no residual var
})
