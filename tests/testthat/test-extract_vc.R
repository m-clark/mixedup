context('test extract_vc')


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


test_that('extract_vc.merMod basic functionality: correct results', {
  raw_output = c(attr(VarCorr(lmer_1)[[1]], 'stddev'), attr(VarCorr(lmer_1), 'sc'))
  names(raw_output) = NULL
  expect_equal(extract_vc(lmer_1, ci_level = 0, digits = 10)$sd, raw_output)
})

test_that('extract_vc.merMod basic functionality: correct results', {
  raw_output = c(attr(VarCorr(lmer_2)[[1]], 'stddev'), attr(VarCorr(lmer_2), 'sc'))
  names(raw_output) = NULL
  expect_equal(extract_vc(lmer_2, ci_level = 0, digits = 10)$sd, raw_output)
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


test_that('extract_vc.merMod basic functionality: correct results', {
  raw_output = c(attr(VarCorr(tmb_1)[['cond']][[1]], 'stddev'), attr(VarCorr(tmb_1)[['cond']], 'sc'))
  names(raw_output) = NULL
  expect_equal(extract_vc(tmb_1, ci_level = 0, digits = 10)$sd, raw_output)
})

test_that('extract_vc.merMod basic functionality: correct results', {
  raw_output = c(attr(VarCorr(tmb_2)[['cond']][[1]], 'stddev'), attr(VarCorr(tmb_2)[['cond']], 'sc'))
  names(raw_output) = NULL
  expect_equal(extract_vc(tmb_2, ci_level = 0, digits = 10)$sd, raw_output)
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

test_that('extract_vc.merMod basic functionality: correct results', {
  raw_output = as.numeric(VarCorr(lme_1)[,'Variance'])
  expect_equal(extract_vc(lme_1, ci_level = 0, digits = 10)$variance, raw_output)
})

test_that('extract_vc.merMod basic functionality: correct results', {
  raw_output = as.numeric(VarCorr(lme_2)[,'Variance'])
  expect_equal(extract_vc(lme_2, ci_level = 0, digits = 10)$variance, raw_output)
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
  expect_equivalent(unique(extract_vc(brm_4)$effect), c('Intercept', 'year', ''))
})

test_that('extract_vc.merMod basic functionality: correct results', {
  raw_output = purrr::map_dbl(VarCorr(brm_1), function(x) x[[1]][,'Estimate'])
  names(raw_output) = NULL
  expect_equal(extract_vc(brm_1, ci_level = 0, digits = 10)$sd, raw_output)
})

test_that('extract_vc.merMod basic functionality: correct results', {
  raw_output = c(VarCorr(brm_2)$Subject$sd[,'Estimate'], VarCorr(brm_2)$residual__$sd[,'Estimate'])
  names(raw_output) = NULL
  expect_equal(extract_vc(brm_2, ci_level = 0, digits = 10)$sd, raw_output)
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
