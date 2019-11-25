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

test_that('extract_vc.merMod basic functionality: random intercept only with no residual', {
  init = extract_vc(glmer_1, ci_args = list(method = 'Wald'))
  expect_equal(nrow(init), 1)

  # check that ci columns are appropriately attached
  expect_true(any(grepl(colnames(init), pattern = 'sd\\_2\\.5')))
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

test_that('extract_vc.merMod basic functionality: correct results', {
  init = data.frame(VarCorr(lmer_4))
  init = init[is.na(init$var2), ]
  raw_output = init[,'vcov']
  expect_equal(extract_vc(lmer_4, ci_level = 0, digits = 10)$variance, raw_output)
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

# test_that('extract_vc.merMod works with ci fail', {
  # find some model that will fail
  # d = data.frame(y = rnorm(50), g1 = sample(1:5, 50, replace = T), g2 = sample(1:2, 50, replace = T))
  #
  # confint(lmer(y ~ (1|g1) + (1|g2), data=d[-sample(1:50, 10),]), method = 'Wald')
  # mod = lmer(y ~ (1|s) + (1|d) + (1|dept), data = InstEval[1:500,])
  #
  # expect_(extract_vc(mod))
# })




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


# Test mgcv ---------------------------------------------------------------

context('test extract_vc.gam')

test_that('Works on single lme model', {
  expect_s3_class(extract_vc(gam_1), 'data.frame')
})


test_that('extract_vc.gam basic functionality: random slopes', {
  expect_s3_class(extract_vc(gam_2), 'data.frame')
})

test_that('extract_vc.gam basic functionality: multiple grouping factors', {
  expect_s3_class(extract_vc(gam_3), 'data.frame')
})

test_that('extract_vc.gam basic functionality: glmm', {
  expect_true(any(grepl(colnames(extract_vc(gam_glm)), pattern = 'sd\\_2\\.5')))
})

test_that('extract_vc.gam basic functionality: bam', {
  expect_s3_class(extract_vc(bam_1), 'data.frame')
})


test_that('extract_vc.gam fails with non-REML', {
  ga_model_noREML = mgcv::gam(Reaction ~  Days + s(Subject, bs='re') + s(Days, Subject, bs='re'),
                              data = lme4::sleepstudy)
  expect_error(extract_vc(ga_model_noREML))
})


test_that('extract_vc.gam basic functionality: correct results', {
  invisible(
    utils::capture.output(
      raw_output <- mgcv::gam.vcomp(gam_1)[,'std.dev']
    )
  )
  names(raw_output) = NULL
  expect_equal(extract_vc(gam_1, digits = 10)$sd, raw_output)
})

test_that('extract_vc.gam basic functionality: correct results', {
  invisible(
    utils::capture.output(
      raw_output <- mgcv::gam.vcomp(gam_2)[,'std.dev']
    )
  )
  names(raw_output) = NULL
  expect_equal(extract_vc(gam_2, digits = 10)$sd, raw_output)
})

test_that('extract_vc.gam works with ci_scale = var and alternate ci_level', {
  invisible(
    utils::capture.output(
      raw_output <- mgcv::gam.vcomp(gam_2, conf.lev = .9)[,'upper']
    )
  )
  names(raw_output) = NULL
  expect_equal(extract_vc(gam_2, ci_level = .9, digits = 10)$sd_95, raw_output)
})


test_that('extract_vc.gam works with ci_scale = var', {
  expect_type(extract_vc(gam_1, ci_scale = 'var')$var_2.5, 'double')
  expect_type(extract_vc(gam_1, ci_scale = 'sd')$sd_2.5, 'double')
})



