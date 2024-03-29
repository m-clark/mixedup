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
  init <-  extract_vc(glmer_1, ci_args = list(method = 'Wald'))
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
  raw_output = c(attr(lme4::VarCorr(lmer_1)[[1]], 'stddev'), attr(lme4::VarCorr(lmer_1), 'sc'))
  names(raw_output) = NULL
  expect_equal(extract_vc(lmer_1, ci_level = 0, digits = 10)$sd, raw_output)
})

test_that('extract_vc.merMod basic functionality: correct results', {
  raw_output = c(attr(lme4::VarCorr(lmer_2)[[1]], 'stddev'), attr(lme4::VarCorr(lmer_2), 'sc'))
  names(raw_output) = NULL
  expect_equal(extract_vc(lmer_2, ci_level = 0, digits = 10)$sd, raw_output)
})

test_that('extract_vc.merMod basic functionality: correct results', {
  init = data.frame(lme4::VarCorr(lmer_4))
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
#   # find some model that will fail
#   d = data.frame(y = rnorm(50), g1 = sample(1:5, 50, replace = T), g2 = sample(1:2, 50, replace = T))
#
#   confint(lmer(y ~ (1|g1) + (1|g2), data=d[-sample(1:50, 10),]), method = 'Wald')
#   mod = lmer(y ~ (1|s) + (1|d) + (1|dept), data = InstEval[1:500,])
#
#   expect_(extract_vc(mod))
# })




# Test glmmTMB ------------------------------------------------------------

context('test extract_vc.glmmTMB')


test_that('extract_vc.glmmTMB basic functionality: random intercept only', {
  expect_s3_class(extract_vc(tmb_1, ci_level = 0), 'data.frame')
})

test_that('extract_vc.glmmTMB basic functionality: random slopes', {
  expect_s3_class(extract_vc(tmb_2, ci_level = 0), 'data.frame')
})

test_that('extract_vc.glmmTMB basic functionality: multiple grouping factors', {
  expect_s3_class(extract_vc(tmb_3, ci_level = 0), 'data.frame')
})

test_that('extract_vc.glmmTMB basic functionality: ints/slopes with multiple grouping factors', {
  expect_s3_class(extract_vc(tmb_4, ci_level = 0), 'data.frame')
})

## zi

test_that('extract_vc.glmmTMB basic functionality: zero-inflated', {
  expect_s3_class(extract_vc(tmb_zip, component = 'zi'), 'data.frame')
})

test_that('extract_vc.glmmTMB basic functionality: zero-inflated', {
  expect_message(extract_vc(tmb_zip_no_zi_re, component = 'zi'))
})

### ar and related

test_that('extract_vc.glmmTMB basic functionality: ar', {
  expect_s3_class(suppressWarnings(extract_vc(tmb_ar)), 'data.frame')
  expect_s3_class(suppressWarnings(extract_vc(tmb_ar_2grp, ci_level = F)), 'data.frame')
})

test_that('extract_vc.glmmTMB basic functionality: ar-related', {
  expect_s3_class(extract_vc(tmb_cs), 'data.frame')
  expect_s3_class(suppressWarnings(extract_vc(tmb_diag)), 'data.frame')
  expect_s3_class(suppressWarnings(extract_vc(tmb_diag_2grp)), 'data.frame')
  expect_s3_class(suppressWarnings(extract_vc(tmb_ou)), 'data.frame')
  expect_s3_class(extract_vc(tmb_toep), 'data.frame')
  expect_s3_class(suppressWarnings(extract_vc(tmb_us)), 'data.frame')
  expect_s3_class(suppressWarnings(extract_vc(tmb_gau)), 'data.frame')
  expect_s3_class(suppressWarnings(extract_vc(tmb_gau_2grp)), 'data.frame')
  expect_s3_class(suppressWarnings(extract_vc(tmb_exp)), 'data.frame')
  expect_s3_class(suppressWarnings(extract_vc(tmb_mat)), 'data.frame')
})

test_that('extract_vc.glmmTMB basic functionality: ar-related warnings', {
  expect_warning(extract_vc(tmb_diag))
  expect_warning(extract_vc(tmb_diag_2grp))
  expect_warning(extract_vc(tmb_ou))
  expect_warning(extract_vc(tmb_us))
  expect_warning(extract_vc(tmb_gau))
  expect_warning(extract_vc(tmb_gau_2grp))
  expect_warning(extract_vc(tmb_exp))
  expect_warning(extract_vc(tmb_mat))
})

test_that('extract_vc.glmmTMB basic functionality: correct results', {
  raw_output = c(attr(glmmTMB::VarCorr(tmb_1)[['cond']][[1]], 'stddev'),
                 attr(glmmTMB::VarCorr(tmb_1)[['cond']], 'sc'))
  names(raw_output) = NULL
  expect_equal(extract_vc(tmb_1, ci_level = 0, digits = 10)$sd, raw_output)
})

test_that('extract_vc.glmmTMB basic functionality: correct results', {
  raw_output = c(attr(glmmTMB::VarCorr(tmb_2)[['cond']][[1]], 'stddev'),
                 attr(glmmTMB::VarCorr(tmb_2)[['cond']], 'sc'))
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


test_that('extract_vc.glmmTMB errors with wrong component', {
  expect_error(extract_vc(tmb_disp, component = 'disp'))
})

test_that('extract_vc.glmmTMB can return heterogenous variances', {
  hv = suppressWarnings({extract_vc(tmb_diag, include_het_var = TRUE)})
  expect_type(hv, 'list')
  expect_equal(length(hv), 2)

  hv = suppressWarnings({extract_vc(tmb_diag, show_cor = TRUE, include_het_var = TRUE)})
  expect_equal(length(hv), 3) # cor is diag but still should work
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
  raw_output = as.numeric(nlme::VarCorr(lme_1)[,'Variance'])
  expect_equal(extract_vc(lme_1, ci_level = 0, digits = 10)$variance, raw_output)
})

test_that('extract_vc.merMod basic functionality: correct results', {
  raw_output = as.numeric(nlme::VarCorr(lme_2)[,'Variance'])
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


# this model apparently calculates ci now.
# test_that('extract_vc.lme warns with no ci', {
#   expect_warning(extract_vc(lme_4))
# })


# maybe change names for consistency to other objects in the future
test_that('extract_vc.lme works with ci_scale = var', {
  expect_type(extract_vc(lme_1, ci_scale = 'var')$var_2.5, 'double')
  expect_type(extract_vc(lme_1, ci_scale = 'sd')$sd_2.5, 'double')
})


test_that('extract_vc.glmmTMB can return heterogenous variances', {
  hv = extract_vc(lme_het_var, include_het_var = TRUE)
  expect_type(hv, 'list')
  expect_equal(length(hv), 2)
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
  expect_equivalent(unique(extract_vc(brm_4)$effect), c('Intercept', 'year', NA_character_))
})

test_that('extract_vc.merMod basic functionality: correct results', {
  raw_output = purrr::map_dbl(brms::VarCorr(brm_1), \(x) x[[1]][,'Estimate'])
  names(raw_output) = NULL
  expect_equal(extract_vc(brm_1, ci_level = 0, digits = 10)$sd, raw_output)
})

test_that('extract_vc.merMod basic functionality: correct results', {
  raw_output = c(brms::VarCorr(brm_2)$Subject$sd[,'Estimate'], brms::VarCorr(brm_2)$residual__$sd[,'Estimate'])
  names(raw_output) = NULL
  expect_equal(extract_vc(brm_2, ci_level = 0, digits = 10)$sd, raw_output)
})


test_that('extract_vc.brmsfit returns correlation', {
  init = extract_vc(brm_2, ci_level = 0, show_cor = TRUE)$Cor[[1]]
  expect_equal(dim(init), c(2, 2))
})

test_that('extract_vc.brmsfit returns correlation', {
  init = extract_vc(brm_4, ci_level = 0, show_cor = TRUE)$Cor

  expect_type(init, 'list')

  dims = lapply(init, dim)

  expect_equal(dims$continent, c(2, 2))
  expect_equal(dims$country, c(2, 2))
})


test_that('extract_vc.brmsfit works with ci_scale = var', {

  init  = extract_vc(brm_1, ci_scale = 'var')$var_2.5
  init2 = extract_vc(brm_1, ci_scale = 'sd')$sd_2.5


  expect_type(init, 'double')
  expect_type(init2, 'double')

  expect_lt(init2[1], init[1])
  expect_lt(init2[2], init[2])
})


test_that('extract_vc.brmsfit basic functionality: non-gaussian', {
  expect_equal(nrow(extract_vc(brm_glm)), 1)  # no residual var
})

test_that('extract_vc.brmsfit basic functionality: multivariate model', {
  init = extract_vc(brm_mv, component = 'back', ci_level = .8, digits = 2)
  expect_match(init$effect, 'back')
})

# corAR and other models will throw an error unless brms is loaded

test_that('extract_vc.brmsfit basic functionality: autocor model', {
  # require(brms)
  expect_s3_class(extract_vc(brm_corAR, ci_level = .8, digits = 2), 'data.frame')
})

test_that('extract_vc.brmsfit basic functionality: zi model', {
  init = extract_vc(brm_zi, component = 'zi', ci_level = .8, digits = 2)
  expect_match(init$effect, 'zi')
})

test_that('extract_vc.brmsfit basic functionality: sigma model', {
  init = extract_vc(brm_sigma, component = 'sigma', ci_level = .8, digits = 2)
  expect_match(init$effect, 'sigma_Intercept')
})

test_that('extract_vc.brmsfit can return heterogenous variances', {
  hv = extract_vc(brm_sigma, include_het_var = TRUE)
  expect_type(hv, 'list')
  expect_equal(length(hv), 2)

  hv = suppressWarnings({extract_vc(brm_sigma, show_cor = TRUE, include_het_var = TRUE)})
  expect_equal(length(hv), 3)
})




# Test rstanarm ---------------------------------------------------------------


context('test extract_vc.stanreg')

test_that('extract_vc.stanreg basic functionality: random intercept only', {
  expect_s3_class(extract_vc(stan_glmer_1), 'data.frame')
})

test_that('extract_vc.stanreg basic functionality: random slopes', {
  expect_s3_class(extract_vc(stan_glmer_2), 'data.frame')
})

test_that('extract_vc.stanreg basic functionality: multiple grouping factors', {
  expect_s3_class(extract_vc(stan_glmer_3), 'data.frame')
})

test_that('extract_vc.stanreg basic functionality: ints/slopes with multiple grouping factors', {
  expect_equivalent(unique(extract_vc(stan_glmer_4)$group), c('country', 'continent', 'Residual'))
  expect_equivalent(unique(extract_vc(stan_glmer_4)$effect), c('Intercept', 'year', NA_character_))
})

test_that('extract_vc.merMod basic functionality: correct results', {
  raw_output = attr(rstanarm::VarCorr(stan_glmer_1)[[1]], 'stddev')
  names(raw_output) = NULL
  expect_equal(extract_vc(stan_glmer_1, digits = 10)$sd[1], raw_output)
})

test_that('extract_vc.merMod basic functionality: correct results', {
  raw_output = attr(rstanarm::VarCorr(stan_glmer_2)[[1]], 'stddev')
  names(raw_output) = NULL
  expect_equal(extract_vc(stan_glmer_2, digits = 10)$sd[1:2], raw_output)
})


test_that('extract_vc.stanreg returns correlation', {
  init = extract_vc(stan_glmer_2, show_cor = TRUE)$Cor[[1]]
  expect_equal(dim(init), c(2, 2))
})

test_that('extract_vc.stanreg returns correlation', {
  init = extract_vc(stan_glmer_4, ci_level = 0, show_cor = TRUE)$Cor

  expect_type(init, 'list')

  dims = lapply(init, dim)

  expect_equal(dims$continent, c(2, 2))
  expect_equal(dims$country, c(2, 2))
})


test_that('extract_vc.stanreg works with ci_scale = var', {

  init  = extract_vc(stan_glmer_1, ci_scale = 'var')$var_2.5
  init2 = extract_vc(stan_glmer_1, ci_scale = 'sd')$sd_2.5

  expect_type(init, 'double')
  expect_type(init2, 'double')

  expect_lt(init2[1], init[1])
})


test_that('extract_vc.stanreg basic functionality: non-gaussian', {
  expect_equal(nrow(extract_vc(stan_glmer_glm)), 1)  # no residual var
})

test_that('extract_vc.stanreg basic functionality: multivariate model', {
  init = extract_vc(stan_glmer_mv, component = 'y2', ci_level = .8, digits = 2)
  expect_match(init$effect, 'y2')
})


test_that('extract_vc.stanreg basic functionality: multivariate model', {
  expect_warning(extract_vc(stan_glmer_mv, component = 'flag'))
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

test_that('extract_vc.gam basic functionality: cat slope', {
  expect_s3_class(extract_vc(gam_cat_slope), 'data.frame')
})

test_that('extract_vc.gam basic functionality: other smooth', {
  init = extract_vc(gam_other_smooth)
  expect_equal(init$group[1], 'x')
})


test_that('extract_vc.gam fails with non-REML', {
  ga_model_noREML = mgcv::gam(
    Reaction ~  Days + s(Subject, bs = 're') + s(Days, Subject, bs = 're'),
    data = lme4::sleepstudy
)
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

  init  = extract_vc(gam_1, ci_scale = 'var')$var_2.5
  init2 = extract_vc(gam_1, ci_scale = 'sd')$sd_2.5

  expect_type(init, 'double')
  expect_type(init2, 'double')

  expect_lt(init2[1], init[1])
})



