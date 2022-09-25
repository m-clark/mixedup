context('test extract_random_effects')



# Overall -----------------------------------------------------------------

test_that('extract_random_effects errors with wrong type of model', {
  mod = lm(mpg ~ vs, mtcars)
  expect_error(extract_random_effects(mod))
})

test_that('extract_random_effects errors with silly ci level', {
  expect_error(extract_random_effects(lmer_1, ci_level = 2))
})

# lme4 --------------------------------------------------------------------


context('test extract_random_effects.merMod')

test_that("lme4 installation is checked", {
  with_mock(
    'rlang::is_installed' = function() FALSE,
    expect_error(extract_random_effects(lmer_1))
  )
})

# does nothing
# test_that("lme4 installation is checked", {
#   mockery::stub(mixedup::extract_random_effects, 'rlang::is_installed',  FALSE) # doesn't work
#   expect_error(extract_random_effects(lmer_1))
# })



test_that('extract_random_effects.merMod basic functionality', {
  expect_s3_class(extract_random_effects(lmer_1), 'data.frame')
})

test_that('extract_random_effects.merMod basic functionality', {
  expect_s3_class(extract_random_effects(lmer_2), 'data.frame')
})


test_that('extract_random_effects.merMod works with multiple re', {
  expect_equal(
    nrow(extract_random_effects(lmer_3, re = 's')),
    nlevels(lmer_3@frame$s)
  )
})

test_that('extract_random_effects.merMod add_group_N', {
  expect_s3_class(extract_random_effects(lmer_4, add_group_N = TRUE), 'data.frame')
})

test_that('extract_random_effects.merMod errors with bad re name', {
  expect_error(extract_random_effects(lmer_2, re = 'subject'))
})



# glmmTMB -----------------------------------------------------------------

context('test extract_random_effects.glmmTMB')

test_that("glmmTMB installation is checked", {
  with_mock(
    'rlang::is_installed' = function() FALSE,
    expect_error(extract_random_effects(tmb_1, re = 'Subject'))
  )
})

test_that('extract_random_effects basic functionality', {
  expect_s3_class(extract_random_effects(tmb_1, re = 'Subject'), 'data.frame')
})

test_that('extract_random_effects basic functionality', {
  expect_s3_class(extract_random_effects(tmb_2, re = 'Subject'), 'data.frame')
})

test_that('extract_random_effects correct output', {
  expect_equal(
    nrow(extract_random_effects(tmb_4, re = 'dept')),
    nlevels(tmb_4$frame$dept)
  )
})


test_that('extract_random_effects can do zip', {
  expect_s3_class(
    extract_random_effects(tmb_zip, re = 'site', component = 'zi'),
    'data.frame'
    )
})

test_that('extract_random_effects.glmmTMB add_group_N', {
  expect_s3_class(extract_random_effects(tmb_4, add_group_N = TRUE), 'data.frame')
})

test_that('extract_random_effects.glmmTMB add_group_N for zip', {
  expect_s3_class(extract_random_effects(tmb_zip, add_group_N = TRUE), 'data.frame')
})

test_that('extract_random_effects errors with bad re name', {
  expect_error(extract_random_effects(tmb_zip, re = 'Site'))
})

test_that('extract_random_effects errors with wrong cond', {
  expect_error(extract_random_effects(tmb_disp, re = 'site', component = 'disp'))
})



# nlme --------------------------------------------------------------------


context('test extract_random_effects.lme')

# no check on package install as it is a base/recommended package

test_that('extract_random_effects.lme basic functionality', {
  expect_s3_class(extract_random_effects(lme_1), 'data.frame')
})

test_that('extract_random_effects.lme basic functionality', {
  expect_s3_class(extract_random_effects(lme_2), 'data.frame')
})


test_that('extract_random_effects.lme works with multiple re', {
  expect_equal(
    nrow(extract_random_effects(lme_3, re = 'd')),
    nlevels(droplevels(lme_3$data)$d)
  )
})

test_that('extract_random_effects.lme errors with bad re name', {
  expect_error(extract_random_effects(lme_3, re = 'subject'))
})


test_that('extract_random_effects.lme works with nlme', {
  expect_equal(
    nrow(extract_random_effects(nlme_1, re = 'Seed')),
    nlevels(Loblolly$Seed)
  )
})


test_that('extract_random_effects.lme add_group_N', {
  expect_s3_class(extract_random_effects(lme_3, add_group_N = TRUE), 'data.frame')
})

test_that('extract_random_effects.lme add_group_N for nlme', {
  expect_s3_class(extract_random_effects(nlme_1, add_group_N = TRUE), 'data.frame')
})



# brms --------------------------------------------------------------------

context('test extract_random_effects.brmsfit')

test_that("brms installation is checked", {
  with_mock(
    'rlang::is_installed' = function() FALSE,
    expect_error(extract_random_effects(brm_1, re = 'Subject'))
  )
})

test_that('extract_random_effects.brmsfit basic functionality', {
  expect_s3_class(extract_random_effects(brm_1, re = 'Subject'), 'data.frame')
})

test_that('extract_random_effects.brmsfit basic functionality', {
  expect_s3_class(extract_random_effects(brm_2, re = 'Subject'), 'data.frame')
})

test_that('extract_random_effects.brmsfit correct output', {
  expect_equal(
    nrow(extract_random_effects(brm_2, re = 'Subject')),
    nlevels(sleepstudy$Subject)*2
  )
})

test_that('extract_random_effects.brmsfit works with multiple re', {
  expect_equal(
    nrow(extract_random_effects(brm_3, re = 's')),
    nlevels(brm_3$data$s)
  )
})

test_that('extract_random_effects.brmsfit errors with bad re name', {
  expect_error(extract_random_effects(brm_2, re = 'subject'))
})



test_that('extract_random_effects.brmsfit basic functionality: multivariate model', {
  init = extract_random_effects(brm_mv, component = 'back', ci_level = .8, digits = 2)
  expect_match(init$component, 'back')
})

# currently fails due to potential brms bug
# test_that('extract_random_effects.brmsfit basic functionality: autocor model', {
#   expect_s3_class(extract_random_effects(brm_corAR, ci_level = .8, digits = 2),
#                   'data.frame')
# })

test_that('extract_random_effects.brmsfit basic functionality: zi model', {
  init = extract_random_effects(brm_zi, component = 'zi', ci_level = .8, digits = 2)
  expect_match(init$component, 'zi')
})

test_that('extract_random_effects.brmsfit add_group_N', {
  expect_s3_class(extract_random_effects(brm_3, add_group_N = TRUE), 'data.frame')
})

test_that('extract_random_effects.brmsfit add_group_N for zi', {
  expect_s3_class(extract_random_effects(brm_zi, component = 'zi', add_group_N = TRUE),
                  'data.frame')
})




# rstanarm ----------------------------------------------------------------

context('test extract_random_effects.stanreg')

test_that("rstanarm installation is checked", {
  with_mock(
    'rlang::is_installed' = function() FALSE,
    expect_error(extract_random_effects('rstanarm'))
  )
})

test_that('extract_random_effects.stanreg basic functionality', {
  expect_s3_class(extract_random_effects(stan_glmer_1), 'data.frame')
})

test_that('extract_random_effects.stanreg basic functionality', {
  expect_s3_class(extract_random_effects(stan_glmer_2), 'data.frame')
})


test_that('extract_random_effects.stanreg works with multiple re', {
  expect_equal(
    nrow(extract_random_effects(stan_glmer_3, re = 's')),
    length(unique(stan_glmer_3$data$s))
  )
})

test_that('extract_random_effects.stanreg errors with bad re name', {
  expect_error(extract_random_effects(stan_glmer_2, re = 'subject'))
})

test_that('extract_random_effects.stanreg add_group_N', {
  init = extract_random_effects(stan_glmer_2, add_group_N = TRUE)
  expect_s3_class(init, 'data.frame')
  expect_equal(unique(init$n), 10)
})

test_that('extract_random_effects.stanreg add_group_N', {
  expect_s3_class(extract_random_effects(stan_glmer_3, add_group_N = TRUE),
                  'data.frame')
})

test_that('extract_random_effects.stanreg returns output with multivariate', {
  init = extract_random_effects(stan_glmer_mv)

  expect_s3_class(init, 'data.frame')
  expect_equal(dplyr::n_distinct(init$component), 2)
})

test_that('extract_random_effects.stanreg multivariate accepts re input', {
  expect_s3_class(extract_random_effects(stan_glmer_mv, re = 'id'), 'data.frame')
})

test_that('extract_random_effects.stanreg returns output with multivariate
          and specific component', {
  re0 = extract_random_effects(stan_glmer_mv)
  re1 = extract_random_effects(stan_glmer_mv, component =  'y2')
  expect_lt(nrow(re1), nrow(re0))
})


test_that('extract_random_effects.stanreg returns output with multivariate
          and group N', {
  init = extract_random_effects(stan_glmer_mv,  add_group_N = TRUE)
  expect_true('n' %in% colnames(init))
})

test_that('extract_random_effects.stanreg works with jm', {
  expect_s3_class(extract_random_effects(stan_glmer_jm, re = 'id'), 'data.frame')
})

test_that('extract_random_effects.stanreg returns output with jm and add group', {
  init = extract_random_effects(
    stan_glmer_jm,
    re = 'id',
    add_group_N = TRUE,
    component = 'Long1'
  )
  expect_equal(unique(init$component), 'Long1')
})



# mgcv --------------------------------------------------------------------

context('test extract_random_effects.gam')

test_that('extract_random_effects.gam basic functionality', {
  init = extract_random_effects(gam_1)

  expect_s3_class(init, 'data.frame')
  expect_equal(nrow(init), 18)
})

test_that('extract_random_effects.gam basic functionality', {
  init = extract_random_effects(gam_2)

  expect_s3_class(init, 'data.frame')
  expect_equal(nrow(init), 36)
})

test_that('extract_vc.gam basic functionality: bam', {
  init = extract_random_effects(bam_1)

  expect_s3_class(extract_random_effects(bam_1), 'data.frame')
  expect_equal(unique(init$group_var), c('d', 'dept'))
})

test_that('extract_random_effects.gam works with multiple re', {
  expect_equal(
    nrow(extract_random_effects(gam_3, re = 's')),
    nlevels(gam_3$model$s)
  )
})

test_that('extract_random_effects.gam errors with bad re name', {
  expect_error(extract_random_effects(gam_2, re = 'subject'))
})

test_that('extract_random_effects.gam errors with only non-factor random effects',
          {
            d = mgcv::gamSim(n = 100, verbose = FALSE)
            m = mgcv::gam(y ~ s(x1, bs = 're'), data = d)
            expect_error(suppressWarnings(extract_random_effects(m)))
          })

test_that('extract_random_effects.gam warns with non-factor random effects', {
  set.seed(4)
  d = mgcv::gamSim(n = 100, verbose = FALSE)
  nb <- 10; n <- 100
  b <- rnorm(nb) * 2 ## random effect
  r <- sample(1:nb, n, replace = TRUE) ## r.e. levels
  y <- 2 + b[r] + rnorm(n)
  r <- factor(r)
  m = mgcv::gam(y ~ s(x1, bs = 're') + s(r, bs = 're'),
                data = cbind(d, r),
                method = 'REML')
  expect_warning(extract_random_effects(m))
})

test_that('extract_random_effects.gam fails if no factors', {

  ga_model_num_re = mgcv::gam(Reaction ~  s(Days) + s(Subject, bs='re') + s(Days, Subject, bs='re'),
                              data = within(lme4::sleepstudy, {Subject = as.integer(Subject)}),
                              method = 'REML')

  expect_error(suppressWarnings(extract_random_effects(ga_model_num_re)))

})


test_that('extract_random_effects.gam can handle other smooths',{
  testmod = mgcv::gam(Reaction ~  s(Days) + s(Subject, bs='re'),
                      data = lme4::sleepstudy,
                      method = 'REML')

  expect_equal(extract_random_effects(gam_1, digits = 1),
               extract_random_effects(testmod, digits = 1))
})

test_that('extract_random_effects.gam can handle other smooths',{

  init = mean(abs(
    extract_random_effects(gam_other_smooth, digits = 1)$value -
      extract_random_effects(gam_1, digits = 1)$value
  ))

  expect_lt(as.numeric(init), 1)
})

test_that('extract_random_effects.gam can handle categorical slopes', {
  # cat_slope discretizes Days into 3 levels
  expect_equal(
    nrow(extract_random_effects(gam_cat_slope, re = 'Subject', add_group_N = TRUE, ci_level = .9)),
    nlevels(sleepstudy$Subject)*1 + nlevels(sleepstudy$Subject)*3
  )
})

test_that('extract_random_effects.gam correct output', {
  expect_equal(
    nrow(extract_random_effects(gam_2, re = 'Subject')),
    nlevels(sleepstudy$Subject)*2
  )
})

test_that('extract_random_effects.gam add_group_N', {
  expect_s3_class(extract_random_effects(gam_3, add_group_N = TRUE), 'data.frame')
})

test_that('extract_random_effects.gam add_group_N', {
  expect_s3_class(extract_random_effects(gam_glm, add_group_N = TRUE),
                  'data.frame')
})
