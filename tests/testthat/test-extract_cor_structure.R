context('test extract_cor_structure')

test_that('extract_cor_structure fails with wrong class object', {
  expect_error(extract_cor_structure(lm(rnorm(10)~ 1)))
})

# nlme --------------------------------------------------------------------


library(nlme)


context('test extract_cor_structure.lme')

test_that('extract_cor_structure returns a proper result', {
  init = extract_cor_structure(lme_corAR)
  cn = colnames(init)
  expect_equal(cn, 'Phi')
})

test_that('extract_cor_structure returns a proper result', {
  init = extract_cor_structure(lme_corCompSymm)
  cn = colnames(init)
  expect_equal(cn, 'Rho')
})

test_that('extract_cor_structure returns a vector of appropriate length', {
  init = extract_cor_structure(lme_corARMA)
  cn = colnames(init)
  expect_true(all(grepl(cn, pattern = c('Phi|Theta'))))
})

test_that('extract_cor_structure returns a proper result', {
  init = extract_cor_structure(lme_corCAR)
  cn = colnames(init)
  expect_equal(cn, 'Phi')
})

test_that('extract_cor_structure returns a proper result', {
  expect_s3_class(extract_cor_structure(lme_corSymm), 'data.frame')
})



# brms --------------------------------------------------------------------

library(brms)


context('test extract_cor_structure.brmsfit')

test_that('extract_cor_structure returns a proper result', {
  init = suppressWarnings(extract_cor_structure(brm_corAR))
  expect_equal(init$parameter, 'ar[1]')
})

test_that('extract_cor_structure returns a proper result', {
  init = suppressWarnings(extract_cor_structure(brm_corARMA))

  expect_true(any(grepl(init$parameter, pattern = '^ma')))
})

test_that('extract_cor_structure returns a proper result', {
  init = suppressWarnings(extract_cor_structure(brm_corCompSymm))

  expect_equal(init$parameter, 'cosy')
})

### spatial models

test_that('extract_cor_structure returns a proper result', {
  init = suppressWarnings(extract_cor_structure(brm_corCAR))

  expect_true(all(grepl(init$parameter, pattern = 'car')))
})


test_that('extract_cor_structure returns a proper result', {
  init = suppressWarnings(extract_cor_structure(brm_corSarError))
  expect_equal(init$parameter, 'errorsar')
})


test_that('extract_cor_structure returns a proper result', {
  init = suppressWarnings(extract_cor_structure(brm_corSarLag))

  expect_equal(init$parameter, 'lagsar')
})


# glmmTMB --------------------------------------------------------------------

library(glmmTMB)

context('test extract_cor_structure.glmmTMB')

test_that('extract_cor_structure returns a proper result: ar1', {
  expect_s3_class(extract_cor_structure(tmb_ar, which_cor = 'ar1'), 'data.frame')
})

test_that('extract_cor_structure returns a proper result: cs', {
  expect_s3_class(extract_cor_structure(tmb_cs, which_cor = 'cs'), 'data.frame')
})

test_that('extract_cor_structure returns a proper result: diag', {
  expect_s3_class(extract_cor_structure(tmb_diag, which_cor = 'diag'), 'data.frame')
})

test_that('extract_cor_structure returns a proper result: ou', {
  expect_s3_class(extract_cor_structure(tmb_ou, which_cor = 'ou'), 'data.frame')
})

test_that('extract_cor_structure returns a proper result: mat', {
  expect_s3_class(extract_cor_structure(tmb_mat, which_cor = 'mat'), 'data.frame')
})

test_that('extract_cor_structure returns a proper result: gau', {
  expect_s3_class(extract_cor_structure(tmb_gau, which_cor = 'gau'), 'data.frame')
})

test_that('extract_cor_structure returns a proper result: exp', {
  expect_s3_class(extract_cor_structure(tmb_exp, which_cor = 'exp'), 'data.frame')
})

test_that('extract_cor_structure errors with wrong cor structure', {
  expect_error(extract_cor_structure(tmb_exp, which_cor = 'ex'))
})

test_that('extract_cor_structure errors with non-matching cor structure', {
  expect_error(extract_cor_structure(tmb_exp, which_cor = 'gau'))
})

## improbable two group structures
test_that('extract_cor_structure returns a proper result: 2 groups ar1', {
  init = extract_cor_structure(tmb_ar_2grp, which_cor = 'ar1')
  expect_identical(init$parameter, 'ar1')
})

test_that('extract_cor_structure returns a proper result: 2 groups diag', {
  init = extract_cor_structure(tmb_diag_2grp, which_cor = 'diag')
  expect_s3_class(init, 'data.frame')
})

test_that('extract_cor_structure returns a proper result: 2 groups gau', {
  init = extract_cor_structure(tmb_gau_2grp, which_cor = 'gau')
  expect_s3_class(init, 'data.frame')
})

