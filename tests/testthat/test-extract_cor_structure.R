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

library(nlme)


context('test extract_cor_structure.lme')

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

