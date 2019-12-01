context('test extract_cor_structure')

library(nlme)



test_that('extract_cor_structure fails with wrong class object', {
  expect_error(extract_cor_structure(lm(rnorm(10)~ 1)))
})

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
  expect_s3_class( extract_cor_structure(lme_corSymm), 'data.frame')
})
