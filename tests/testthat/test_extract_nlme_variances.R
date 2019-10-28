context('test extract_nlme_variances')

library(nlme)
res <- lme(distance ~ age + Sex,
           data = Orthodont,
           random = ~ 1|Subject,
           weights=varIdent(form = ~1|Sex))


test_that('extract_nlme_variances returns a vector', {
  expect_equal(class(extract_nlme_variances(res)), 'numeric')
})

test_that('extract_nlme_variances returns a vector of appropriate length', {
  expect_equal(length(extract_nlme_variances(res)), 2)
})
