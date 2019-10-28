context('test extract_nlme_variances')

library(lme4)

lmer_1 <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
lmer_2 <- lmer(Reaction ~ Days + (1 + Days| Subject), data = sleepstudy)

library(glmmTMB)

tmb_1 <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
tmb_2 <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)


test_that('', {
  # expect_equal(class(extract_nlme_variances(res)), 'numeric')
})

test_that('', {
  # expect_equal(length(extract_nlme_variances(res)), 2)
})
