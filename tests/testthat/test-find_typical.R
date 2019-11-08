context('test find_typical')



# Overall -----------------------------------------------------------------


# lme4 --------------------------------------------------------------------

context('test find_typical.merMod')

library(lme4)

lmer_1 <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
lmer_2 <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
lmer_3 <- lmer(y ~ service + (1 | s) + (1 | d), data = InstEval[1:1000, ])
lmer_4 <- lmer(y ~ service + (1 + service | dept) + (1 | s), data = droplevels(InstEval[1:3000, ]))

test_that('find_typical.merMod basic functionality', {
  expect_s3_class(find_typical(lmer_1, re = 'Subject'), 'data.frame')
})

test_that('find_typical.merMod basic functionality', {
  expect_s3_class(find_typical(lmer_2, re = 'Subject'), 'data.frame')
})

test_that('find_typical.merMod basic functionality', {
  expect_s3_class(find_typical(lmer_3, re = 's'), 'data.frame')
})

test_that('find_typical.merMod probs', {
  # should have two results each for intercept and Days
  expect_equal(nrow(find_typical(lmer_2, re = 'Subject', probs = c(.25, .75))), 4)
})



# glmmTMB -----------------------------------------------------------------

context('test find_typical.glmmTMB')

library(glmmTMB)

tmb_1 <- glmmTMB(Reaction ~ Days + (1 | Subject), data = sleepstudy)
tmb_2 <- glmmTMB(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
tmb_3 <- glmmTMB(y ~ service + (1 | s) + (1 | d), data = InstEval[1:1000, ])
tmb_4 <- glmmTMB(y ~ service + (1 + service | d) + (1 | s), data = droplevels(InstEval[1:3000, ]))

test_that('find_typical.merMod basic functionality', {
  expect_s3_class(find_typical(tmb_1, re = 'Subject'), 'data.frame')
})

test_that('find_typical.merMod basic functionality', {
  expect_s3_class(find_typical(tmb_2, re = 'Subject'), 'data.frame')
})

test_that('find_typical.merMod basic functionality', {
  expect_s3_class(find_typical(tmb_3, re = 's'), 'data.frame')
})

test_that('find_typical.merMod probs', {
  # should have two results each for intercept and Days
  expect_equal(nrow(find_typical(tmb_2, re = 'Subject', probs = c(.25, .75))), 4)
})

# nlme --------------------------------------------------------------------

context('test find_typical.lme')

library(nlme)

lme_1 <- lme(Reaction ~ Days, random = ~ 1 | Subject, data = sleepstudy)
lme_2 <- lme(Reaction ~ Days, random = ~ 1 + Days | Subject, data = sleepstudy)
lme_3 <- lme(y ~ service,
             random = list(d = ~ 1, s = ~ 1),
             data = droplevels(InstEval[1:1000, ]))
lme_4 <- lme(y ~ service,
             random = list(d = ~ 1 + service, s = ~ 1 ),
             data = droplevels(InstEval[1:3000, ]))

test_that('find_typical.merMod basic functionality', {
  expect_s3_class(find_typical(lme_1, re = 'Subject'), 'data.frame')
})

test_that('find_typical.merMod basic functionality', {
  expect_s3_class(find_typical(lme_2, re = 'Subject'), 'data.frame')
})

test_that('find_typical.merMod basic functionality', {
  expect_s3_class(find_typical(lme_3, re = 's'), 'data.frame')
})

test_that('find_typical.merMod probs', {
  # should have two results each for intercept and Days
  expect_equal(nrow(find_typical(lme_2, re = 'Subject', probs = c(.25, .75))), 4)
})

# brms --------------------------------------------------------------------

context('test find_typical.brm')

library(brms)

# brms results are autoloaded
