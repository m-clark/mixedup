
# for testing
# all_mods  <- list.files('tests/testthat/', pattern = 'RData', full.names = T)
# sapply(all_mods, load, envir = .GlobalEnv)
#
# some_mods <- list.files('tests/testthat/', pattern = '^rstan(.)*RData', full.names = T)
# sapply(some_mods, load, envir = .GlobalEnv)

# Run lme4 models ---------------------------------------------------------

load('lme4_results.RData')

data("sleepstudy", package = 'lme4')

# library(lme4)
#
# lmer_0 <- lmer(Reaction ~ (1|Subject), data = sleepstudy)
# lmer_1 <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
# lmer_2 <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
# lmer_3 <- lmer(y ~ service + (1 | s) + (1 | d), data = InstEval[1:1000, ])
#
# # unusually complex models just for testing
# suppressWarnings({
#   lmer_4 <- lmer(y ~ service + (1 + as.numeric(lectage)| s) + (1 + as.numeric(studage)| d) + (1|dept), data = InstEval[1:5000, ])
#   lmer_5 <- lmer(y ~ service + (1 + as.numeric(lectage) + as.numeric(studage) + service| d) , data = InstEval[1:5000, ])
# })
#
# glmer_1 <- glmer(
#   count ~ spp + mined + (1 | site),
#   family = poisson,
#   data = glmmTMB::Salamanders
# )
#
# # for converge_it
#
# suppressWarnings({
#   lmer_conv <- lmer(
#     score ~ Machine + (1 + Machine | Worker),
#     data = nlme::Machines
#   )
# })
#
# suppressWarnings({
#   glmer_conv <- glmer(
#     count ~ spp + mined + DOP + Wtemp + DOY + (1 | sample) + (1 + DOY + DOP | site),
#     family = poisson,
#     data = glmmTMB::Salamanders
#   )
# })
# suppressWarnings({
#   lmer_conv_ridiculous <- lmer(
#     distance ~  Sex*age + (1 + Sex + age | Subject),
#     data = nlme::Orthodont,
#     control = lmerControl(optCtrl = list(maxfun = 10))
#   )
# })
#
# save(
#   lmer_0,
#   lmer_1,
#   lmer_2,
#   lmer_3,
#   lmer_4,
#   lmer_5,
#   glmer_1,
#   lmer_conv,
#   lmer_conv_ridiculous,
#   glmer_conv,
#   file = 'tests/testthat/lme4_results.RData'
# )

# Run TMB models ----------------------------------------------------------

load('tmb_results.RData')
load('tmb_cor_struct_results.RData')

# library(glmmTMB)
#
# tmb_0 <- glmmTMB(Reaction ~ (1 | Subject), data = sleepstudy)
# tmb_1 <- glmmTMB(Reaction ~ Days + (1 | Subject), data = sleepstudy)
# tmb_2 <- glmmTMB(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
# tmb_3 <- glmmTMB(y ~ service + (1 | s) + (1 | d), data = InstEval[1:1000, ])
#
# suppressWarnings({
#   tmb_4 <- glmmTMB(y ~ service + (1 + as.numeric(lectage)| s) + (1 + as.numeric(studage)| d) + (1|dept),
#                    data = InstEval[1:5000, ])
# })
#
# tmb_zip <- glmmTMB(
#   count ~ spp + mined + (1 | site),
#   zi =  ~ spp + mined + (1 | site),
#   family = truncated_poisson,
#   data = Salamanders
# )
#
# tmb_disp <- update(tmb_2, . ~ ., dispformula =  ~Days)
#
# save(
#   tmb_0,
#   tmb_1,
#   tmb_2,
#   tmb_3,
#   tmb_4,
#   tmb_zip,
#   tmb_disp,
#   file = 'tests/testthat/tmb_results.RData'
# )

# couldn't get corresponding brms model to run. This follows the vignette
# example.
# set.seed(1234)
# simGroup <- function(g, n = 6) {
#   x <- MASS::mvrnorm(mu = rep(0, n),
#                      Sigma = .7 ^ as.matrix(dist(1:n)))    ## Simulate the process
#   y <- x + rnorm(n)                               ## Add measurement noise
#   times <- factor(1:n)
#   group <- factor(rep(g, n))
#   data.frame(y, times, group)
# }
#
# dat1 <- do.call("rbind", lapply(1:100, simGroup))
# dat1$times_coord = numFactor(dat1$times)
# dat1$group2 <- rep(1:200, e =3)
#
# tmb_ar <- glmmTMB(y ~ ar1(times + 0 | group), data = dat1)
# tmb_us <- update(tmb_ar, . ~ . - ar1(times + 0 | group) + us(times + 0 | group))
# tmb_toep <- update(tmb_ar, . ~ . - ar1(times + 0 | group) + toep(times + 0 | group))
# tmb_cs <- update(tmb_ar, . ~ . - ar1(times + 0 | group) + cs(times + 0 | group))
# tmb_diag <- suppressWarnings(update(tmb_ar, . ~ . - ar1(times + 0 | group) + diag(times + 0 | group)))
# tmb_ou <- update(tmb_ar, . ~ . - ar1(times + 0 | group) + ou(times_coord + 0 | group))
# tmb_mat <- update(tmb_ar, . ~ . - ar1(times + 0 | group) + mat(times_coord + 0 | group))
# tmb_gau <- update(tmb_ar, . ~ . - ar1(times + 0 | group) + gau(times_coord + 0 | group))
# tmb_exp <- update(tmb_ar, . ~ . - ar1(times + 0 | group) + exp(times_coord + 0 | group))
#
# tmb_ar_2grp <- update(tmb_ar, . ~ . + ar1(times + 0 | group2))
# tmb_diag_2grp <- update(tmb_diag, . ~ . + diag(times + 0 | group2))
# tmb_gau_2grp <- update(tmb_gau, . ~ . + gau(times_coord + 0 | group2))
#
# save(
#   tmb_ar,
#   tmb_us,
#   tmb_toep,
#   tmb_cs,
#   tmb_diag,
#   tmb_ou,
#   tmb_mat,
#   tmb_gau,
#   tmb_exp,
#   tmb_ar_2grp,
#   tmb_diag_2grp,
#   tmb_gau_2grp,
#   file = 'tests/testthat/tmb_cor_struct_results.RData'
# )


# Run nlme models ---------------------------------------------------------

load('nlme_results.RData')
#
# library(nlme)
#
# lme_0 <- lme(Reaction ~ 1, random = ~ 1 | Subject, data = sleepstudy)
# lme_1 <- lme(Reaction ~ Days, random = ~ 1 | Subject, data = sleepstudy)
# lme_2 <- lme(Reaction ~ Days, random = ~ 1 + Days | Subject, data = sleepstudy)
# lme_3 <- lme(y ~ service,
#              random = list(d = ~ 1, s = ~ 1),
#              data = droplevels(InstEval[1:1000, ]))
# lme_4 <- lme(y ~ service,
#              random = list(d = ~ 1 + service, s = ~ 1 ),
#              data = droplevels(InstEval[1:3000, ]))
#
# # won't converge
# # lme_5 <- lme(y ~ service,
# #              random = list(d = ~ 1 + as.numeric(lectage) + as.numeric(studage) + service) ,
# #              data = droplevels(InstEval[1:5000, ]))
#
# nlme_1 <-  nlme(height ~ SSasymp(age, Asym, R0, lrc),
#                 data = Loblolly,
#                 fixed = Asym + R0 + lrc ~ 1,
#                 random = Asym ~ 1,
#                 start = c(Asym = 103, R0 = -8.5, lrc = -3.3))
#
# Just run these at test time?
#
# lme_het_var <- lme(
#   distance ~ scale(age) + Sex,
#   data = Orthodont,
#   random = ~ 1 | Subject,
#   weights = varIdent(form = ~ 1 | Sex)
# )
#
# lme_corCompSymm <-
#   update(
#     lme_2,
#     corr = corCompSymm(form = ~ Days),
#     data = sleepstudy %>% dplyr::filter(Days < 5)
#   )
#
# lme_corSymm <-
#   update(
#     lme_2,
#     corr = corSymm(form = ~ 1 | Subject),
#     data = sleepstudy %>% dplyr::filter(Days < 5)
#   )
#
# lme_corAR <-
#   update(
#     lme_2,
#     corr = corAR1(form = ~ Days),
#     data = sleepstudy %>% dplyr::filter(Days < 5)
#   )
#
# lme_corARMA <-
#   update(
#     lme_2,
#     corr = corARMA(form = ~ Days, p = 1, q = 1),
#     data = sleepstudy %>% dplyr::filter(Days < 5)
#   )
#
# lme_corCAR <-
#   update(
#     lme_2,
#     corr = corCAR1(form = ~ Days),
#     data = sleepstudy %>% dplyr::filter(Days < 5)
#   )
#
# save(
#   lme_0,
#   lme_1,
#   lme_2,
#   lme_3,
#   lme_4,
#   nlme_1,
#   lme_het_var,
#   lme_corCompSymm,
#   lme_corSymm,
#   lme_corAR,
#   lme_corARMA,
#   lme_corCAR,
#   file = 'tests/testthat/nlme_results.RData'
# )

# Run brms models ---------------------------------------------------------


load('brm_results.RData')
load('brm_cor_struct_results.RData')
load('brm_extended_results.RData')
brm_corCAR <- readRDS('brm_car_results.rds')

# library(brms)
#
# bprior1 <- prior(student_t(5,0,10), class = b) +
#   prior(cauchy(0,2), class = sd)
#
# brm_glm <- brm(
#   count ~ zAge + zBase * Trt + (1 | patient),
#   data = epilepsy,
#   family = poisson(),
#   prior = bprior1,
#   cores = 4,
#   thin  = 40
# )
#
# pr = prior(normal(0, 10), class = b)
#
# brm_0 <-
#   brm(
#     Reaction ~ 1 + (1 | Subject),
#     data = lme4::sleepstudy,
#     cores = 4,
#     thin  = 40
#   )
#
# brm_1 <-
#   brm(
#     Reaction ~ Days + (1 | Subject),
#     data = lme4::sleepstudy,
#     prior = pr,
#     cores = 4,
#     thin  = 40
#   )
#
# brm_2 <-
#   brm(
#     Reaction ~ Days + (1 + Days | Subject),
#     data = lme4::sleepstudy,
#     prior = pr,
#     cores = 4,
#     thin  = 40
#   )
#
#
#
# # more complex models just for testing (changed from others to minimize nparameters/object size)
# brm_3 <-
#   brm(
#     y ~ service + (1 | s) + (1 | dept),
#     data = lme4::InstEval[1:1000,],
#     prior = pr,
#     cores = 4,
#     thin  = 40
#   )
#
# brm_4 <-
#   brm(
#     giniPercap ~ math + year + (1 + year | country) + (1 + year | continent),
#     data = dplyr::mutate(noiris::pisa, year = year-min(year), math = scale(math)[,1]),
#     prior = pr,
#     cores = 4,
#     thin  = 40
#   )
#
# brm_5 <-
#   brm(
#     giniPercap ~ math + year + (1 + year + math | country) ,
#     data = dplyr::mutate(noiris::pisa, year = year-min(year), math = scale(math)[,1]),
#     prior = pr,
#     cores = 4,
#     thin  = 40
#   )
#
# probably problematic models but fine for testing

### standard autocor

# brm_corAR <- update(
#   brm_2,
#   autocor = cor_ar( ~ Days | Subject),
#   save_ranef = FALSE,
#   cores = 4,
#   thin = 40
# )
#
# brm_corARMA <-
#   update(
#     brm_2,
#     autocor = cor_arma(~ Days | Subject, p = 1, q = 2),
#     save_ranef = FALSE,
#     cores = 4,
#     thin = 40
#   )
#
# brm_corCompSymm <-
#   update(
#     brm_2,
#     autocor = cor_cosy(~ Days | Subject),
#     save_ranef = FALSE,
#     cores = 4,
#     thin = 40
#   )
#

### spatial
# generate some spatial data
# east <- north <- 1:10
# Grid <- expand.grid(east, north)
# K <- nrow(Grid)
#
# # set up distance and neighbourhood matrices
# distance <- as.matrix(dist(Grid))
# W <- array(0, c(K, K))
# W[distance == 1] <- 1
#
# # generate the covariates and response data
# x1 <- rnorm(K)
# x2 <- rnorm(K)
# theta <- rnorm(K, sd = 0.05)
# phi <- rmulti_normal(
#   1, mu = rep(0, K), Sigma = 0.4 * exp(-0.1 * distance)
# )
# eta <- x1 + x2 + phi
# prob <- exp(eta) / (1 + exp(eta))
# size <- rep(50, K)
# y <- rbinom(n = K, size = size, prob = prob)
# dat <- data.frame(y, size, x1, x2)

# fit a CAR model
#
# brm_corCAR <- brm(
#   y | trials(size) ~ x1 + x2,
#   data = dat,
#   family = binomial(),
#   autocor = cor_car(W),
#   cores = 2,  # very large file
#   chains = 2,
#   file = 'tests/testthat/brm_car_results',
#   thin = 40
# )
#
# data(oldcol, package = "spdep")
# brm_corSarLag <- brm(
#   CRIME ~ INC + HOVAL,
#   data = COL.OLD,
#   autocor = cor_lagsar(COL.nb),
#   cores = 4,
#   thin = 40
# )
#
# brm_corSarError <- brm(
#   CRIME ~ INC + HOVAL,
#   data = COL.OLD,
#   autocor = cor_errorsar(COL.nb),
#   cores = 4,
#   thin = 40
# )
#
#
#
# save(
#   brm_glm,
#   brm_0,
#   brm_1,
#   brm_2,
#   brm_3,
#   brm_4,
#   brm_5,
#   file = 'tests/testthat/brm_results.RData'
# )
#
#
# save(
#   brm_corAR,
#   brm_corARMA,
#   brm_corCompSymm,
#   brm_corSarLag,
#   brm_corSarError,
#   file = 'tests/testthat/brm_cor_struct_results.RData'
# )
#

### misc other models

# group <- rep(c("treat", "placebo"), each = 30)
# symptom_post <- c(rnorm(30, mean = 1, sd = 2), rnorm(30, mean = 0, sd = 1))
# dat1 <- data.frame(group, symptom_post)

# brm_sigma_simple <-
#   brm(
#     bf(symptom_post ~ group, sigma ~ group),
#     data = dat1,
#     family = gaussian,
#     cores = 4,
#     thin = 40
#   )
#
# brm_sigma <-
#   update(
#     brm_2,
#     bf(. ~ ., sigma ~ Days + (1 | Subject)),
#     cores = 4,
#     thin = 40,
#     newdata = lme4::sleepstudy
#   )
#
# brm_zi <- brm(
#   bf(count ~ persons + child + camper + (1|group), zi ~ child + (1|group)),
#   data = noiris::fish %>% mutate(group = rep(1:25, e=10)),
#   family = zero_inflated_poisson(),
#   cores = 4,
#   thin = 40
# )
#
# # # data("BTdata", package = "MCMCglmm")
#
# brm_mv <- brm(
#   mvbind(tarsus, back) ~ sex + hatchdate + (1 | p | fosternest) + (1 | q | dam),
#   rescor = TRUE,
#   data = BTdata,
#   cores = 4,
#   thin = 40,
# )
#
# save(
#   brm_sigma_simple,
#   brm_sigma,
#   brm_zi,
#   brm_mv,
#   BTdata,
#   file = 'tests/testthat/brm_extended_results.RData'
# )

# Run rstanarm models ---------------------------------------------------------


load('rstanarm_results.RData')


#
# library(rstanarm)
#
#
# stan_glmer_0 <-
#   stan_glmer(
#     Reaction ~ (1 | Subject),
#     data = lme4::sleepstudy,
#     prior = normal(0, 10),
#     cores = 4,
#     thin  = 40
#   )
#
# stan_glmer_1 <- update(stan_glmer_0, .~.+ Days)
#
# stan_glmer_2 <- update(stan_glmer_1, .~. - (1 | Subject) + (1 + Days | Subject))
#
# # more complex models just for testing (changed from others to minimize nparameters/object size)
# stan_glmer_3 <-
#   stan_glmer(
#     y ~ service + (1 | s) + (1 | dept),
#     data = lme4::InstEval[1:1000,],
#     prior = normal(0, 10),
#     cores = 4,
#     thin  = 40
#   )
#
# stan_glmer_4 <-
#   stan_glmer(
#     giniPercap ~ math + year + (1 + year | country) + (1 + year | continent),
#     data = dplyr::mutate(noiris::pisa, year = year-min(year), math = scale(math)[,1]),
#     prior = normal(0, 10),
#     cores = 4,
#     thin  = 40
#   )
#
# stan_glmer_5 <-
#   stan_glmer(
#     giniPercap ~ math + year + (1 + year + math | country) ,
#     data = dplyr::mutate(noiris::pisa, year = year-min(year), math = scale(math)[,1]),
#     prior = normal(0, 10),
#     cores = 4,
#     thin  = 40
#   )
#
# stan_glmer_glm <- stan_glmer(
#   count ~ zAge + zBase * Trt + (1 | patient),
#   data = epilepsy,
#   family = poisson(),
#   prior = student_t(5,0,10),
#   cores = 4,
#   thin  = 40
# )
#
# stan_glmer_mv <-
#   stan_mvmer(
#     formula = list(logBili ~ year + (1 | id),
#                    albumin ~ sex + year + (year | id)),
#     data = pbcLong,
#     cores = 4,
#     thin = 40
#   )
#
#
# save(
#   stan_glmer_glm,
#   stan_glmer_0,
#   stan_glmer_1,
#   stan_glmer_2,
#   stan_glmer_3,
#   stan_glmer_4,
#   stan_glmer_5,
#   stan_glmer_mv,
#   file = 'tests/testthat/rstanarm_results.RData'
# )

# Run mgcv models ---------------------------------------------------------

load('mgcv_results.RData')

# library(mgcv)
#
# gam_0 = gam(Reaction ~  s(Subject, bs = 're'),
#                   data = lme4::sleepstudy,
#                   method = 'REML')
#
# gam_1 = gam(Reaction ~  Days + s(Subject, bs = 're'),
#                   data = lme4::sleepstudy,
#                   method = 'REML')
#
# gam_2 = gam(
#   Reaction ~  Days +
#     s(Subject, bs = 're') +
#     s(Days, Subject, bs = 're'),
#   data = lme4::sleepstudy,
#   method = 'REML'
# )
#
# gam_3 <- gam(
#   y ~ service +
#     s(s, bs = 're') +
#     s(dept, bs = 're'),
#   method = 'REML',
#   data = lme4::InstEval[1:1000, ]
# )
#
# gam_glm = gam(
#   count ~ spp + mined + (1 | site),
#   family = poisson,
#   data = glmmTMB::Salamanders
# )
#
#
# bam objects are very large to save even for small models
# bam_1 = bam(
#   y ~ service +
#     s(d, bs = 're') +
#     s(dept, bs = 're'),
#   data = lme4::InstEval[1:1000, ],
#   nthreads = 10
#   )
#
#
# save(
#   gam_0,
#   gam_1,
#   gam_2,
#   gam_3,
#   gam_glm,
#   bam_1,
#   file = 'tests/testthat/mgcv_results.RData'
# )
