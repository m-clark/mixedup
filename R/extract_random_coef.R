#' Extract random coefficients and their variances
#'
#' @param model A merMod or glmmTMB object
#' @param re The name of the grouping variable for the random effects.
#' @param component Which of the three components 'cond', 'zi' or 'other' to
#'   select for a glmmTMB model. Default is 'cond'. Minimal testing on other
#'   options.
#' @param digits Rounding. Default is 3.
#'
#' @details Returns a data frame with random coefficients, a.k.a. random
#'   intercepts and random slopes, and their standard errors. Note that the
#'   standard errors assume independence of the conditional variance and the
#'   fixed-effects variance, thus the standard errors are the sum of variances
#'   for the fixed and random effects. See Bolker's demo
#'   \href{https://stackoverflow.com/questions/26198958/extracting-coefficients-and-their-standard-error-from-lme}{here}
#'   and additional discussion at the
#'   \href{https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#confidence-intervals-on-conditional-meansblupsrandom-effects}{GLMM
#'   FAQ}. This assumption may not be appropriate.
#'
#'
#' \code{nlme} only provides the coefficients no estimated variance, so this
#' doesn't add to what you get from basic functionality for those models.  In
#' addition, nlme adds all random effects to the fixed effects, whereas
#' \code{lme4} and others only add the effects requested.
#'
#' @return A data frame of the random coefficients and their standard errors.
#'
#' @importFrom stats coef vcov
#'
#' @examples
#' library(lme4)
#' lmer_1 <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
#' extract_random_coef(lmer_1, re = 'Subject')

#' library(glmmTMB)
#' tmb_1 <- glmmTMB(Reaction ~ Days + (1 | Subject), data = sleepstudy)
#' extract_random_coef(tmb_1, re = 'Subject')
#'
#' @export
extract_random_coef <- function(
  model,
  re = NULL,
  component = 'cond',
  digits = 3
) {

  if (!inherits(model, c('merMod', 'glmmTMB', 'lme')))
    stop('This only works for merMod objects from lme4, glmmTMB, and nlme.')

  UseMethod('extract_random_coef')
}

#' @export
extract_random_coef.merMod <- function(
  model,
  re = NULL,
  component,
  digits = 3
) {

  if (!is_package_installed('lme4'))
    stop('lme4 package required', call. = FALSE)

  if (is.null(re)) {
    warning('No random effect specified, using first.')
    re = 1
  }

  # call method for merMod vs. glmmTMB
  ran_coefs = coef(model)[[re]]
  random_effects = lme4::ranef(model, condVar = TRUE)[[re]]
  random_effect_covar <- attr(random_effects, "postVar")

  # deal with single random effect
  if (is.null(dim(random_effect_covar[,,1]))) {
    random_effect_var = matrix(random_effect_covar[1,,], ncol = 1)
  }
  else {
    random_effect_var = t(apply(random_effect_covar, 3, diag))
  }

  # extract only pertinent fe
  fe_names = names(lme4::fixef(model))
  re_names = colnames(random_effects)
  fixed_effect_var = diag(as.matrix(vcov(model)))
  fixed_effect_var = fixed_effect_var[fe_names %in% re_names]

  se <- sqrt(sweep(random_effect_var, 2, fixed_effect_var, "+"))
  se = as.data.frame(se)

  ran_coefs = ran_coefs[, re_names, drop = FALSE]

  # cleanup names, i.e. remove parens from (Intercept)
  out = cleanup_coefs(re_names, ran_coefs, se)

  dplyr::mutate_if(out, is.numeric, round, digits = digits)
}

#' @export
extract_random_coef.glmmTMB <- function(
  model,
  re = NULL,
  component = 'cond',
  digits = 3
  ) {

  if (!is_package_installed('glmmTMB'))
    stop('glmmTMB package required', call. = FALSE)

  if (is.null(re)) {
    warning('No random effect specified, using first.')
    re = 1
  }

  ran_coefs = coef(model)[[component]][[re]]
  random_effects = glmmTMB::ranef(model, condVar = TRUE)[[component]][[re]]
  random_effect_covar <- attr(random_effects, "condVar")

  # deal with single random effect
  if (is.null(dim(random_effect_covar[,,1]))) {
    random_effect_var = matrix(random_effect_covar[1,,], ncol = 1)
  }
  else {
    random_effect_var = t(apply(random_effect_covar, 3, diag))
  }

  # extract only pertinent fe
  fe_names = names(glmmTMB::fixef(model)[[component]])
  re_names = colnames(random_effects)
  fixed_effect_var = diag(vcov(model)[[component]])
  fixed_effect_var = fixed_effect_var[fe_names %in% re_names]

  se <- sqrt(sweep(random_effect_var, 2, fixed_effect_var, "+"))
  se = as.data.frame(se)

  ran_coefs = ran_coefs[, re_names, drop = FALSE]

  # cleanup names, i.e. remove parens from (Intercept)
  out = cleanup_coefs(re_names, ran_coefs, se)

  dplyr::mutate_if(out, is.numeric, round, digits = digits)
}

#' @export
extract_random_coef.lme <- function(
  model,
  re = NULL,
  component,
  digits = 3
) {

  fe = nlme::fixef(model)

  names(fe) =  gsub(
    names(fe),
    pattern = '[\\(, \\)]',
    replacement = ''
  )

  # necessary checks will be done via this
  re = extract_random_effects(model = model, re = re)

  out = sweep(re[,-1, drop = FALSE], 2, fe[names(fe) %in% names(re)], `+`)

  out$group = re$group

  out = dplyr::mutate_if(out, is.numeric, round, digits = digits)

  dplyr::select(out, group, dplyr::everything())
}

