#' Extract random coefficients and their variances
#'
#' @param model A merMod or glmmTMB object
#' @param re The name of the grouping variable for the random effects.
#' @param zi For a zero-inflated glmmTMB model, which part of the model you want
#'   random coefficients for?
#'
#' @details Returns a data frame with random coefficients, a.k.a. random
#'   intercepts and random slopes, and their standard errors.   The standard
#'   errors are the sum of variances for the fixed and random effects. See
#'   Bolker's demo
#'   \href{https://stackoverflow.com/questions/26198958/extracting-coefficients-and-their-standard-error-from-lme}{here}.
#'
#'
#' @return A data frame
#'
#' @importFrom stats coef vcov
#' @importFrom lme4 fixef ranef vcov.merMod
#' @importFrom glmmTMB fixef ranef
#' @importFrom dplyr rename_all
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
extract_random_coef <- function(model, re = NULL, zi = FALSE) {

  if (!inherits(model, c('merMod', 'glmmTMB')))
    stop('This only works for merMod objects from lme4 or models from glmmTMB.')

  UseMethod('extract_random_coef')
}

#' @export
#' @rdname extract_random_coef
extract_random_coef.merMod <- function(model, re = NULL, zi) {

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

  out
}

#' @export
#' @rdname extract_random_coef
extract_random_coef.glmmTMB <- function(model, re = NULL, zi = FALSE) {

  if (is.null(re)) {
    warning('No random effect specified, using first.')
    re = 1
  }

  cond_zi = 1 + zi
  ran_coefs = coef(model)[[cond_zi]][[re]]
  random_effects = glmmTMB::ranef(model, condVar = TRUE)[[cond_zi]][[re]]
  random_effect_covar <- attr(random_effects, "condVar")

  # deal with single random effect
  if (is.null(dim(random_effect_covar[,,1]))) {
    random_effect_var = matrix(random_effect_covar[1,,], ncol = 1)
  }
  else {
    random_effect_var = t(apply(random_effect_covar, 3, diag))
  }

  # extract only pertinent fe
  fe_names = names(glmmTMB::fixef(model)[[cond_zi]])
  re_names = colnames(random_effects)
  fixed_effect_var = diag(vcov(model)[[cond_zi]])
  fixed_effect_var = fixed_effect_var[fe_names %in% re_names]

  se <- sqrt(sweep(random_effect_var, 2, fixed_effect_var, "+"))
  se = as.data.frame(se)

  ran_coefs = ran_coefs[, re_names, drop = FALSE]

  # cleanup names, i.e. remove parens from (Intercept)
  out = cleanup_coefs(re_names, ran_coefs, se)

  out
}



cleanup_coefs <- function(re_names, ran_coefs, se) {
  colnames(se) = paste0('se_', re_names)
  colnames(se) = gsub(
    colnames(se),
    pattern = '[\\(, \\)]',
    replacement = ''
  )

  colnames(ran_coefs) = gsub(
    colnames(ran_coefs),
    pattern = '[\\(, \\)]',
    replacement = ''
  )

  out = data.frame(
    group = rownames(ran_coefs),
    ran_coefs,
    se
  )

  rownames(out) = NULL # remove

  out
}
