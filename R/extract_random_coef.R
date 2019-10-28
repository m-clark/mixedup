#' Extract random coefficients and their variances
#'
#' @param model A merMod or glmmTMB object
#' @param which_re The name of the grouping variable desired.
#' @param zi For a zero-inflated glmmTMB model, which part of the model you want random coefficients for?
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
#'
#' @export
extract_random_coef <- function(model, which_re = NULL, zi = FALSE) {

  if (!inherits(model, c('merMod', 'glmmTMB')))
    stop('This only works for merMod objects from lme4 or models from glmmTMB.')

  UseMethod('extract_random_coef')
}

extract_random_coef.merMod <- function(model, which_re = NULL) {
  # https://stackoverflow.com/questions/26198958/extracting-coefficients-and-their-standard-error-from-lme

  if (is.null(which_re)) {
    warning('No random effect specified, using first.')
    which_re = 1
  }

  # call method for merMod vs. glmmTMB
  ran_coefs = coef(model)[[which_re]]
  random_effects = lme4::ranef(model, condVar = TRUE)[[which_re]]
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
  fixed_effect_var = diag(lme4::vcov.merMod(model))
  fixed_effect_var = fixed_effect_var[fe_names %in% re_names]

  se <- sqrt(sweep(random_effect_var, 2, fixed_effect_var, "+"))
  se = as.data.frame(se)
  colnames(se) = paste0('se_', re_names)

  out = cbind(ran_coefs, se)

  dplyr::rename_all(out,
                    function(nam) gsub(nam,
                                       pattern = '[\\(, \\)]',
                                       replacement = ''
                                       )
                    )
}


extract_random_coef.glmmTMB <- function(model, which_re = NULL, zi = FALSE) {
  # https://stackoverflow.com/questions/26198958/extracting-coefficients-and-their-standard-error-from-lme


  if (is.null(which_re)) {
    warning('No random effect specified, using first.')
    which_re = 1
  }


  # call method for merMod vs. glmmTMB?

  cond_zi = 1 + zi
  ran_coefs = coef(model)[[cond_zi]][[which_re]]
  random_effects = glmmTMB::ranef(model, condVar = TRUE)[[cond_zi]][[which_re]]
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
  colnames(se) = paste0('se_', re_names)

  out = cbind(ran_coefs, se)

  dplyr::rename_all(out,
                    function(nam) gsub(nam,
                                       pattern = '[\\(, \\)]',
                                       replacement = ''
                    )
  )
}
