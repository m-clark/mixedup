#' Extract random effects
#'
#' @description Extracts the random effects and their standard errors.
#'
#' @inheritParams extract_random_coef
#'
#' @details Relative to \code{ranef} for the various packages, this just adds
#'   the standard errors and cluster ids as columns.
#'
#' @return data frame of the random effects
#'
#' @examples
#' library(lme4)
#' lmer_2 <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
#' extract_random_effects(lmer_2)
#'
#' @seealso \code{\link{extract_random_coef}}
#'
#' @export
extract_random_effects <- function(
  model,
  re = NULL,
  component = 'cond',
  digits = 3
) {
  if (!inherits(model, c('merMod', 'glmmTMB')))
    stop('This only works for merMod objects from lme4 or models from glmmTMB.')

  UseMethod('extract_random_effects')
}


#' @export
extract_random_effects.merMod <- function(
  model,
  re = NULL,
  component = 'cond',
  digits = 3
) {

  if (is.null(re)) {
    warning('No random effect specified, using first.')
    re <- 1
  }

  random_effects <- lme4::ranef(model, condVar = TRUE)[[re]]
  re_names <- colnames(random_effects)

  random_effect_covar <- attr(random_effects, "postVar")

  # deal with single random effect
  if (is.null(dim(random_effect_covar[,,1]))) {
    random_effect_var <- data.frame(se = random_effect_covar[1,,])
  }
  else {
    random_effect_var <- t(apply(random_effect_covar, 3, diag))
    colnames(random_effect_var) <- paste0(colnames(random_effects), '_se')
    random_effect_var <- data.frame(random_effect_var)
  }

  re = cleanup_coefs(re_names, random_effects, random_effect_var)

  dplyr::mutate_if(re, is.numeric, round, digits = digits)

}

extract_random_effects.glmmTMB <- function(model, re = NULL, component) {
  "Not ready yet"
}

extract_random_effects.nlme <- function(model, re = NULL, component) {
  "Not ready yet"
}
