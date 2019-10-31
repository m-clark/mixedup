#' Extract random effects
#'
#' @description Extracts the random effects and their standard errors.
#'
#' @inheritParams extract_random_coef
#'
#' @details Relative to \code{ranef} for the various packages, this just adds
#'   the standard errors and cluster ids as columns.
#'
#' \code{nlme} only provides the estimated random effect parameters, not their uncertainty, so it isn't provided.
#'
#' @return data frame of the random effects
#'
#' @examples
#' library(lme4)
#' lmer_2 <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
#' extract_random_effects(lmer_2)
#'
#' @seealso \code{\link{extract_random_coef}}, \code{\link{ranef.merMod}}, \code{\link{ranef.glmmTMB}}, \code{\link{ranef.nlme}}
#'
#' @export
extract_random_effects <- function(
  model,
  re = NULL,
  component = 'cond',
  digits = 3
) {
  if (!inherits(model, c('merMod', 'glmmTMB', 'lme')))
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

  # add check on re name
  all_re_names = names(ranef(model))

  if (!is.null(re) && !re %in% all_re_names)
    stop(
      paste0('re is not among the names of the random effects: ',
             paste0(all_re_names, collapse = ' ')
      )
    )

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

#' @export
extract_random_effects.glmmTMB <- function(
  model,
  re = NULL,
  component = 'cond',
  digits = 3
) {

  # add check on re name
  all_re_names = names(ranef(model)[[component]])

  if (!is.null(re) && !re %in% all_re_names)
    stop(
      paste0('re is not among the names of the random effects: ',
             paste0(all_re_names, collapse = ' ')
      )
    )

  if (is.null(re)) {
    warning('No random effect specified, using first.')
    re <- 1
  }

  random_effects <- glmmTMB::ranef(model, condVar = TRUE)[[component]][[re]]
  re_names <- colnames(random_effects)

  random_effect_covar <- attr(random_effects, "condVar")

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



#' @export
extract_random_effects.lme <- function(
  model,
  re = NULL,
  component,
  digits = 3
) {

  # output is inconsistent and inconsistently names, so just get all ranefs and
  # extract as needed

  re0 = nlme::ranef(model)

  if (is.data.frame(re0)) {
    n_re = 1
    all_re_names = attr(re0, 'grpNames')
  }
  else {
    n_re = length(re0)
    all_re_names = names(re0)

  }

  # add check on re name
  if (!is.null(re) && !re %in% all_re_names)
    stop(
      paste0('re is not among the names of the random effects: ',
             paste0(all_re_names, collapse = ' ')
      )
    )

  if (is.null(re)) {
    warning('No random effect specified, using first.')
    re <- 1
  }

  if (n_re > 1)
    random_effects <- re0[[re]]
  else
    random_effects <- re0

  re_names <- colnames(random_effects)

  re = cleanup_coefs(re_names, random_effects, se = NULL)

  dplyr::mutate_if(re, is.numeric, round, digits = digits)


}
