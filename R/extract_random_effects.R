#' Extract random effects
#'
#' @description Extracts the random effects and their standard errors.
#'
#' @param model A merMod or glmmTMB object
#' @param re The name of the grouping variable for the random effects. Default
#'   is \code{NULL} to return all.
#' @param component Which of the three components 'cond', 'zi' or 'other' to
#'   select for a glmmTMB model. Default is 'cond'. Minimal testing on other
#'   options.
#'
#' @details Relative to \code{ranef} for the various packages, this just adds
#'   the standard errors and cluster ids as columns, and intervals for brmsfit objects.
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
#'@seealso \code{\link[lme4:ranef]{ranef.merMod}}, \code{\link[glmmTMB:ranef]{ranef.glmmTMB}},
#' \code{\link[nlme:ranef]{ranef.lme}}, \code{\link[brms:ranef]{ranef.brmsfit}}
#'
#' @export
extract_random_effects <- function(
  model,
  re = NULL,
  component = 'cond',
  digits = 3
) {
  if (!inherits(model, c('merMod', 'glmmTMB', 'lme', 'brmsfit')))
    stop('This only works for model objects from lme4, glmmTMB, brms, and nlme.')

  UseMethod('extract_random_effects')
}


#' @export
extract_random_effects.merMod <- function(
  model,
  re = NULL,
  component = 'cond',
  digits = 3
) {

  if (!is_package_installed('lme4'))
    stop('lme4 package required', call. = FALSE)

  # add check on re name
  all_re_names <- names(lme4::ranef(model))

  if (!is.null(re) && !re %in% all_re_names)
    stop(
      paste0('re is not among the names of the random effects: ',
             paste0(all_re_names, collapse = ' ')
      )
    )

  random_effects <- as.data.frame(lme4::ranef(model, condVar = TRUE))
  colnames(random_effects) <- c('group_var', 'effect', 'group', 'value', 'sd')

  if (!is.null(re)) {
    random_effects <- random_effects %>%
      filter(group_var == re)
  }

  random_effects %>%
    dplyr::mutate(
      lower  = value - 1.96 * sd,
      upper  = value + 1.96 * sd,
      effect = gsub(effect,
                    pattern = '[\\(, \\)]',
                    replacement = '')
    ) %>%
    dplyr::mutate_at(vars(group_var, effect, group), as.ordered) %>%
    dplyr::mutate_if(is.numeric, round, digits = digits)
}

#' @export
extract_random_effects.glmmTMB <- function(
  model,
  re = NULL,
  component = 'cond',
  digits = 3
) {

  if (!is_package_installed('glmmTMB'))
    stop('glmmTMB package required', call. = FALSE)

  # add check on re name
  all_re_names <- names(glmmTMB::ranef(model)[[component]])

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

  re  <- cleanup_coefs(re_names, random_effects, random_effect_var)

  dplyr::mutate_if(re, is.numeric, round, digits = digits)
}



#' @export
extract_random_effects.lme <- function(
  model,
  re = NULL,
  component,
  digits = 3
) {

  # output is inconsistent and inconsistently named, so just get all ranefs and
  # extract as needed

  re0 <- nlme::ranef(model)

  if (is.data.frame(re0)) {
    n_re <- 1
    all_re_names <- attr(re0, 'grpNames')
  }
  else {
    n_re <- length(re0)
    all_re_names <- names(re0)
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

  re <- cleanup_coefs(re_names, random_effects, se = NULL)

  dplyr::mutate_if(re, is.numeric, round, digits = digits)
}

#' @export
extract_random_effects.brmsfit <- function(
  model,
  re = NULL,
  component,
  digits = 3
) {

  if (!is_package_installed('brms'))
    stop('brms package required', call. = FALSE)

  # add check on re name
  all_re_names <- names(brms::ranef(model))

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

  random_effects <- brms::ranef(model)[[re]]

  group_names <- dimnames(random_effects)[[1]]

  re_names <- dimnames(random_effects)[[3]]

  # convert to data frame, cleanup names
  random_effects <-
    apply(random_effects, 3, function(x)
      as.data.frame(x) %>%
        dplyr::mutate(group = group_names) %>%
        dplyr::rename(value = Estimate, se = Est.Error) %>%
        dplyr::rename_at(dplyr::vars(dplyr::starts_with('Q')), function(x)
          gsub(x, pattern = 'Q', replacement = 'q\\_')))

  # reduce to a single data frame
  re <- purrr::reduce(
    random_effects,
    dplyr::left_join,
    by = 'group',
    suffix = paste0('_', re_names)
  )

  re %>%
    dplyr::mutate_if(is.numeric, round, digits = digits) %>%
    dplyr::rename_at(dplyr::vars(dplyr::starts_with('value_')), function(x)
      gsub(x, pattern = 'value_', replacement = '')) %>%
    dplyr::select(group, dplyr::everything())
}
