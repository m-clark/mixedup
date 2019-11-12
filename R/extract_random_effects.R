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
#' @param digits  Rounding. Default is 3.
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
      dplyr::filter(group_var == re)
  }

  random_effects %>%
    dplyr::mutate(
      lower  = value - 1.96 * sd,
      upper  = value + 1.96 * sd,
      effect = gsub(effect,
                    pattern = '[\\(, \\)]',
                    replacement = '')
    ) %>%
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

  random_effects <- as.data.frame(glmmTMB::ranef(model, condVar = TRUE)) %>%
    dplyr::filter(component == component) %>%
    dplyr::select(-component)

  colnames(random_effects) <- c('group_var', 'effect', 'group', 'value', 'sd')

  if (!is.null(re)) {
    random_effects <- random_effects %>%
      dplyr::filter(group_var == re)
  }

  random_effects %>%
    dplyr::mutate(
      lower  = value - 1.96 * sd,
      upper  = value + 1.96 * sd,
      effect = gsub(effect,
                    pattern = '[\\(, \\)]',
                    replacement = '')
    ) %>%
    dplyr::mutate_if(is.numeric, round, digits = digits)
}



#' @export
extract_random_effects.lme <- function(
  model,
  re = NULL,
  component,
  digits = 3
) {

  re0 <- nlme::ranef(model)

  # add check on re name
  if (inherits(re0, 'data.frame')) {
    all_re_names <- attr(re0, 'grpNames')
  }
  else {
    all_re_names <- names(re0)
  }

  if (!is.null(re) && !re %in% all_re_names)
    stop(
      paste0('re is not among the names of the random effects: ',
             paste0(all_re_names, collapse = ' ')
      )
    )

  if (inherits(re0, 'data.frame')) {
    random_effects <- re0 %>%
      as.data.frame() %>%
      dplyr::mutate(
        group = rownames(.)
      ) %>%
      tidyr::pivot_longer(
        cols = -group,
        names_to = 'effect',
        values_to = 'value'
      ) %>%
      dplyr::mutate(group_var = attr(re0, 'grpNames'))
  }
  else {
    random_effects <- re0 %>%
      purrr::map(function(x)
        dplyr::mutate(
          x,
          group = rownames(x))
      ) %>%
      purrr::map_df(
        tidyr::pivot_longer,
        cols = -group,
        names_to = 'effect',
        values_to = 'value',
        .id = 'group_var'
      )
  }

  if (!is.null(re)) {
    random_effects <- random_effects %>%
      dplyr::filter(group_var == re)
  }

  random_effects %>%
    dplyr::mutate_if(is.numeric, round, digits = digits) %>%
    dplyr::select(group_var, effect, group, value) %>%
    dplyr::mutate(
      effect = gsub(
        effect,
        pattern = '[\\(, \\)]',
        replacement = ''
      )
    ) %>%
    dplyr::arrange(group_var, effect, group)
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

  # more or less following broom
  re0 <- brms::posterior_samples(model, pars = '^r_')

  if (is.null(re0)) {
    stop("No parameter name matches the specified pattern.",
         call. = FALSE)
  }

  random_effects <- data.frame(effect = names(re0), stringsAsFactors = FALSE)

  random_effects <- random_effects %>%
    dplyr::mutate(
      effect = gsub("^r_", "", effect),
      group_var = gsub("\\[.*", "", effect),
      group = gsub(".*\\[|,.*", "", effect),
      effect = gsub(".*,|\\]", "", effect),
      value = apply(re0, 2, base::mean),
      se = apply(re0, 2, stats::sd)
    )

  # add_ci may add prob as arg in future
  probs <- c((1 - .95)/2, 1 - (1 - .95)/2)

  random_effects[, c("lower", "upper")] <-
    t(apply(re0, 2, stats::quantile, probs = probs))

  if (!is.null(re)) {
    random_effects <- random_effects %>%
      dplyr::filter(group_var == re)
  }

  random_effects %>%
    dplyr::mutate_if(is.numeric, round, digits = digits)
}
