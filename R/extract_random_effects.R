#' Extract random effects
#'
#' @description Extracts the random effects and their standard errors.
#'
#' @param model An appropriate model. See details.
#' @param re The name of the grouping variable for the random effects. Default
#'   is \code{NULL} to return all.
#' @param ci_level Where possible, confidence level < 1, typically above 0.90. A
#'   value of 0 will not report it. Default is .95. Not applicable to nlme
#'   objects.
#' @param component For glmmTMB objects, which of the three
#'   components 'cond', 'zi' or 'other' to select for a glmmTMB model. Default
#'   is 'cond'. Minimal testing on other options. For brmsfit objects, this can
#'   filter results to a certain part of the output, e.g. 'sigma' or 'zi' of
#'   distributional models, or a specific outcome of a multivariate model.  In
#'   this case \code{component} is a regular expression that ends the name of
#'   the parameters of the output (e.g. '__component').
#' @param digits  Rounding. Default is 3.
#' @param ... Other arguments specific to the method. Unused at present.
#'
#' @details Relative to \code{ranef} for the various packages, this just adds
#'   the standard errors and cluster ids as columns, and intervals.
#'
#'  Current models supported:
#'
#' \describe{
#'  \item{merMod}{}
#'  \item{glmmTMB}{}
#'  \item{brms}{}
#'  \item{nlme}{}
#'  \item{brms}{}
#'  \item{rstanarm}{}
#'  \item{mgcv}{}
#' }
#'
#' @note
#' \code{nlme} only provides the estimated random effect parameters, not their uncertainty, so it isn't provided.
#'
#' \code{merMod} and \code{glmmTMB} results are based on the estimated conditional variances, i.e. \code{condvar = TRUE}.  This is likely an underestimate relative to brms results.
#'
#' For \code{mgcv}, the `Vp` (Bayesian) estimated variance covariance matrix is used.
#' @return data frame of the random effects
#' @seealso
#' \code{\link[lme4:ranef.merMod]{ranef.merMod}},
#' \code{\link[glmmTMB:ranef.glmmTMB]{ranef.glmmTMB}},
#' \code{\link[nlme:ranef.lme]{ranef.lme}},
#' \code{\link[brms:ranef.brmsfit]{ranef.brmsfit}},
#' \code{\link[mgcv:gamObject]{gamObject}},
#' \code{\link[mgcv:smooth.construct.re.smooth.spec]{smooth.construct.re.smooth.spec}}
#'
#' @examples
#' library(lme4)
#' library(mixedup)
#'
#' lmer_model <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
#' extract_random_effects(lmer_model)
#'
#'@seealso \code{\link[lme4:ranef.merMod]{ranef.merMod}}, \code{\link[glmmTMB:ranef.glmmTMB]{ranef.glmmTMB}},
#' \code{\link[nlme:ranef.lme]{ranef.lme}}, \code{\link[brms:ranef.brmsfit]{ranef.brmsfit}}
#'
#' @export
extract_random_effects <- function(
  model,
  re = NULL,
  ci_level = .95,
  digits = 3,
  component = NULL,
  ...
) {
  if (!inherits(model,
                c('merMod', 'glmmTMB', 'lme', 'gam', 'stanreg', 'brmsfit'))
  )
    stop('This only works for model objects from lme4, glmmTMB, rstanarm, brms,
         mgcv, and nlme.')

  if (ci_level < 0 | ci_level >= 1)
    stop('Nonsensical confidence level for ci_level.  Must be between 0 and 1.')

  UseMethod('extract_random_effects')
}


#' @rdname extract_random_effects
#' @export
extract_random_effects.merMod <- function(
  model,
  re = NULL,
  ci_level = .95,
  digits = 3,
  ...
  # component = 'cond',
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
  colnames(random_effects) <- c('group_var', 'effect', 'group', 'value', 'se')

  if (ci_level > 0) {

    lower = (1 - ci_level)/2
    upper = 1 - lower
    mult <- stats::qnorm(upper)

    random_effects <- random_effects %>%
      dplyr::mutate(
        lower = value - mult * se,
        upper = value + mult * se
      )

    colnames(random_effects)[colnames(random_effects) %in% c('lower', 'upper')] <-
      paste0(c('lower_', 'upper_'), c(lower, upper) * 100)
  }

  if (!is.null(re)) {
    random_effects <- random_effects %>%
      dplyr::filter(group_var == re)
  }


  random_effects %>%
    dplyr::mutate(
      effect = gsub(effect,
                    pattern = '[\\(, \\)]',
                    replacement = '')
    ) %>%
    dplyr::mutate_if(is.numeric, round, digits = digits) %>%
    dplyr::as_tibble()
}

#' @rdname extract_random_effects
#' @export
extract_random_effects.glmmTMB <- function(
  model,
  re = NULL,
  ci_level = .95,
  digits = 3,
  component = 'cond',
  ...
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

  colnames(random_effects) <- c('group_var', 'effect', 'group', 'value', 'se')

  if (ci_level > 0) {

    lower = (1 - ci_level)/2
    upper = 1 - lower
    mult <- stats::qnorm(upper)

    random_effects <- random_effects %>%
      dplyr::mutate(
        lower = value - mult * se,
        upper = value + mult * se
      )

    colnames(random_effects)[colnames(random_effects) %in% c('lower', 'upper')] <-
      paste0(c('lower_', 'upper_'), c(lower, upper) * 100)
  }

  if (!is.null(re)) {
    random_effects <- random_effects %>%
      dplyr::filter(group_var == re)
  }

  random_effects %>%
    dplyr::mutate(
      effect = gsub(effect,
                    pattern = '[\\(, \\)]',
                    replacement = '')
    ) %>%
    dplyr::mutate_if(is.numeric, round, digits = digits) %>%
    dplyr::as_tibble()
}


#' @rdname extract_random_effects
#' @export
extract_random_effects.lme <- function(
  model,
  re = NULL,
  ci_level = NULL,
  digits = 3,
  # component,
  ...
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
    dplyr::arrange(group_var, effect, group) %>%
    dplyr::as_tibble()
}

#' @rdname extract_random_effects
#' @export
extract_random_effects.brmsfit <- function(
  model,
  re = NULL,
  ci_level = .95,
  digits = 3,
  component = NULL,
  ...
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

  # more or less following broom, but note that brms offers both pars and groups
  # args that might be better to use, if not less computation
  re0 <- brms::posterior_samples(model, pars = '^r_')

  # not sure this is necessary, would only happen if something very wrong with
  # model or no random effects.
  if (is.null(re0)) {
    stop("No parameter name matches the specified pattern.", call. = FALSE)
  }

  random_effects <- data.frame(effect = names(re0), stringsAsFactors = FALSE)

  random_effects <- random_effects %>%
    dplyr::mutate(
      effect = gsub("^r_", "", effect),
      group_var = gsub("\\[.*", "", effect),
      group = gsub(".*\\[|,.*", "", effect),
      effect = gsub(".*,|\\]", "", effect),
      value = base::colMeans(re0),
      se = purrr::map_dbl(re0, stats::sd)
    )

  # add_ci may add prob as arg in future
  if (ci_level > 0) {

    lower <- (1 - ci_level)/2
    upper <- 1 - lower

    ci <- t(apply(re0, 2, stats::quantile, probs = c(lower, upper)))

    colnames(ci) <- paste0(c('lower_', 'upper_'), c(lower, upper) * 100)

    random_effects <- dplyr::bind_cols(random_effects, data.frame(ci))

  }

  if (!is.null(re)) {
    random_effects <- random_effects %>%
      dplyr::filter(group_var == re)
  }

  if (!is.null(component)) {
    random_effects <- random_effects %>%
      dplyr::filter(grepl(group_var, pattern = paste0('__', component, '$')))
  }

  random_effects %>%
    dplyr::mutate_if(is.numeric, round, digits = digits) %>%
    dplyr::select(group_var, dplyr::everything()) %>%
    dplyr::as_tibble()
}

#' @rdname extract_random_effects
#' @export
extract_random_effects.stanreg <- function(...) {

  if (!is_package_installed('rstanarm'))
    stop('rstanarm package required', call. = FALSE)

  # Structure is the same as lme4
  extract_random_effects.merMod(...)
}



#' @rdname extract_random_effects
#' @export
extract_random_effects.gam <- function(
  model,
  re = NULL,
  # component,
  ci_level = .95,
  digits = 3,
  ...
) {
  # get the re variables and their levels
  re_terms <- purrr::map_lgl(model$smooth,
                             function(x)
                               inherits(x, "random.effect"))

  re_names <- purrr::map_chr(model$smooth[re_terms],
                             function(x)
                               ifelse(length(x$vn) == 1,
                                      x$vn,
                                      x$vn[length(x$vn)]))

  re_levels <- vector("list", length(re_names))

  # add check on re name/type
  # check that re is factor as re smooth can be applied to continuous
  for (i in seq_along(re_names)) {
    if (!inherits(model$model[, re_names[i]], "factor")) {
      warning(
        paste0(re_names[i], ' is not a factor. No results provided for it.')
      )
      re_levels[[i]] <- NULL
    }
    else {
      re_levels[[i]] <- levels(model$model[, re_names[i]])
    }
  }


  if (purrr::is_empty(re_levels) | all(purrr::map_lgl(re_levels, is.null))) {
    stop('No factor random effects.')
  }

  non_factors <- purrr::map_lgl(re_levels, is.null)

  # this test is covered but covr ignores for some reason
  if (any(non_factors)) {
    re_terms[non_factors] <- FALSE
  }

  if (!is.null(re) && !re %in% re_names)
    stop(
      paste0('re is not among the names of the random effects: ',
             paste0(re_names, collapse = ' ')
      )
    )

  re_labels <- purrr::map(model$smooth[re_terms], function(x) x$label)

  gam_coef <- stats::coef(model)

  # issue, parenthesis in the names means problematic regex matching so remove
  # all but key part of pattern
  re_label_base <- gsub(re_labels, pattern = "s\\(", replacement = '') # remove first s
  re_label_base <- gsub(re_label_base, pattern = "\\(|\\)", replacement = '') # remove parenthesis

  re_coef <- grepl(names(gam_coef), pattern = paste0('^s\\(', re_label_base, collapse = "|"))

  re0 <- gam_coef[re_coef]

  gam_se <- sqrt(diag(model$Vp)) # no names
  gam_se <- gam_se[names(gam_coef) %in% names(re0)]

  # clean up names
  names(re0) <- gsub(names(re0), pattern = "s\\(|\\)", replacement = '')
  names(re0) <- gsub(names(re0), pattern = "\\.[0-9]+", replacement = '')

  re_n <- dplyr::n_distinct(names(re0)) # possible use later
  re_names <- names(re0)

  random_effects <- dplyr::tibble(effect = re_names) %>%
    dplyr::mutate(
      group_var = split_group_effect(effect, which = 2),
      effect = split_group_effect(effect, which = 1),
      effect = ifelse(effect ==  group_var, 'Intercept', effect),
      group = unlist(re_levels),
      value = re0,
      se = gam_se
    )

  if (ci_level > 0) {

    lower = (1 - ci_level)/2
    upper = 1 - lower
    mult <- stats::qnorm(upper)

    random_effects <- random_effects %>%
      dplyr::mutate(
        lower = value - mult * se,
        upper = value + mult * se
      )

    colnames(random_effects)[colnames(random_effects) %in% c('lower', 'upper')] <-
      paste0(c('lower_', 'upper_'), c(lower, upper) * 100)
  }

  if (!is.null(re)) {
    random_effects <- random_effects %>%
      dplyr::filter(group_var == re)
  }

  random_effects %>%
    dplyr::select(group_var, effect, dplyr::everything()) %>%
    dplyr::mutate_if(is.numeric, round, digits = digits)
}
