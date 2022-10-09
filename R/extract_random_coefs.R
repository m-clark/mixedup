#' Extract random coefficients and their variances
#'
#' @description Fixed effect + random effects.
#'
#' @param model A merMod, nlme, brms, or glmmTMB object
#' @param re The name of the grouping variable for the random effects.
#' @param ci_level Where possible, confidence level < 1, typically above 0.90. A
#'   value of 0 will not report it. Default is .95. Not applicable to nlme
#'   objects.
#' @param digits Rounding. Default is 3.
#' @param component For glmmTMB objects, which of the two components 'cond' or
#'   'zi' to select. Default is 'cond'. For brmsfit objects, this can filter
#'   results to a certain part of the output, e.g. 'sigma' or 'zi' of
#'   distributional models, or a specific outcome of a multivariate model.  In
#'   this case `component` is a regular expression that ends the name of the
#'   parameters of the output (e.g. '__component').
#' @param ... Other arguments specific to the method. For example `add_group_N`
#'   for `extract_random_effects`. Will not apply to brmsfit or stanreg models.
#'   Experimental.
#'
#' @details Returns a data frame with *random coefficients*, a.k.a. random
#'   intercepts and random slopes, and their standard errors.
#'r
#'   Note that the standard errors assume independence of the conditional
#'   variance and the fixed-effects variance, thus the standard errors are the
#'   sum of variances for the respective fixed and random effects. See Bolker's
#'   demo
#'   \href{https://stackoverflow.com/questions/26198958/extracting-coefficients-and-their-standard-error-from-lme}{here}
#'   and additional discussion at the
#'   \href{https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#confidence-intervals-on-conditional-meansblupsrandom-effects}{GLMM
#'   FAQ}. As noted there, this assumption may not be, and likely is not,
#'   appropriate, and if you are really interested in an accurate uncertainty
#'   estimate you should probably use `brms`.
#'
#'   Please realize that this functionality is likely only appropriate for
#'   simpler GLMM type models, and is mostly just a shortcut for those settings.
#'   It may work for more complicated situations also, but I don't make any
#'   guarantees. For more complex models that include multiple
#'   outcomes/categories or have other anomalies, this function likely will not
#'   work even if the underlying `extract_fixed_effects` and
#'   `extract_random_effects` do, as naming conventions are not consistent
#'   enough within the relative packages to deal with this in a general way. I
#'   will continue to look into its feasibility, but don't expect much.
#'
#' @note The `nlme` package only provides the coefficients with no estimated
#'   variance, so this function doesn't add to what you get from basic
#'   functionality for those models.  In addition, `nlme` adds all random
#'   effects to the fixed effects, whereas `lme4` and others only add the
#'   effects requested.
#'
#' @note For multicomponent `glmmTMB` models, e.g. zip, please specify the component
#'   argument.
#'
#'   `extract_coef` and `extract_random_coefficients` are aliases.
#'
#' @return A data frame of the random coefficients and their standard errors.
#'
#' @family extract
#'
#' @importFrom stats coef vcov
#' @importFrom dplyr rename_all
#'
#' @examples
#' library(lme4)
#' library(mixedup)
#'
#' lmer_1 <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
#' extract_random_coefs(lmer_1, re = 'Subject')

#' library(glmmTMB)
#' tmb_1 <- glmmTMB(Reaction ~ Days + (1 | Subject), data = sleepstudy)
#' extract_random_coefs(tmb_1, re = 'Subject')
#'
#' @export
extract_random_coefs <- function(
  model,
  re        = NULL,
  ci_level  = .95,
  digits    = 3,
  component = NULL,
  ...
) {

  assertthat::assert_that(
    inherits(model, c('merMod', 'glmmTMB', 'lme', 'gam', 'stanreg', 'brmsfit')),
    msg = 'This only works for model objects from lme4, glmmTMB, rstanarm, brms,
         mgcv, and nlme.'
  )

  assertthat::assert_that(
    ci_level >= 0 & ci_level < 1,
    msg = 'Nonsensical confidence level for ci_level.  Must be between 0 and 1.'
  )

  UseMethod('extract_random_coefs')
}

#' @rdname extract_random_coefs
#' @export
extract_random_coefs.merMod <- function(
  model,
  re       = NULL,
  ci_level = .95,
  digits   = 3,
  ...
) {

  ze_dots = rlang::dots_list(...)

  # don't want/need cis calculated here, just values
  random_effects <- extract_random_effects(model, re = re, ci_level = 0, ...)

  fixed_effects  <- extract_fixed_effects(model, ci_level = 0, ...) %>%
    dplyr::rename(effect   = term,
                  se_fe    = se,
                  value_fe = value)

  coefs_init <- random_effects %>%
    dplyr::left_join(fixed_effects, by = 'effect')

  # if condvar is true or there is no condvar, add variances
  if ((any(names(ze_dots) == 'condvar') && ze_dots$condvar) |
      ! 'condvar' %in% names(ze_dots) ) {
    coefs_init <- coefs_init %>%
      dplyr::mutate(
        value = value + value_fe,
        se = sqrt(se^2 + se_fe^2)
      )
  }
  else {
    coefs_init <- coefs_init %>%
      dplyr::mutate(
        value = value + value_fe
      )

    ci_level <- 0
  }


  if (ci_level > 0) {

    lower <- (1 - ci_level)/2
    upper <- 1 - lower
    mult  <- stats::qnorm(upper)

    coefs <- coefs_init %>%
      dplyr::mutate(
        lower = value - mult * se,
        upper = value + mult * se
      )

    colnames(coefs)[colnames(coefs) %in% c('lower', 'upper')] <-
      paste0(c('lower_', 'upper_'), c(lower, upper) * 100)
  }

  coefs <- coefs %>%
    dplyr::mutate(dplyr::across(\(x) is.numeric(x), round, digits = digits)) %>%
    dplyr::select(-matches('_fe$|p_value|^z$|^t$')) # remove fe related columns

  coefs
}

#' @rdname extract_random_coefs
#' @export
extract_random_coefs.glmmTMB <- function(
  model,
  re        = NULL,
  ci_level  = .95,
  digits    = 3,
  component = 'cond',
  ...
  ) {

  if (is.null(component)) {
    component <- 'cond'
    message("component argument not specified, setting to 'cond'")
  }

  random_effects <-
    extract_random_effects(model, re = re, component = component, ci_level = 0, ...)

  fixed_effects <-
    extract_fixed_effects(model, component = component, ci_level = 0, ...) %>%
    dplyr::rename(effect   = term,
                  se_fe    = se,
                  value_fe = value)

  coefs_init <- random_effects %>%
    dplyr::left_join(fixed_effects, by = 'effect') %>%
    dplyr::mutate(
      value = value + value_fe,
      se = sqrt(se^2 + se_fe^2)
    )

  if (ci_level > 0) {

    lower <- (1 - ci_level)/2
    upper <- 1 - lower
    mult  <- stats::qnorm(upper)

    coefs <- coefs_init %>%
      dplyr::mutate(
        lower = value - mult * se,
        upper = value + mult * se
      )

    colnames(coefs)[colnames(coefs) %in% c('lower', 'upper')] <-
      paste0(c('lower_', 'upper_'), c(lower, upper) * 100)
  }

  coefs <- coefs %>%
    dplyr::mutate(dplyr::across(\(x) is.numeric(x), round, digits = digits)) %>%
    dplyr::select(-matches('_fe$|p_value|^z$|^t$')) # remove fe related columns

  coefs
}

#' @rdname extract_random_coefs
#' @export
extract_random_coefs.lme <- function(
  model,
  re       = NULL,
  ci_level = NULL,
  digits   = 3,
  ...
) {

  random_effects <- extract_random_effects(model, re = re, ...)

  fixed_effects  <- extract_fixed_effects(model, ...) %>%
    dplyr::rename(effect   = term,
                  se_fe    = se,
                  value_fe = value)

  coefs <- random_effects %>%
    dplyr::left_join(fixed_effects, by = 'effect') %>%
    dplyr::mutate(
      value = value + value_fe,
    )

  coefs %>%
    dplyr::mutate(dplyr::across(\(x) is.numeric(x), round, digits = digits)) %>%
    dplyr::select(-matches('_fe$|p_value|^z$')) # remove fe related columns

}

#' @rdname extract_random_coefs
#' @export
extract_random_coefs.brmsfit <- function(
  model,
  re        = NULL,
  ci_level  = .95,
  digits    = 3,
  component = NULL,
  ...
) {

  assertthat::assert_that(
    rlang::is_installed('brms'),
    msg = 'brms package required'
  )

  # we don't call the extract* functions here as they already summarize the
  # results, and we need the draws to estimate the variance

  # do re
  # suppress warning about metadata
  suppressWarnings({
    re0 <-
      brms::as_draws_df(model, variable = '^r_', regex = TRUE) %>%
      dplyr::select(-(.chain:.draw))
  })

  random_effects <- tibble::tibble(effect = names(re0))

  random_effects <- random_effects %>%
    dplyr::mutate(
      effect    = gsub("^r_", "", effect),
      group_var = gsub("\\[.*", "", effect),
      group     = gsub(".*\\[|,.*", "", effect),
      effect    = gsub(".*,|\\]", "", effect)
    ) %>%
    cbind(t(re0))

  # do fe
  suppressWarnings({
    fe0 <- brms::as_draws_df(model, variable = '^b_', regex = TRUE) %>%
      dplyr::select(-(.chain:.draw))
  })

  fixed_effects <- tibble::tibble(effect = names(fe0))

  fixed_effects <- fixed_effects %>%
    dplyr::mutate(
      effect    = gsub("^b_", "", effect),
      group_var = gsub("\\[.*", "", effect),
      group     = gsub(".*\\[|,.*", "", effect),
      effect    = gsub(".*,|\\]", "", effect)
    ) %>%
    cbind(t(fe0))

  # previous is done to ensure samples are accurately matched, now combine
  coefs_init <- random_effects %>%
    dplyr::left_join(
      fixed_effects,
      by = 'effect',
      suffix = c('_re', '_fe')
    )

  fe_samples <- coefs_init %>%
    dplyr::select(dplyr::matches('^[0-9]+_fe'))

  re_samples <- coefs_init %>%
    dplyr::select(dplyr::matches('^[0-9]+_re'))

  coef_samples <- fe_samples + re_samples

  coefs <- random_effects %>%
    dplyr::select(group_var, group, effect) %>%
    dplyr::mutate(value = rowMeans(coef_samples),
                  se    = apply(coef_samples, 1, sd))

  if (ci_level > 0) {

    lower <- (1 - ci_level)/2
    upper <- 1 - lower

    ci <- data.frame(
      lower = apply(coef_samples, 1, quantile, prob = lower),
      upper = apply(coef_samples, 1, quantile, prob = upper)
    )

    colnames(ci) <- paste0(c('lower_', 'upper_'), c(lower, upper) * 100)

    coefs <- dplyr::bind_cols(coefs, ci)

  }

  if (!is.null(re)) {
    coefs <- coefs %>%
      dplyr::filter(group_var == re)
  }

  if (!is.null(component)) {
    coefs <- coefs %>%
      dplyr::filter(grepl(group_var, pattern = paste0('__', component, '$')))
  }

  coefs %>%
    dplyr::as_tibble() %>%
    dplyr::select(group_var, effect, dplyr::everything()) %>%
    dplyr::mutate(dplyr::across(\(x) is.numeric(x), round, digits = digits))

}


#' @rdname extract_random_coefs
#' @export
extract_random_coefs.stanreg <- function(
  model,
  re        = NULL,
  ci_level  = .95,
  digits    = 3,
  component = NULL,
  ...
) {

  assertthat::assert_that(
    rlang::is_installed('rstanarm'),
    msg = 'rstanarm package required'
  )

  # we don't call the extract* functions here as they already summarize the
  # results, and we need the draws to estimate the variance. However, since they
  # are so poorly named, much work is to be done for random effects.

  # do re
  re0 <- as.data.frame(model, regex_pars = '^b\\[')

  # this  pretty much gets us what we want first element will be blank, the
  # second the effect, the third the group_var, and the fourth the group
  effects_group_var_group = do.call(
    rbind,
    strsplit(names(re0), split = '^b\\[| |:|\\]')
  )

  random_effects <-
    data.frame(effect = effects_group_var_group[, 2], stringsAsFactors = FALSE)

  random_effects <- random_effects %>%
    dplyr::mutate(effect = remove_parens(effect),
                  group_var = effects_group_var_group[, 3],
                  group = effects_group_var_group[, 4]) %>%
    cbind(t(re0))

  # do fe
  fe0 <- as.data.frame(model, pars = names(rstanarm::fixef(model)))

  fixed_effects <- data.frame(effect = names(fe0), stringsAsFactors = FALSE)

  fixed_effects <- fixed_effects %>%
    dplyr::mutate(
      effect = remove_parens(effect),
      group_var = effect,
      group = effect
    ) %>%
    cbind(t(fe0))

  # previous is done to ensure samples are accurately matched, now combine
  coefs_init <- random_effects %>%
    dplyr::left_join(
      fixed_effects,
      by = 'effect',
      suffix = c('_re', '_fe')
    )

  fe_samples <- coefs_init %>%
    dplyr::select(dplyr::matches('^[0-9]+_fe'))

  re_samples <- coefs_init %>%
    dplyr::select(dplyr::matches('^[0-9]+_re'))

  coef_samples <- fe_samples + re_samples

  coefs <- random_effects %>%
    dplyr::select(group_var, group, effect) %>%
    dplyr::mutate(value = rowMeans(coef_samples),
                  se = apply(coef_samples, 1, sd))

  if (ci_level > 0) {

    lower <- (1 - ci_level)/2
    upper <- 1 - lower

    ci <- data.frame(
      lower = apply(coef_samples, 1, quantile, prob = lower),
      upper = apply(coef_samples, 1, quantile, prob = upper)
    )

    colnames(ci) <- paste0(c('lower_', 'upper_'), c(lower, upper) * 100)

    coefs <- dplyr::bind_cols(coefs, ci)

  }

  if (!is.null(re)) {
    coefs <- coefs %>%
      dplyr::filter(group_var == re)
  }

  if (!is.null(component)) {
    message('component not yet implemented. Skipping.')
    # coefs <- coefs %>%
    #   dplyr::filter(grepl(group_var, pattern = paste0('__', component, '$')))
  }

  coefs %>%
    dplyr::as_tibble() %>%
    dplyr::select(group_var, effect, dplyr::everything()) %>%
    dplyr::mutate(dplyr::across(\(x) is.numeric(x), round, digits = digits))

}

#' @rdname extract_random_coefs
#' @export
extract_random_coefs.gam <- function(
  model,
  re = NULL,
  ci_level = .95,
  digits = 3,
  ...
) {
  random_effects <- extract_random_effects(model, re = re, ci_level = 0, ...)

  fixed_effects  <- extract_fixed_effects(model, ci_level = 0, ...) %>%
    dplyr::rename(effect   = term,
                  se_fe    = se,
                  value_fe = value)

  # given that these aren't blups, not sure what to do here, but to keep consistent.
  coefs_init <- random_effects %>%
    dplyr::left_join(fixed_effects, by = 'effect') %>%
    dplyr::mutate(
      value = value + value_fe,
      se    = sqrt(se^2 + se_fe^2)
    )

  if (ci_level > 0) {

    lower <- (1 - ci_level)/2
    upper <- 1 - lower
    mult  <- stats::qnorm(upper)

    coefs <- coefs_init %>%
      dplyr::mutate(
        lower = value - mult * se,
        upper = value + mult * se
      )

    colnames(coefs)[colnames(coefs) %in% c('lower', 'upper')] <-
      paste0(c('lower_', 'upper_'), c(lower, upper) * 100)
  }

  coefs <- coefs %>%
    dplyr::mutate(dplyr::across(\(x) is.numeric(x), round, digits = digits)) %>%
    dplyr::select(-matches('_fe$|p_value|^z$|^t$')) # remove fe related columns

  coefs
}


#' @rdname extract_random_coefs
#' @export
extract_coef <- extract_random_coefs

#' @rdname extract_random_coefs
#' @export
extract_random_coefficients <- extract_random_coefs
