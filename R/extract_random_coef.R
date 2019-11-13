#' Extract random coefficients and their variances
#'
#' @param model A merMod or glmmTMB object
#' @param re The name of the grouping variable for the random effects.
#' @param component Which of the three components 'cond', 'zi' or 'other' to
#'   select for a glmmTMB model. Default is 'cond'. Minimal testing on other
#'   options.
#' @param ci_level Where possible, confidence level < 1, typically above 0.90. A value of 0 will
#'   not report it. Default is .95.
#' @param digits Rounding. Default is 3.
#' @param ... Other arguments specific to the method. Unused at present.
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
extract_random_coef <- function(
  model,
  re = NULL,
  ci_level = .95,
  # component,
  digits = 3,
  ...
) {

  if (!inherits(model, c('merMod', 'glmmTMB', 'lme', 'brmsfit')))
    stop('This only works for model objects from lme4, glmmTMB, brms, and nlme.')

  if (ci_level < 0 | ci_level >= 1)
    stop('Nonsensical confidence level for ci_level. Must be between 0 and 1.')

  UseMethod('extract_random_coef')
}

#' @rdname extract_random_coef
#' @export
extract_random_coef.merMod <- function(
  model,
  re = NULL,
  ci_level = .95,
  digits = 3,
  ...
) {

  random_effects <- extract_random_effects(model, re = re)

  fixed_effects  <- extract_fixed(model) %>%
    dplyr::rename(effect = term,
                  se_fe = se,
                  value_fe = value)

  coefs_init <- random_effects %>%
    dplyr::left_join(fixed_effects, by = 'effect') %>%
    dplyr::mutate(
      coef = value + value_fe,
      se = sd + se_fe
    ) %>%
    dplyr::select(group_var, effect, group, coef, se)

  if (ci_level > 0) {

    lower = (1 - ci_level)/2
    upper = 1 - lower
    mult <- stats::qnorm(upper)

    coefs <- coefs_init %>%
      dplyr::mutate(
        lower = coef - mult * se,
        upper = coef + mult * se
      )

    colnames(coefs)[colnames(coefs) %in% c('lower', 'upper')] <-
      paste0(c('lower_', 'upper_'), c(lower, upper) * 100)
  }

  coefs <- coefs %>%
    dplyr::mutate_if(is.numeric, round, digits = digits)

  coefs
}

#' @rdname extract_random_coef
#' @export
extract_random_coef.glmmTMB <- function(
  model,
  re = NULL,
  ci_level = .95,
  # component = 'cond',
  digits = 3,
  ...
  ) {

  random_effects <-
    extract_random_effects(model, re = re, component = component)

  fixed_effects  <- extract_fixed(model, component = component) %>%
    dplyr::rename(effect = term,
                  se_fe = se,
                  value_fe = value)

  coefs_init <- random_effects %>%
    dplyr::left_join(fixed_effects, by = 'effect') %>%
    dplyr::mutate(
      coef = value + value_fe,
      se = sd + se_fe
    ) %>%
    dplyr::select(group_var, effect, group, coef, se)

  if (ci_level > 0) {

    lower = (1 - ci_level)/2
    upper = 1 - lower
    mult <- stats::qnorm(upper)

    coefs <- coefs_init %>%
      dplyr::mutate(
        lower = coef - mult * se,
        upper = coef + mult * se
      )

    colnames(coefs)[colnames(coefs) %in% c('lower', 'upper')] <-
      paste0(c('lower_', 'upper_'), c(lower, upper) * 100)
  }

  coefs <- coefs %>%
    dplyr::mutate_if(is.numeric, round, digits = digits)

  coefs
}

#' @rdname extract_random_coef
#' @export
extract_random_coef.lme <- function(
  model,
  re = NULL,
  digits = 3,
  ...
) {

  random_effects <- extract_random_effects(model, re = re)

  fixed_effects  <- extract_fixed(model) %>%
    dplyr::rename(effect = term,
                  se_fe = se,
                  value_fe = value)

  coefs <- random_effects %>%
    dplyr::left_join(fixed_effects, by = 'effect') %>%
    dplyr::mutate(
      coef = value + value_fe,
    ) %>%
    dplyr::select(group_var, effect, group, coef)

  coefs %>%
    dplyr::mutate_if(is.numeric, round, digits = digits)

}

#' @rdname extract_random_coef
#' @export
extract_random_coef.brmsfit <- function(
  model,
  re = NULL,
  ci_level = .95,
  digits = 3,
  ...
) {

  if (!is_package_installed('brms'))
    stop('brms package required', call. = FALSE)

  # we don't call the extract* functions directly as they already summarize the
  # results

  # do re
  re0 <- brms::posterior_samples(model, pars = '^r_')

  random_effects <- data.frame(effect = names(re0), stringsAsFactors = FALSE)

  random_effects <- random_effects %>%
    dplyr::mutate(
      effect = gsub("^r_", "", effect),
      group_var = gsub("\\[.*", "", effect),
      group = gsub(".*\\[|,.*", "", effect),
      effect = gsub(".*,|\\]", "", effect)) %>%
    cbind(t(re0))

  # do fe
  fe0 <- brms::posterior_samples(model, pars = '^b_')

  fixed_effects <- data.frame(effect = names(fe0), stringsAsFactors = FALSE)

  fixed_effects <- fixed_effects %>%
    dplyr::mutate(
      effect = gsub("^b_", "", effect),
      group_var = gsub("\\[.*", "", effect),
      group = gsub(".*\\[|,.*", "", effect),
      effect = gsub(".*,|\\]", "", effect)) %>%
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
                  sd = apply(coef_samples, 1, sd))

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

  coefs %>%
    dplyr::select(group_var, effect, dplyr::everything()) %>%
    dplyr::mutate_if(is.numeric, round, digits = digits)

}
