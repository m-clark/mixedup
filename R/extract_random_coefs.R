#' Extract random coefficients and their variances
#'
#' @description Fixed effect + random effects.
#'
#' @param model A merMod, nlme, brms, or glmmTMB object
#' @param re The name of the grouping variable for the random effects.
#' @param ci_level Where possible, confidence level < 1, typically above 0.90. A value of 0 will
#'   not report it. Default is .95. Not applicable to nlme objects.
#' @param digits Rounding. Default is 3.
#' @param component For glmmTMB objects, which of the two components 'cond' or
#'   'zi' to select. Default is 'cond'. For brmsfit objects, this can filter
#'   results to a certain part of the output, e.g. 'sigma' or 'zi' of
#'   distributional models, or a specific outcome of a multivariate model.  In
#'   this case `component` is a regular expression that ends the name of the
#'   parameters of the output (e.g. '__component').
#' @param ... Other arguments specific to the method. Unused at present.
#'
#' @details Returns a data frame with random coefficients, a.k.a. random
#'   intercepts and random slopes, and their standard errors. Note that the
#'   standard errors assume independence of the conditional variance and the
#'   fixed-effects variance, thus the standard errors are the sum of variances
#'   for the respective fixed and random effects. See Bolker's demo
#'   \href{https://stackoverflow.com/questions/26198958/extracting-coefficients-and-their-standard-error-from-lme}{here}
#'   and additional discussion at the
#'   \href{https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#confidence-intervals-on-conditional-meansblupsrandom-effects}{GLMM
#'   FAQ}. As noted there, this assumption may not be appropriate, and if you
#'   are really interested in an accurate uncertainty estimate you should
#'   probably use `brms`.
#'
#'   For more complex models that include multiple outcomes/categories, this
#'   function likely will not work at present, as naming conventions are not
#'   consistent. I will possibly be able to update this in the future.
#'
#'
#'   The `nlme` package only provides the coefficients no estimated variance, so this
#'   function doesn't add to what you get from basic functionality for those
#'   models.  In addition, `nlme` adds all random effects to the fixed effects,
#'   whereas `lme4` and others only add the effects requested.
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
  re = NULL,
  ci_level = .95,
  digits = 3,
  component = NULL,
  ...
) {

  if (!inherits(model, c('merMod', 'glmmTMB', 'gam', 'lme', 'brmsfit', 'stanreg')))
    stop('This is not a supported model class.')

  if (ci_level < 0 | ci_level >= 1)
    stop('Nonsensical confidence level for ci_level. Must be between 0 and 1.')

  UseMethod('extract_random_coefs')
}

#' @rdname extract_random_coefs
#' @export
extract_random_coefs.merMod <- function(
  model,
  re = NULL,
  ci_level = .95,
  digits = 3,
  # component = NULL,
  ...
) {

  random_effects <- extract_random_effects(model, re = re)

  fixed_effects  <- extract_fixed_effects(model) %>%
    dplyr::rename(effect = term,
                  se_fe = se,
                  value_fe = value)

  coefs_init <- random_effects %>%
    dplyr::left_join(fixed_effects, by = 'effect') %>%
    dplyr::mutate(
      value = value + value_fe,
      se = sqrt(se^2 + se_fe^2)
    ) %>%
    dplyr::select(group_var, effect, group, value, se)

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
    dplyr::mutate_if(is.numeric, round, digits = digits)

  coefs
}

#' @rdname extract_random_coefs
#' @export
extract_random_coefs.glmmTMB <- function(
  model,
  re = NULL,
  ci_level = .95,
  digits = 3,
  component = 'cond',
  ...
  ) {

  if (is.null(component)) component <- 'cond'

  random_effects <-
    extract_random_effects(model, re = re, component = component)

  fixed_effects <-
    extract_fixed_effects(model, component = component) %>%
    dplyr::rename(effect = term,
                  se_fe = se,
                  value_fe = value)

  coefs_init <- random_effects %>%
    dplyr::left_join(fixed_effects, by = 'effect') %>%
    dplyr::mutate(
      value = value + value_fe,
      se = sqrt(se^2 + se_fe^2)
    ) %>%
    dplyr::select(group_var, effect, group, value, se)

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
    dplyr::mutate_if(is.numeric, round, digits = digits)

  coefs
}

#' @rdname extract_random_coefs
#' @export
extract_random_coefs.lme <- function(
  model,
  re = NULL,
  ci_level = NULL,
  digits = 3,
  # component = NULL,
  ...
) {

  random_effects <- extract_random_effects(model, re = re)

  fixed_effects  <- extract_fixed_effects(model) %>%
    dplyr::rename(effect = term,
                  se_fe = se,
                  value_fe = value)

  coefs <- random_effects %>%
    dplyr::left_join(fixed_effects, by = 'effect') %>%
    dplyr::mutate(
      value = value + value_fe,
    ) %>%
    dplyr::select(group_var, effect, group, value)

  coefs %>%
    dplyr::mutate_if(is.numeric, round, digits = digits)

}

#' @rdname extract_random_coefs
#' @export
extract_random_coefs.brmsfit <- function(
  model,
  re = NULL,
  ci_level = .95,
  digits = 3,
  component = NULL,
  ...
) {

  if (!is_package_installed('brms'))
    stop('brms package required', call. = FALSE)

  # we don't call the extract* functions here as they already summarize the
  # results, and we need the draws to estimate the variance

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
    coefs <- coefs %>%
      dplyr::filter(grepl(group_var, pattern = paste0('__', component, '$')))
  }

  coefs %>%
    dplyr::as_tibble() %>%
    dplyr::select(group_var, effect, dplyr::everything()) %>%
    dplyr::mutate_if(is.numeric, round, digits = digits)

}


#' @rdname extract_random_coefs
#' @export
extract_random_coefs.stanreg <- function(
  model,
  re = NULL,
  ci_level = .95,
  digits = 3,
  component = NULL,
  ...
) {

  if (!is_package_installed('rstanarm'))
    stop('rstanarm package required', call. = FALSE)

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
    dplyr::mutate_if(is.numeric, round, digits = digits)

}

#' @rdname extract_random_coefs
#' @export
extract_random_coefs.gam <- function(
  model,
  re = NULL,
  ci_level = .95,
  digits = 3,
  # component = NULL,
  ...
) {
  random_effects <- extract_random_effects(model, re = re)

  fixed_effects  <- extract_fixed_effects(model) %>%
    dplyr::rename(effect = term,
                  se_fe = se,
                  value_fe = value)

  # given that these aren't blups, not sure what to do here, but to keep consistent.
  coefs_init <- random_effects %>%
    dplyr::left_join(fixed_effects, by = 'effect') %>%
    dplyr::mutate(
      value = value + value_fe,
      se = sqrt(se^2 + se_fe^2)
    ) %>%
    dplyr::select(group_var, effect, group, value, se)

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
    dplyr::mutate_if(is.numeric, round, digits = digits)

  coefs
}
