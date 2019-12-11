#' Extract fixed effects
#'
#' @inheritParams extract_vc
#' @param exponentiate Exponentiate the fixed-effect coefficient estimates and
#'   confidence intervals (common for logistic regression). If TRUE, also scales
#'   the standard errors by the exponentiated coefficient, transforming them to
#'   the new scale.
#'
#' @details Essentially duplicates the \code{broom::tidy} approach with minor
#'   name changes.  The package may or may not provide p-values by default.
#'
#' @return A data.frame with the fixed effects and associated statistics.
#'
#' @note For nlme, this is just a multiplier based on the estimated standard
#'   error and critical value for the \code{ci_level}.
#'
#' @seealso \code{\link[broom:tidy.merMod]{tidy.merMod}},
#'   \code{\link[broom.mixed:tidy.glmmTMB]{tidy.glmmTMB}},
#'   \code{\link[broom.mixed:tidy.brmsfit]{tidy.brmsfit}}
#'
#' @examples
#' library(lme4)
#' library(mixedup)
#'
#' lmer_mod <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
#'
#' extract_fixed_effects(lmer_mod)
#' @importFrom stats qnorm qt
#' @export
extract_fixed_effects <- function(
  model,
  ci_level = .95,
  ci_args = NULL,
  digits = 3,
  exponentiate = FALSE,
  ...
) {
  if (!inherits(model,
                c('merMod', 'glmmTMB', 'lme', 'gam', 'stanreg', 'brmsfit'))
  )
    stop('This is not a supported model type.')

  if (ci_level < 0 | ci_level >= 1)
    stop('Nonsensical confidence level for ci_level. Must be between 0 and 1.')

  UseMethod('extract_fixed_effects')
}

#' @rdname extract_fixed_effects
#' @export
extract_fixed_effects.merMod <-
  function(
    model,
    ci_level = .95,
    ci_args = list(method = 'Wald'),
    digits = 3,
    exponentiate = FALSE,
    ...
  ) {

    fe <- data.frame(stats::coef(summary(model)))

    if (inherits(model, 'glmerMod')) {
      colnames(fe) <- c('value', 'se', 'z', 'p_value')
    }
    else {
      colnames(fe) <- c('value', 'se', 't')
    }

    if (ci_level > 0) {

      lower <- (1 - ci_level)/2
      upper <- 1 - lower

      ci <- do.call(
        confint,
        c(
          list(
            object = model,
            parm = 'beta_',
            level = ci_level,
            oldNames = FALSE
          ),
          ci_args
        )
      )

      colnames(ci) <- paste0(c('lower_', 'upper_'), c(lower, upper) * 100)

      fe <- data.frame(fe, ci)
    }

    fe <- fe %>%
      dplyr::mutate_all(round, digits = digits) %>%
      dplyr::mutate(term = gsub(rownames(fe),
                                pattern = '[\\(,\\)]',
                                replacement = '')) %>%
      dplyr::select(term, dplyr::everything()) %>%
      dplyr::as_tibble()

    if (exponentiate) {
      fe <- fe %>%
        dplyr::mutate_at(
          dplyr::vars(dplyr::matches('^value|^low|^upp')),
          exp
        ) %>%
        dplyr::mutate(se = se * value)
    }

    fe
}

#' @rdname extract_fixed_effects
#' @export
extract_fixed_effects.glmmTMB <-
  function(
    model,
    ci_level = .95,
    ci_args = NULL,
    digits = 3,
    exponentiate = FALSE,
    component = 'cond',
    ...
  ) {

    if (!component %in% c('cond', 'zi', 'disp')) {
      stop('component must be one of "cond", "zi", "disp".')
    }

    # if don't use summary.glmmTMB, will fail if glmmTMB not loaded, but it's
    # not exported, requiring ::: which isn't good.  will work around at some
    # point.
    fe <- data.frame(stats::coef(summary(model))[[component]])

    colnames(fe) <- c('value', 'se', 'z', 'p_value')

    if (ci_level > 0) {

      lower <- (1 - ci_level)/2
      upper <- 1 - lower

      # glmmTMB has some issues with confint (see
      # https://github.com/glmmTMB/glmmTMB/issues/401 for example), and at least
      # one tested case with 3 group vars and multiple random effects. Also fails with
      # parm = 'beta_' and probably other places.
      ci <- tryCatch(
        do.call(confint,
                c(
                  list(
                    object = model,
                    parm = seq(nrow(fe)),
                    # think there is a glmmTMB bug here if you just do 'beta_'
                    level = ci_level,
                    component = component,
                    estimate = FALSE
                  ),
                  ci_args
                )),
        error = function(c) {
          msg <- conditionMessage(c)
          invisible(structure(msg, class = "try-error"))
        }
      )

      if (inherits(ci, 'try-error')) {
        warning('Intervals could not be computed. Returning ci based on se.
                \nIf se is NaN, check random effects for zero variance estimates.')
        mult <- stats::qnorm(upper)

        ci <- data.frame(
          lower = fe$value - mult * fe$se,
          upper = fe$value + mult * fe$se
        )
      }

      colnames(ci) <- paste0(c('lower_', 'upper_'), c(lower, upper) * 100)

      fe <- data.frame(fe, ci)

    }

    fe <- fe %>%
      dplyr::mutate_all(round, digits = digits) %>%
      dplyr::mutate(term = gsub(rownames(fe),
                                pattern = '[\\(,\\)]',
                                replacement = '')) %>%
      dplyr::select(term, dplyr::everything()) %>%
      dplyr::as_tibble()

    if (exponentiate) {
      fe <- fe %>%
        dplyr::mutate_at(
          dplyr::vars(dplyr::matches('^value|^low|^upp')),
          exp
        ) %>%
        dplyr::mutate(se = se * value)
    }

    fe
}

#' @rdname extract_fixed_effects
#' @export
extract_fixed_effects.lme <-
  function(
    model,
    ci_level = .95,
    ci_args = list(method = 'Wald'),
    digits = 3,
    exponentiate = FALSE,
    ...
  ) {

    fe <- as.data.frame(stats::coef(summary(model)))
    dfs <- fe$DF
    fe <- fe %>%
      dplyr::select(-DF)

    colnames(fe) =  c('value', 'se', 't', 'p_value')

    if (ci_level > 0) {

      lower = (1 - ci_level)/2
      upper = 1 - lower

      # nlme does not have a confint method
      mult <- stats::qt(upper, dfs)

      ci <- data.frame(
        lower = fe$value - mult * fe$se,
        upper = fe$value + mult * fe$se
      )

      colnames(ci) <- paste0(c('lower_', 'upper_'), c(lower, upper) * 100)

      fe <- data.frame(fe, ci)
    }

    fe <- fe %>%
      dplyr::mutate_all(round, digits = digits) %>%
      dplyr::mutate(term = gsub(rownames(fe),
                                pattern = '[\\(,\\)]',
                                replacement = '')) %>%
      dplyr::select(term, dplyr::everything()) %>%
      dplyr::as_tibble()

    if (exponentiate) {
      fe <- fe %>%
        dplyr::mutate_at(
          dplyr::vars(dplyr::matches('^value|^low|^upp')),
          exp
        ) %>%
        dplyr::mutate(se = se * value)
    }

    fe
}

#' @rdname extract_fixed_effects
#' @export
extract_fixed_effects.brmsfit <-
  function(
    model,
    ci_level = .95,
    ci_args = NULL,
    digits = 3,
    exponentiate = FALSE,
    component = NULL,
    ...
  ) {

    if (ci_level == 0) {
      message('ci automatically provided for brms fixed effects. Setting ci_level to .95.')
      ci_level <- .95
    }

    lower <- (1 - ci_level)/2
    upper <- 1 - lower
    probs <- c(lower, upper)

    fe <- data.frame(brms::fixef(model, probs = probs))

    colnames(fe)[3:4] = paste0(c('lower_', 'upper_'), c(lower, upper) * 100)

    fe <- fe %>%
      dplyr::rename(
        value = Estimate,
        se = Est.Error
      ) %>%
      dplyr::mutate_all(round, digits = digits) %>%
      dplyr::mutate(term = gsub(rownames(fe),
                                pattern = '[\\(,\\)]',
                                replacement = '')) %>%
      dplyr::select(term, dplyr::everything()) %>%
      dplyr::as_tibble()

    if (exponentiate) {
      fe <- fe %>%
        dplyr::mutate_at(
          dplyr::vars(dplyr::matches('^value|^low|^upp')),
          exp
        ) %>%
        dplyr::mutate(se = se * value)
    }

    if (!is.null(component)) {
      fe <- fe %>%
        dplyr::filter(grepl(term, pattern = paste0('^', component)))
    }

    fe
  }

#' @export
#' @rdName extract_fixed_effects
extract_fixed_effects.stanreg <-
  function(
    model,
    ci_level = .95,
    ci_args = NULL,
    digits = 3,
    exponentiate = FALSE,
    component = NULL,
    ...
  ) {

    if (inherits(model, 'stanmvreg'))
      stop('Multivariate models not supported yet.') # note pull y_vars attr from summary object as well as y* names

    if (ci_level == 0) {
      message('ci automatically provided for rstanarm fixed effects. Setting ci_level to .95.')
      ci_level <- .95
    }

    lower <- (1 - ci_level)/2
    upper <- 1 - lower
    probs <- c(lower, upper)

    fe <- rstanarm:::summary.stanreg(model, pars = c('alpha', 'beta'), probs = probs)
    fe <- data.frame(fe) %>%
      select(-mcse, -n_eff, -Rhat)

    colnames(fe)[3:4] = paste0(c('lower_', 'upper_'), c(lower, upper) * 100)

    fe <- fe %>%
      dplyr::rename(
        value = mean,
        se = sd
      ) %>%
      dplyr::mutate_all(round, digits = digits) %>%
      dplyr::mutate(term = gsub(rownames(fe),
                                pattern = '[\\(,\\)]',
                                replacement = '')) %>%
      dplyr::select(term, dplyr::everything()) %>%
      dplyr::as_tibble()

    if (exponentiate) {
      fe <- fe %>%
        dplyr::mutate_at(
          dplyr::vars(dplyr::matches('^value|^low|^upp')),
          exp
        ) %>%
        dplyr::mutate(se = se * value)
    }

    if (!is.null(component)) {
      warning('component not yet implemented for stanreg objects. Ignoring.')
      # fe <- fe %>%
      #   dplyr::filter(grepl(term, pattern = paste0('^', component)))
    }

    fe
  }

#' @rdname extract_fixed_effects
#' @export
extract_fixed_effects.gam <-
  function(
    model,
    ci_level = .95,
    ci_args = list(method = 'Wald'),
    digits = 3,
    exponentiate = FALSE,
    ...
  ) {

    fe <- data.frame(summary(model)$p.table)

    colnames(fe) =  c('value', 'se', 't', 'p_value')

    # no confint.gam
    if (ci_level > 0) {

      lower <- (1 - ci_level)/2
      upper <- 1 - lower
      nu <- model$df.residual
      mult <- stats::qt(upper, nu)

      ci <- data.frame(
        lower = fe$value - mult * fe$se,
        upper = fe$value + mult * fe$se
      )

      colnames(ci) <- paste0(c('lower_', 'upper_'), c(lower, upper) * 100)

      fe <- data.frame(fe, ci)
    }

    fe <- fe %>%
      dplyr::mutate_all(round, digits = digits) %>%
      dplyr::mutate(term = gsub(rownames(fe),
                                pattern = '[\\(,\\)]',
                                replacement = '')) %>%
      dplyr::select(term, dplyr::everything()) %>%
      dplyr::as_tibble()

    if (exponentiate) {
      fe <- fe %>%
        dplyr::mutate_at(
          dplyr::vars(dplyr::matches('^value|^low|^upp')),
          exp
        ) %>%
        dplyr::mutate(se = se * value)
    }

    fe
  }
