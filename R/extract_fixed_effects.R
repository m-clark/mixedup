#' Extract fixed effects
#'
#' @description Extract fixed effects parameters, variance estimates etc.
#'
#' @inheritParams extract_vc
#'
#' @param exponentiate Exponentiate the fixed-effect coefficient estimates and
#'   confidence intervals (common for logistic regression). If `TRUE`, also scales
#'   the standard errors by the exponentiated coefficient, transforming them to
#'   the new scale.
#' @param p_value For `lme4` models, one of 'Wald' or 'KR'. See details.
#'
#' @details Essentially duplicates the `broom::tidy` approach with minor
#'   name changes.  For `lme4`, 'Wald' p-values are provided lmer models for
#'   consistency with others, but there is [much issue with
#'   them](https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#what-are-the-p-values-listed-by-summaryglmerfit-etc.-are-they-reliable),
#'   especially for low N/small numbers of groups.  The Kenward-Roger is also
#'   available if the `pbkrtest` package is installed (experimental). For either
#'   case, Only the p-value from the process is provide, all other output is
#'   default provided `lme4` without adjustment.
#'
#' @note For `nlme`, this is just a multiplier based on the estimated standard
#'   error and critical value for the `ci_level`.
#'
#' @return A data.frame with the fixed effects and associated statistics.
#'
#' @seealso
#'   [broom.mixed::tidy.merMod()],
#'   [broom.mixed::tidy.glmmTMB()],
#'   [broom.mixed::tidy.lme()],
#'   [broom.mixed::tidy.brmsfit()]
#'
#' @examples
#' library(lme4)
#' library(mixedup)
#'
#' lmer_mod <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
#'
#' extract_fixed_effects(lmer_mod)
#'
#' @family extract
#'
#' @importFrom stats qnorm qt pt
#'
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
    stop('This is not a supported model class.')

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
    ...,
    p_value = 'Wald'
  ) {

    if (!p_value %in% c('Wald', 'Satterthwaite', 'KR')) {
      warning("p_value must be one of 'Wald' or 'KR', switching to 'Wald'")
      p_value = 'Wald'
    }


    # do term now otherwise you can lose rownames if ci_level = 0
    fe <- data.frame(stats::coef(summary(model))) %>%
      dplyr::mutate(term = rownames(.)) %>%
      dplyr::select(term, dplyr::everything())

    # edge case of a no covariate, no intercept model
    if (nrow(fe) == 0) return(NULL)

    if (inherits(model, 'glmerMod')) {
      colnames(fe) <- c('term', 'value', 'se', 'z', 'p_value')
    }
    else {
      colnames(fe) <- c('term', 'value', 'se', 't')

      # Note, Satterthwaite required calling lmerTest::as_lmerModTest which has
      # historical issues being used inside other functions that are still
      # evidently present, so left out for now.
      # if (p_value == 'Satterthwaite') {
        # if (!is_package_installed('lmerTest')) {
        #   p_value <- 'Wald'
        #   warning('lmerTest package not installed. Switching p_value to Wald.')
        # } else {
        #   coerced_model <- lmerTest::as_lmerModLmerTest(model)
        #   ps <- coef(summarycoerced_model())[, 'Pr(>|t|)']
        # }
      # }
      if (p_value == 'KR') {
        if (!is_package_installed('pbkrtest')) {
          p_value <- 'Wald'
          warning('pbkrtest required for Kenward-Roger. Switching p_value to Wald.')
        } else {
          L <- diag(nrow(fe))
          ps <-
            apply(L, 2, function(l)
              pbkrtest::KRmodcomp(model, matrix(l, ncol = 2))$stats$p.value)
          fe <-
            dplyr::mutate(fe, p_value = ps)
        }
      }
      else if (p_value == 'Wald')
        fe <- dplyr::mutate(fe, p_value = 2 * pt(abs(t), Inf, lower.tail = FALSE))

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

    if (exponentiate) {
      fe <- fe %>%
        dplyr::mutate_at(
          dplyr::vars(dplyr::matches('^value|^low|^upp')),
          exp
        ) %>%
        dplyr::mutate(se = se * value)
    }

    # cleanup names, round, etc.
    fe <- fe %>%
      dplyr::mutate_if(is.numeric, round, digits = digits) %>%
      dplyr::mutate(term = gsub(term,
                                pattern = '[\\(,\\)]',
                                replacement = '')) %>%
      dplyr::as_tibble()

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
    ...,
    exponentiate = FALSE,
    component = 'cond'
  ) {

    if (!component %in% c('cond', 'zi', 'disp')) {
      stop('component must be one of "cond", "zi", "disp".')
    }

    # if don't use summary.glmmTMB, will fail if glmmTMB not loaded, but it's
    # not exported, requiring ::: which isn't good.  will work around at some
    # point.
    # do term now otherwise you can lose rownames if ci_level = 0
    fe <- data.frame(stats::coef(summary(model))[[component]]) %>%
      dplyr::mutate(term = rownames(.)) %>%
      dplyr::select(term, dplyr::everything())

    colnames(fe) <- c('term', 'value', 'se', 'z', 'p_value')

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

    if (exponentiate) {
      fe <- fe %>%
        dplyr::mutate_at(
          dplyr::vars(dplyr::matches('^value|^low|^upp')),
          exp
        ) %>%
        dplyr::mutate(se = se * value)
    }

    # cleanup names, round, etc.
    fe <- fe %>%
      dplyr::mutate_if(is.numeric,round, digits = digits) %>%
      dplyr::mutate(term = gsub(term,
                                pattern = '[\\(,\\)]',
                                replacement = '')) %>%
      dplyr::as_tibble()

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
      dplyr::mutate(term = rownames(.)) %>%
      dplyr::select(term, dplyr::everything(), -DF)

    colnames(fe) <- c('term', 'value', 'se', 'z', 'p_value')

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

    if (exponentiate) {
      fe <- fe %>%
        dplyr::mutate_at(
          dplyr::vars(dplyr::matches('^value|^low|^upp')),
          exp
        ) %>%
        dplyr::mutate(se = se * value)
    }

    # cleanup names, round, etc.
    fe <- fe %>%
      dplyr::mutate_if(is.numeric, round, digits = digits) %>%
      dplyr::mutate(term = gsub(term,
                                pattern = '[\\(,\\)]',
                                replacement = '')) %>%
      dplyr::as_tibble()

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
    ...,
    component = NULL
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
      dplyr::mutate(term = rownames(.)) %>%
      dplyr::rename(
        value = Estimate,
        se = Est.Error
      )

    if (exponentiate) {
      fe <- fe %>%
        dplyr::mutate_at(
          dplyr::vars(dplyr::matches('^value|^low|^upp')),
          exp
        ) %>%
        dplyr::mutate(se = se * value)
    }

    # cleanup names, round, etc.
    fe <- fe %>%
      dplyr::mutate_if(is.numeric, round, digits = digits) %>%
      dplyr::mutate(term = gsub(term,
                                pattern = '[\\(,\\)]',
                                replacement = '')) %>%
      dplyr::select(term, dplyr::everything()) %>%
      dplyr::as_tibble()

    if (!is.null(component)) {
      fe <- fe %>%
        dplyr::filter(grepl(term, pattern = paste0('^', component)))
    }

    fe
  }

#' @export
#' @rdname extract_fixed_effects
extract_fixed_effects.stanreg <-
  function(
    model,
    ci_level = .95,
    ci_args = NULL,
    digits = 3,
    exponentiate = FALSE,
    ...,
    component = NULL
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

    model_summary <-
      summary(model, pars = c('alpha', 'beta'), probs = probs)


    fe <- data.frame(model_summary) %>%
      dplyr::select(-mcse, -n_eff, -Rhat)

    colnames(fe)[3:4] = paste0(c('lower_', 'upper_'), c(lower, upper) * 100)

    fe <- fe %>%
      dplyr::mutate(term = rownames(.)) %>%
      dplyr::rename(
        value = mean,
        se = sd
      )

    if (exponentiate) {
      fe <- fe %>%
        dplyr::mutate_at(
          dplyr::vars(dplyr::matches('^value|^low|^upp')),
          exp
        ) %>%
        dplyr::mutate(se = se * value)
    }

    fe <- fe %>%
      dplyr::mutate_if(is.numeric, round, digits = digits) %>%
      dplyr::mutate(term = gsub(term,
                                pattern = '[\\(,\\)]',
                                replacement = '')) %>%
      dplyr::select(term, dplyr::everything()) %>%
      dplyr::as_tibble()



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
      dplyr::mutate(term = remove_parens(rownames(.))) %>%
      dplyr::mutate_if(is.numeric, round, digits = digits)

    if (exponentiate) {
      fe <- fe %>%
        dplyr::mutate_at(
          dplyr::vars(dplyr::matches('^value|^low|^upp')),
          exp
        ) %>%
        dplyr::mutate(se = se * value)
    }

    fe <- fe %>%
      dplyr::select(term, dplyr::everything()) %>%
      dplyr::as_tibble()

    fe
  }
