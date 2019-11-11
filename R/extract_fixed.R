#' Extract fixed effects
#'
#' @inheritParams extract_vc
#' @param ci_args A list of additional arguments to the corresponding confint method. Default (\code{list(method = 'Wald')}) is to change the default CI method for speedier results.
#' @param ... Other arguments to pass. Nothing at present.
#' @details Essentially duplicates the \code{broom::tidy} approach with minor
#'   name changes.
#'
#' @return A data.frame with the fixed effects and associated statistics
#'
#' @seealso \code{\link[broom:tidy.merMod]{tidy.merMod}}
#'
#' @examples
#' library(lme4)
#' library(mixedup)
#'
#' lmer_mod <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
#'
#' extract_fixed(lmer_mod)
#'
#' @export
extract_fixed <- function(
  model,
  ci_level = .95,
  ci_args = NULL,
  digits = 3,
  ...
) {
  if (!inherits(model, c('merMod', 'glmmTMB', 'lme', 'brmsfit')))
    stop('This only works for model objects from lme4, glmmTMB, brms, and nlme.')

  if (ci_level < 0 | ci_level >= 1)
    stop('Nonsensical confidence level for ci_level. Must be between 0 and 1.')

  UseMethod('extract_fixed')
}

#' @export
extract_fixed.merMod <-
  function(
    model,
    ci_level = .95,
    ci_args = list(method = 'Wald'),
    digits = 3,
    ...
  ) {

    fe <- stats::coef(summary(model))

    colnames(fe) =  c('value', 'se', 't')

    if (ci_level > 0) {

      lower = (1 - ci_level)/2
      upper = ci_level + lower

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

    fe
}

#' @export
extract_fixed.glmmTMB <-
  function(
    model,
    ci_level = .95,
    ci_args = list(method = 'Wald'),
    digits = 3,
    component = 'cond',
    ...
  ) {

    fe <- stats::coef(summary(model))[[component]]

    colnames(fe) =  c('value', 'se', 'z', 'p_value')

    if (ci_level > 0) {

      lower = (1 - ci_level)/2
      upper = ci_level + lower

      ci <- do.call(
        confint,
        c(
          list(
            object = model,
            parm = seq(nrow(fe)),  # think there is a glmmTMB bug here if you just do 'beta_'
            level = ci_level,
            component = component,
            estimate = FALSE
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

    fe
}

#' @export
extract_fixed.lme <-
  function(
    model,
    ci_level = .95,
    ci_args = list(method = 'Wald'),
    digits = 3,
    ...
  ) {

    fe <- as.data.frame(stats::coef(summary(model))) %>%
      dplyr::select(-DF)

    colnames(fe) =  c('value', 'se', 't', 'p_value')

    if (ci_level > 0) {

      lower = (1 - ci_level)/2
      upper = ci_level + lower

      # nlme does not have a confint method
      mult <- qnorm(upper)

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

    fe
}
