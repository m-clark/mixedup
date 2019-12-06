#' Extract correlation structure
#' @description Extract residual correlation structure for nlme, brms, and
#' potentially other models.
#' @inheritParams extract_het_var
#' @param ci_level confidence level < 1, typically above 0.90. A value of 0 will
#'   not report it. Default is .95.
#'
#' @details For models with correlation, i.e. that contain something like
#'   corAR1(form = ~time) for \code{nlme} or \code{brms} models with an
#'   \code{autocor} argument.  For more see this
#'   \href{https://bbolker.github.io/mixedmodels-misc/notes/corr_braindump.html}{'braindump'
#'    from Ben Bolker}.
#'
#'    Spatial models have been tested for brms, so should work as well.
#'
#' @return For nlme models, a data frame of the estimates. For brms, the
#'   parameters and related uncertainty, similar to
#'   \link{extract_fixed_effects}.
#'
#' @export
extract_cor_structure <- function(
  model,
  digits = 3,
  ...
) {
  if (!inherits(model, c('lme', 'brmsfit')))
    stop('This only works for model objects from nlme and brms at present.')

  UseMethod('extract_cor_structure')
}

#' @rdname extract_cor_structure
#' @export
extract_cor_structure.lme <- function(
  model,
  digits = 3,
  ...
) {

  cs <- model$modelStruct$corStruct

  if (inherits(cs,
               c(
                 'corAR1',
                 'corARMA',
                 'corCAR',
                 'corCompSymm',
                 'corSpher',
                 'corLin',
                 'corExp',
                 'corRatio',
                 'corGaus'
               ))) {

    cs <- t(round(coef(cs, unconstrained = FALSE), digits = digits))
    dplyr::as_tibble(cs)

  } else if (inherits(cs, c('corSymm'))) {

    # get the largest matrix; first if balanced
    cor_matrices <- as.matrix(cs)

    res <- as.data.frame(cor_matrices[[which.max(attr(cs, 'Dim')$len)[1]]])

    rownames(res) <- colnames(res)

    round(res, digits = digits)

  } else {
    message('This correlation structure may not be supported')
    cs <- t(round(coef(cs, unconstrained = FALSE), digits = digits))

    dplyr::as_tibble(cs)
  }

}


#' @rdname extract_cor_structure
#' @export
extract_cor_structure.brmsfit <- function(
  model,
  digits = 3,
  ...,
  ci_level = .95
) {

  cor_par <- summary(model, prob = ci_level)$cor_par

  lower <- (1 - ci_level)/2
  upper <- 1 - lower

  # rename intervals
  cor_par <- dplyr::as_tibble(cor_par, rownames = 'parameter') %>%
    dplyr::rename_at(dplyr::vars(dplyr::matches('l-')), function(x)
      paste0('lower_', lower*100)) %>%
    dplyr::rename_at(dplyr::vars(dplyr::matches('u-')), function(x)
      paste0('upper_', upper*100))

  # more cleanup/return
  cor_par %>%
    dplyr::select(parameter, Estimate, Est.Error,
                  dplyr::matches('lower|upper')) %>%
    dplyr::rename(
      value = Estimate,
      se = Est.Error
    ) %>%
    dplyr::mutate_if(is.numeric, round, digits = digits)

}
