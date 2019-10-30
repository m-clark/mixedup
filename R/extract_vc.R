#' Extract variance components
#'
#' @description Right now this only works for lme4 model objects (i.e.
#'   \code{merMod}).
#'
#' @param model an lme4 or glmmTMB model
#' @param ci_level confidence level < 1, typically above 0.90. A value of 0 will
#'   not report it. Default is .95.
#' @param ci_args Additional arguments to the corresponding confint method.
#' @param ci_scale A character string of 'sd' or 'var' to note the scale of the
#'   interval estimate.  Default is 'sd'.
#' @param show_cor Return the intercept/slope correlations. Default is
#'   \code{FALSE}.
#' @param digits Rounding. Default is 3.
#' @param ... Other stuff to pass to the corresponding function. Unused/tested
#'   at present.
#'
#' @details Returns a more usable (my opinion) version of variance components
#'   estimates including variance, standard deviation, the confidence interval
#'   for either, the relative proportion of variance, and all in a data frame
#'   with names that are clean and easy to use.
#'
#' @return A data frame with output for variance components or list that also
#' contains the correlations of the random effects.
#'
#' @seealso \code{\link{confint.merMod}}, \code{\link{VarCorr.merMod}},
#'   \code{\link{confint.glmmTMB}}, \code{\link{VarCorr.glmmTMB}}
#'
#' @examples
#' library(lme4)
#'
#' lmer_mod <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
#'
#' extract_vc(lmer_mod)
#' extract_vc(lmer_mod, ci_scale = 'var')
#'
#' @importFrom  stats confint
#' @export
extract_vc <- function(
  model,
  ci_level = .95,
  ci_args = NULL,
  ci_scale = 'sd',
  show_cor = FALSE,
  digits = 3,
  ...
) {
  if (!inherits(model, c('merMod', 'glmmTMB')))
    stop('This only works for merMod objects from lme4 or models from glmmTMB.')

  if (ci_level < 0 | ci_level >= 1)
    stop('Nonsensical confidence level for ci_level.  Must be between 0 and 1.')

  if (!ci_scale %in% c('var', 'sd'))
    stop("ci_scale must be 'var' or 'sd'")

  UseMethod('extract_vc')
}

#' @export
extract_vc.merMod <- function(
  model,
  ci_level = .95,
  ci_args = NULL,
  ci_scale = 'sd',
  show_cor = FALSE,
  digits = 3,
  ...
) {
  vc_mat = lme4::VarCorr(model)

  # make dataframe and add names
  vc = data.frame(vc_mat)
  colnames(vc) = c('group', 'coefficient', 'coefficient_2', 'variance', 'sd')

  # as part of package, create better named and dataframe for ci output to add to results?
  if (ci_level > 0) {
    ci = do.call(
      confint,
      c(
        list(
          object = model,
          parm = 'theta_',
          level = ci_level,
          oldNames = FALSE
          ),
          ci_args
        )
      )

    if (ci_scale == 'var') {
      ci = ci^2
      colnames(ci) = paste0('var_', colnames(ci))
    }
    else {
      colnames(ci) = paste0('sd_', colnames(ci))
    }

    colnames(ci) = gsub(colnames(ci), pattern = ' %', replacement = '')

    vc = cbind(vc, ci)
  }

  # cleanup/add to results
  vc = dplyr::filter(vc, is.na(coefficient) | is.na(coefficient_2))

  vc = dplyr::mutate(
    vc,
    var_prop    = variance / sum(variance),
    coefficient = gsub(coefficient, pattern = '[\\(,\\)]', replacement = ''),
    coefficient = ifelse(is.na(coefficient), '', coefficient)
  )

  vc = dplyr::select(vc, -coefficient_2)

  vc = dplyr::mutate_if(vc, is.numeric, round, digits = digits)

  # deal with correlations
  if (show_cor) {

    cormats = lapply(vc_mat, attr, 'correlation')

    remove_parens = function(x) {
      colnames(x) = gsub(colnames(x), pattern = '[\\(,\\)]', replacement = '')
      rownames(x) = colnames(x)
      x
    }

    cormats <- lapply(cormats, remove_parens)
    cormats <- lapply(cormats, round, digits = digits)

    return(list(`Variance Components` = vc, Cor = cormats))
  }

  vc
}

#' @export
extract_vc.glmmTMB <- function(
  model,
  ci_level = .95,
  ci_args = NULL,
  ci_scale = 'sd',
  show_cor = FALSE,
  digits = 3,
  ...
) {
  'Not there yet'
}

