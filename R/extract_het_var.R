#' Extract heterogeneous variances
#'
#' @description Extract heterogeneous variances for nlme, and potentially other models.
#'
#' @param model An appropriate mixed model
#' @param digits Rounding. Default is 3.
#' @param ... Other arguments appropriate to the method
#'
#' @details For models with heterogeneous variance, i.e. that contain something
#'   like varIdent(form = ~1|Group), nlme returns a result in the summary
#'   regarding the variances that many do not know what to do with, nor likely
#'   would be what they would want to report. I rarely do these models with
#'   nlme, and have only played around with the \code{varIdent} case.
#' @return A vector of the estimates on the variance scale.
#' @importFrom stats coef
#'
#' @examples
#' library(nlme)
#' library(mixedup)
#'
#' model <- lme(distance ~ age + Sex, data = Orthodont, random = ~ 1|Subject,
#' weights = varIdent(form = ~ 1 | Sex))
#'
#' summary(model)
#'
#' extract_het_var(model)
#'
#' @export
extract_het_var <- function(
  model,
  digits = 3,
  ...
) {
  if (!inherits(model, c('lme')))
    stop('This only works for model objects from nlme at present.')

  UseMethod('extract_het_var')
}

#' @export
extract_het_var.lme <- function(
  model,
  digits = 3,
  ...
) {

  init = coef(model$modelStruct$varStruct, unconstrained = FALSE)

  out = (c(1.0, init) * model$sigma) ^ 2

  reflev = attributes(model$modelStruct$varStruct)$groupNames[1]

  names(out)[1] = reflev

  data.frame(as.list(out)) %>%
    dplyr::mutate_if(is.numeric, round, digits = digits)
}


