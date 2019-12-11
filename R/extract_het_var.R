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
#'
#'   For glmmTMB, this serves as a wrapper for \link{extract_cor_struct} with for the
#'   diagonal case.  See that function for details.
#'
#' @return A vector of the estimates on the variance scale.
#'
#' @seealso \link{extract_cor_struct}
#'
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
#' library(glmmTMB)
#'
#' # does not get the same estimates as nlme, but would get similar if modeled
#' using dispersion approach.
#' model <-
#'   glmmTMB(distance ~ age + Sex + (1 | Subject) + diag(Sex + 0 | Subject),
#'           data = Orthodont)
#'
#'extract_het_var(model)
#'
#' @export
extract_het_var <- function(
  model,
  digits = 3,
  ...
) {
  if (!inherits(model, c('lme', 'glmmTMB')))
    stop('This only works for model objects from nlme and glmmTMB at present.')

  UseMethod('extract_het_var')
}

#' @export
#' @rdname extract_het_var
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

#' @export
#' @rdname extract_het_var
extract_het_var.glmmTMB <- function(
  model,
  digits = 3,
  ...
) {
  extract_cor_structure(model, digits = digits, which_cor = 'diag', ...)
}
