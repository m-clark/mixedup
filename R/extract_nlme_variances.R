#' extract_nlme_variances
#'
#' @description Because nlme won't do it for you.
#' @param model The nlme model object
#' @details For models with heterogeneous variance, i.e. that contain something
#'   like varIdent(form = ~1|Group), nlme returns a result in the summary
#'   regarding the variances that many do not know what to do with, nor likely
#'   would be what they would want to report. I rarely do these models with
#'   nlme, and have only played around with the varIdent case.   This function '
#'   will save me a little trouble in demos I do, but I have no plans to do any
#'   more with it.
#' @return A vector of the estimates on the variance scale.
#' @importFrom stats coef
#' @export
#'
#' @examples
#' library(nlme)
#' fm2 <- lme(distance ~ age + Sex, data = Orthodont, random = ~ 1|Subject,
#' weights=varIdent(form = ~1|Sex))
#' summary(fm2)
#' extract_nlme_variances(fm2)

extract_nlme_variances <- function(model) {
  init = coef(model$modelStruct$varStruct, unconstrained=F)
  out = (c(1.0, init)*model$sigma)^2
  reflev = attributes(model$modelStruct$varStruct)$groupNames[1]
  names(out)[1] = reflev
  out
}
