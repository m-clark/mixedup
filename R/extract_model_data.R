#' Extract model data
#'
#' @description This is mostly just a wrapper for \code{model.frame}.
#'
#' @param model The mixed model
#'
#' @return a tibble/dataframe
#'
#' @seealso \link{model.frame}
#'
#' @examples
#' library(lme4)
#' library(mixedup)
#'
#' lmer_1 <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
#' extract_model_data(lmer_1)
#'
#' @export
extract_model_data <- function(model) {
  if (!inherits(model,
                c('merMod', 'glmmTMB', 'lme', 'gam', 'stanreg', 'brmsfit')))
    stop(paste0(
      'This is for model objects from ',
      'lme4, glmmTMB,rstanarm, brms, mgcv, and nlme.'
    ))

  if (inherits(model, 'lme')) {
    data <- dplyr::as_tibble(model$data)
  }
  else {
    data <- dplyr::as_tibble(stats::model.frame(model))
  }

  data
}