#' Extract model data
#'
#' @description This is mostly just a wrapper for \code{model.frame}.
#'
#' @param model The mixed model
#'
#' @return A tibble/dataframe of the data used in the model.
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
    stop('This is not a supported model class.')

  if (inherits(model, 'lme')) {
    data <- dplyr::as_tibble(model$data)
  }
  else {
    data <- dplyr::as_tibble(stats::model.frame(model))
  }

  data
}
