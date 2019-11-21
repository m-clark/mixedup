#' Extract model data
#'
#' @description This is just a wrapper for \code{model.frame}.
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
  dplyr::as_tibble(stats::model.frame(model))
}
