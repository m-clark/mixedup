#' Extract model data
#'
#' @description This is mostly just a wrapper for \code{model.frame}.
#'
#' @param model The mixed model.
#'
#' @return A tibble/dataframe of the data used in the model.
#'
#' @note For whatever reason, `nlme` class objects do not save the model data,
#'   so this will throw an error stating as much. `lme` objects do save the data
#'   so are fine.
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
#'
#' @family extract
#'
#' @export
extract_model_data <- function(model) {
  if (!inherits(model,
                c('merMod', 'glmmTMB', 'lme', 'gam', 'stanreg', 'brmsfit')))
    stop('This is not a supported model class.')

  if (inherits(model, 'nlme')) {
    # seriously wtf?
    stop("nlme doesn't save the data for models of class nlme. Sorry.")
  }
  else if (inherits(model, 'lme')) {
    data <- dplyr::as_tibble(model$data)
  }
  else {
    data <- dplyr::as_tibble(stats::model.frame(model))
  }

  data
}
