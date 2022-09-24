#' Find typical groups
#'
#' @description Given random effect values, find 'typical' instances.
#'
#' @param model A supported model, see \link{extract_random_effects}.
#' @param re The name of the grouping variable for the random effects. Default
#'   is \code{NULL} to return all.
#' @param probs Quantile probabilities.
#' @param ... Other arguments to pass to \link{extract_random_effects}.
#'
#' @details In many cases, predictions are made by holding the random effects to
#'   zero, which can be seen as the typical case. When the clusters carry
#'   substantive meaning, for example, school, country, or hospital, it might be
#'   of interest to note which are most 'typical', or at any other value
#'   relative to their peers.
#'
#'   By default, this finds the labels associated with random effects that are
#'   closest to zero.  Alternatively, one can specify quantile probabilities
#'   rather than the 'typical' value.
#'
#'   One should see the results as examples, not definitive.  Depending on the
#'   results of the model, even differences from the lowest valued estimated
#'   random effect and the highest may not be statistically notable.
#'
#'
#' @note Not surprisingly, different estimation procedures may produce different
#'   random effect estimates. As such, you may get a different result from say,
#'   the lme4 vs. glmmTMB package.  In addition, because nlme doesn't
#'   automatically handle crossed random effects, you will potentially get a
#'   label that is not actually seen in the data, but reflects the nesting of
#'   multiple grouping factors.  I don't care enough about nlme usage to
#'   do anything about this at present.
#'
#' @return A data frame that is a subset of [extract_random_effects()].
#'
#' @examples
#' library(mixedup)
#' library(lme4)
#'
#' lmer_1 <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
#'
#' find_typical(lmer_1, re = 'Subject')
#'
#' @importFrom stats quantile
#'
#' @export
find_typical <- function(
  model,
  re    = NULL,
  probs = NULL,
  ...
) {

  if (!is.null(probs) && is.numeric(probs)) {
    if (min(probs) < 0 | max(probs) > 1)
      stop('probs must be between 0 and 1.')
  }

  re <- extract_random_effects(model, re = re, ...)

  if (is.null(probs)) {
    re <- re %>%
      dplyr::group_by(group_var, effect) %>%
      dplyr::slice(which.min(abs(value))) %>% # get closest to zero
      dplyr::ungroup()
  }
  else {
    re <- re %>%
      dplyr::group_by(group_var, effect) %>%
      dplyr::slice(find_quants(value, probs)) %>%
      dplyr::mutate(probs = paste0(round(probs*100, 1), '%')) %>%
      dplyr::ungroup()
  }

  re
}
