#' Find typical groups
#'
#' @description Given random effect values find 'typical' instances.
#'
#' @param model A supported model, e.g. lme4, glmmTMB, brms, or lme
#' @param re The name of the grouping variable for the random effects. Default
#'   is \code{NULL} to return all.
#' @param probs Quantile probabilities
#' @param ... Other arguments to pass to \link{extract_random_effects}
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
#' @inheritParams extract_random_effects
#'
#' @note Not surprisingly, different estimation procedures may produce different
#'   random effect estimates. As such, you may get a different result from say,
#'   \code{lme4} vs. \code{glmmTMB}.  In addition, because \code{nlme} doesn't
#'   automatically handle crossed random effects, you will potentially get a
#'   label that is not actually seen in the data, but reflects the nesting of
#'   multiple grouping factors.  I don't care enough about \code{nlme} usage to
#'   do anything about this at present.
#'
#' @return A data frame of group label(s) and their random effect values.
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
#' @export
find_typical <- function(
  model,
  re = NULL,
  probs = NULL,
  ...
) {

  if (!is.null(probs) && is.numeric(probs)) {
    if (range(probs)[1] < 0 | range(probs)[2] > 1)
      stop('probs must be between 0 and 1.')
  }

  re <- extract_random_effects(model, re = re, ...)

  if (is.null(probs)) {
    re <- re %>%
      dplyr::group_by(group_var, effect) %>%
      dplyr::slice(which.min(abs(value))) %>%
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
