#' Find typical groups
#'
#' @description Given random effect values find 'typical' instances.
#'
#' @param model A supported model, e.g. lme4, glmmTMB, or lme
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
#' @inheritParams extract_random_coef
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
#' @importFrom purrr map2 map2_df map_int
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom stats quantile
#' @export
find_typical <- function(
  model,
  re = NULL,
  probs = NULL,
  ...
) {

  re <- extract_random_effects(model, re = re, ...)
  re <- dplyr::select(re, -dplyr::starts_with('se_'))
  re_names <- colnames(re)[-1]  # 1 is the 'group' column

  if (is.null(probs)) {
    idx <- sapply(re[, -1, drop = FALSE], function(x) which.min(abs(x)))
    re_typical <- dplyr::slice(re, idx)
  }
  else {
    p_names <- paste0(probs * 100, "%")
    res = lapply(re[, -1, drop = FALSE], quantile, probs = probs, na.rm = TRUE)

    # get indices of values nearest quantiles; looked at ecdf, but wasn't really able to simplify
    indices = purrr::map2(
      re[, -1, drop = FALSE],
      res,
      function(x, y)
        purrr::map_int(y, function(z)
          which.min(abs(x - z)))
    )

    # extract
    re_typical = map2_df(
      indices,
      re_names,
      function(x, y)
        dplyr::select(dplyr::slice(re, x), group, y)
    )

    return(
      re_typical %>%
        tidyr::pivot_longer(cols = re_names, names_to = 'effect') %>%
        tidyr::drop_na() %>%
        dplyr::group_by(effect) %>%
        dplyr::mutate(probs = p_names) %>%
        dplyr::ungroup()
    )
  }

  re <- dplyr::mutate(
    re_typical,
    effect = re_names,
    value  = diag(as.matrix(dplyr::select(re_typical, re_names)))
  )

  dplyr::select(re, group, effect, value)
}
