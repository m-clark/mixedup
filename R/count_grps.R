#' Count groups
#'
#' @description A helper function to count groups to add to random effect results.
#'
#' @param model A fitted model e.g. from `lme4`.
#' @param grp_vars A character vector fo the grouping/cluster variables used for random effects
#'
#' @details For each grouping variable for which random effects are estimated, count the respective group sizes.  This is not meant to be used directly.
#'
#' @return A tibble of the results.
#' @examples
#'
#' library(lme4)
#' library(mixedup)
#'
#' mod = lmer(Reaction ~ Days + (1 + Days | Subject), sleepstudy)
#'
#' count_grps(mod, 'Subject')
#'
count_grps <- function(model, grp_vars) {

  gv = purrr::map(grp_vars, rlang::sym)

  purrr::map2_df(
    gv,
    grp_vars,
    function(gr, nm)
      extract_model_data(model) %>%
      dplyr::count(!!gr) %>%
      dplyr::mutate(group_var = nm) %>%
      dplyr::rename(group = !!gr) %>%
      dplyr::mutate_if(is.factor, as.character)
  )
}
