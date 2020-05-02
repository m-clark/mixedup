#' Count groups
#'
#' @description A helper function to count groups to add to random effect
#'   results.
#'
#' @param model A fitted model e.g. from `lme4`.
#' @param grp_vars A character vector fo the grouping/cluster variables used for
#'   random effects
#'
#' @details For each grouping variable for which random effects are estimated,
#'   count the respective group sizes.  This is not meant to be used directly.
#'
#' @return A tibble of the results.
#'
#' @examples
#'
#' library(lme4)
#' library(mixedup)
#'
#' mod = lmer(Reaction ~ Days + (1 + Days | Subject), sleepstudy)
#'
#' count_grps(mod, 'Subject')
#'
#' @keywords internal
#' @export
count_grps <- function(model, grp_vars) {

  if (!inherits(model, c('merMod', 'glmmTMB', 'lme', 'brmsfit',
                         'gam', 'stanreg')))
    stop('This only works for model objects from lme4, glmmTMB, brms, rstanarm,
    mgcv, and nlme.') #

  UseMethod('count_grps')

}

#' @rdname count_grps
#' @export
count_grps.default <- function(model, grp_vars) {

  gv <- purrr::map(grp_vars, dplyr::sym)

  purrr::map2_df(
    gv,
    grp_vars,
    function(grp, name)
      extract_model_data(model) %>%
      dplyr::count(!!grp) %>%
      dplyr::mutate(group_var = name) %>%
      dplyr::rename(group = !!grp) %>%
      dplyr::mutate_if(is.factor, as.character) %>%
      dplyr::mutate(group = as.character(group)) %>%
      dplyr::select(group_var, group, n)
  )
}

#' @rdname count_grps
#' @export
count_grps.merMod <- function(model, grp_vars) {

  count_grps.default(model, grp_vars)

}


#' @rdname count_grps
#' @export
count_grps.glmmTMB <- function(model, grp_vars) {

  count_grps.default(model, grp_vars)

}


#' @rdname count_grps
#' @export
count_grps.lme <- function(model, grp_vars) {

  gv <- purrr::map(grp_vars, dplyr::sym)

  # for nlme objects (lme is fine), can't use extract model data as data isn't
  # saved, but it actually does save the groups as a data frame (for both
  # classes) in the `groups` element
  purrr::map2_df(
    gv,
    grp_vars,
    function(grp, name)
      model$groups %>%
      dplyr::count(!!grp) %>%
      dplyr::mutate(group_var = name) %>%
      dplyr::rename(group = !!grp) %>%
      dplyr::mutate_if(is.factor, as.character) %>%
      dplyr::select(group_var, group, n)
  )
}

#' @rdname count_grps
#' @export
count_grps.brmsfit <- function(model, grp_vars) {

  count_grps.default(model, grp_vars)

}


#  standard stan_glmer objects are structured the same as merMod objects
#' @rdname count_grps
#' @export
count_grps.stanreg <- count_grps.merMod

#' @rdname count_grps
#' @export
count_grps.stanmvreg <-  function(model, grp_vars) {
  # the model data is actually a list of separate dataframes, one for each
  # target variable
  gv <- purrr::map(grp_vars, dplyr::sym)

  # if (inherits(model, 'stanjm'))
  #   model_data <- model$dataLong

  purrr::pmap_df(
    list(
    extract_model_data(model),
    gv,
    grp_vars
    ),
    function(data, grp, name)
      data %>%
      dplyr::count(!!grp) %>%
      dplyr::mutate(group_var = name) %>%
      dplyr::rename(group = !!grp) %>%
      dplyr::mutate_if(is.factor, as.character) %>%
      dplyr::select(group_var, group, n),
    .id = 'component'
  )

}

#' @rdname count_grps
#' @export
count_grps.gam <- function(model, grp_vars) {

  count_grps.default(model, grp_vars)

}
