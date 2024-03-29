#' Extract random effects
#'
#' @description Extracts the random effects and their standard errors.
#'
#' @param model An appropriate model. See details.
#' @param re The name of the grouping variable for the random effects. Default
#'   is `NULL` to return all.
#' @param ci_level Where possible, confidence level < 1, typically above 0.90. A
#'   value of 0 will not report it. Default is .95. Not applicable to nlme
#'   objects.
#' @param digits  Rounding. Default is 3.
#' @param add_group_N  Add group sample sizes to output? Default is `FALSE`.
#' @param ... Other arguments specific to the method. Unused at present.
#' @param component For `glmmTMB` objects, which of the two components 'cond' or
#'   'zi' to select. Default is 'cond'. For `brmsfit` objects, this can filter
#'   results to a certain part of the output, e.g. 'sigma' or 'zi' of
#'   distributional models, or a specific outcome name of a multivariate model.
#'   In this case `component` is a regular expression that ends the name of the
#'   parameters of the output (e.g. '__component'). For `stanreg` objects, this
#'   could be the
#' @param condvar Include conditional variance. Used in `lme4` and `glmmTMB` objects.
#' @details Relative to `ranef` for the various packages, this just adds the
#'   standard errors and cluster ids as columns, and uncertainty intervals.
#'
#'   Current models supported:
#'
#' \describe{
#'  \item{merMod}{}
#'  \item{glmmTMB}{}
#'  \item{brms}{}
#'  \item{nlme}{}
#'  \item{brms}{}
#'  \item{rstanarm}{}
#'  \item{mgcv}{}
#' }
#'
#' @note The nlme package only provides the estimated random effect parameters,
#'   not their uncertainty, so it isn't provided.
#'
#' `merMod` and `glmmTMB` objects results are based on the estimated
#' conditional variances, i.e. `condvar = TRUE`.  This is likely an
#' underestimate relative to brms results.
#'
#' For mgcv, the `Vp` (Bayesian) estimated variance covariance matrix is
#' used.
#'
#'@aliases `extract_ranef` is an alias.
#'
#' @return data frame of the random effects
#'
#' @seealso
#' [lme4::ranef()],
#' [glmmTMB::ranef()],
#' [nlme::ranef()],
#' [brms::ranef()],
#' [rstanarm::ranef()],
#' [mgcv::gamObject()],
#' [mgcv::smooth.construct.re.smooth.spec()]
#'
#' @examples
#' library(lme4)
#' library(mixedup)
#'
#' lmer_model <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
#' extract_random_effects(lmer_model)
#'
#' @family extract
#'
#' @export
extract_random_effects <- function(
  model,
  re = NULL,
  ci_level = .95,
  digits   = 3,
  add_group_N = FALSE,
  ...
) {
  assertthat::assert_that(
    inherits(model, c('merMod', 'glmmTMB', 'lme', 'gam', 'stanreg', 'brmsfit')),
    msg = 'This only works for model objects from lme4, glmmTMB, rstanarm, brms,
         mgcv, and nlme.'
  )

  assertthat::assert_that(
    ci_level >= 0 & ci_level < 1,
    msg = 'Nonsensical confidence level for ci_level.  Must be between 0 and 1.'
  )

  UseMethod('extract_random_effects')
}


#' @rdname extract_random_effects
#' @export
extract_random_effects.merMod <- function(
  model,
  re = NULL,
  ci_level = .95,
  digits = 3,
  add_group_N = FALSE,
  condvar = TRUE,
  ...
  # component = 'cond',
) {

  assertthat::assert_that(
    rlang::is_installed('lme4'),
    msg = 'lme4 package required'
  )

  lmer_re <- lme4::ranef(model, condVar = condvar)

  # add check on re name

  all_re_names <- names(lmer_re)

  if (!is.null(re) && !re %in% all_re_names)
    stop(
      paste0('re is not among the names of the random effects: ',
             paste0(all_re_names, collapse = ' ')
      )
    )

  random_effects <- as.data.frame(lmer_re)

  if (!condvar) {
    ci_level <- 0
    colnames(random_effects) <- c('group_var', 'effect', 'group', 'value')
  }
  else {
    colnames(random_effects) <- c('group_var', 'effect', 'group', 'value', 'se')
  }

  if (add_group_N) {
    grp_vars <- unique(random_effects$group_var)

    ns <- count_grps(model, grp_vars)

    # suppress warning of char vs. factor
    random_effects <- suppressWarnings({
      dplyr::left_join(random_effects, ns, by = c("group_var", "group"))
    })
  }

  if (ci_level > 0) {

    lower <- (1 - ci_level)/2
    upper <- 1 - lower
    mult  <- stats::qnorm(upper)

    random_effects <- random_effects %>%
      dplyr::mutate(
        lower = value - mult * se,
        upper = value + mult * se
      )

    colnames(random_effects)[colnames(random_effects) %in% c('lower', 'upper')] <-
      paste0(c('lower_', 'upper_'), c(lower, upper) * 100)
  }

  if (!is.null(re)) {
    random_effects <- random_effects %>%
      dplyr::filter(group_var == re)
  }

  random_effects <- random_effects %>%
    dplyr::mutate(
      effect = gsub(effect,
                    pattern = '[\\(, \\)]',
                    replacement = '')
    ) %>%
    dplyr::mutate(dplyr::across(\(x) is.numeric(x), round, digits = digits)) %>%
    dplyr::as_tibble()

  if (add_group_N) {
    random_effects <- random_effects %>% dplyr::select(-n, dplyr::everything())
  }

  random_effects
}

#' @rdname extract_random_effects
#' @export
extract_random_effects.glmmTMB <- function(
  model,
  re = NULL,
  ci_level = .95,
  digits = 3,
  add_group_N = FALSE,
  component = 'cond',
  condvar = TRUE,
  ...
) {

  assertthat::assert_that(
    rlang::is_installed('glmmTMB'),
    msg = 'glmmTMB package required'
  )

  assertthat::assert_that(
    component %in% c('cond', 'zi'),
    msg = 'component must be one of "cond" or "zi".'
  )

  # add check on re name
  all_re_names <- names(glmmTMB::ranef(model)[[component]])

  if (!is.null(re) && !re %in% all_re_names)
    stop(
      paste0('re is not among the names of the random effects: ',
             paste0(all_re_names, collapse = ' ')
      )
    )

  random_effects <- as.data.frame(glmmTMB::ranef(model, condVar = condvar)) %>%
    dplyr::filter(component == component) %>%
    dplyr::select(-component)

  if (!condvar) {
    ci_level <- 0
    colnames(random_effects) <- c('group_var', 'effect', 'group', 'value')
  }
  else {
    colnames(random_effects) <- c('group_var', 'effect', 'group', 'value', 'se')
  }

  if (add_group_N) {
    grp_vars <- unique(random_effects$group_var)

    ns <- count_grps(model, grp_vars)

    # suppress warning of char vs. factor
    random_effects <- suppressWarnings({
      dplyr::left_join(random_effects, ns, by = c("group_var", "group"))
    })
  }

  if (ci_level > 0) {

    lower <- (1 - ci_level)/2
    upper <- 1 - lower
    mult  <- stats::qnorm(upper)

    random_effects <- random_effects %>%
      dplyr::mutate(
        lower = value - mult * se,
        upper = value + mult * se
      )

    colnames(random_effects)[colnames(random_effects) %in% c('lower', 'upper')] <-
      paste0(c('lower_', 'upper_'), c(lower, upper) * 100)
  }

  if (!is.null(re)) {
    random_effects <- random_effects %>%
      dplyr::filter(group_var == re)
  }

  random_effects <- random_effects %>%
    dplyr::mutate(
      effect = gsub(effect,
                    pattern = '[\\(, \\)]',
                    replacement = '')
    ) %>%
    dplyr::mutate(dplyr::across(\(x) is.numeric(x), round, digits = digits)) %>%
    dplyr::as_tibble()

  if (add_group_N) {
    random_effects <- random_effects %>% dplyr::select(-n, dplyr::everything())
  }

  random_effects
}


#' @rdname extract_random_effects
#' @export
extract_random_effects.lme <- function(
  model,
  re = NULL,
  ci_level = NULL,
  digits = 3,
  add_group_N = FALSE,
  # component,
  ...
) {

  re0 <- nlme::ranef(model)

  # add check on re name
  if (inherits(re0, 'data.frame')) {
    all_re_names <- attr(re0, 'grpNames')
  }
  else {
    all_re_names <- names(re0)
  }

  if (!is.null(re) && !re %in% all_re_names)
    stop(
      paste0('re is not among the names of the random effects: ',
             paste0(all_re_names, collapse = ' ')
      )
    )

  if (inherits(re0, 'data.frame')) {
    random_effects <- re0 %>%
      as.data.frame() %>%
      dplyr::mutate(
        group = rownames(.)
      ) %>%
      tidyr::pivot_longer(
        cols = -group,
        names_to = 'effect',
        values_to = 'value'
      ) %>%
      dplyr::mutate(group_var = attr(re0, 'grpNames'))
  }
  else {
    random_effects <- re0 %>%
      purrr::map(\(x)
        dplyr::mutate(
          x,
          group = rownames(x))
      ) %>%
      purrr::map_df(
        tidyr::pivot_longer,
        cols = -group,
        names_to = 'effect',
        values_to = 'value',
        .id = 'group_var'
      )
  }

  if (!is.null(re)) {
    random_effects <- random_effects %>%
      dplyr::filter(group_var == re)
  }

  random_effects <-   random_effects %>%
    dplyr::mutate(dplyr::across(\(x) is.numeric(x), round, digits = digits)) %>%
    dplyr::select(group_var, effect, group, value) %>%
    dplyr::mutate(
      effect = gsub(
        effect,
        pattern = '[\\(, \\)]',
        replacement = ''
      )
    ) %>%
    dplyr::arrange(group_var, effect, group) %>%
    dplyr::as_tibble()

  if (add_group_N) {
    grp_vars <- unique(random_effects$group_var)

    ns <- count_grps(model, grp_vars)

    # suppress warning of char vs. factor
    random_effects <- suppressWarnings({
      dplyr::left_join(random_effects, ns, by = c("group_var", "group"))
    })


    random_effects <- random_effects %>% dplyr::select(-n, dplyr::everything())
  }

  random_effects
}

#' @rdname extract_random_effects
#' @export
extract_random_effects.brmsfit <- function(
  model,
  re = NULL,
  ci_level = .95,
  digits = 3,
  add_group_N = FALSE,
  component = NULL,
  ...
) {

  assertthat::assert_that(
    rlang::is_installed('brms'),
    msg = 'brms package required'
  )

  # add check on re name
  all_re_names <- names(brms::ranef(model))

  if (!is.null(re) && !re %in% all_re_names)
    stop(
      paste0('re is not among the names of the random effects: ',
             paste0(all_re_names, collapse = ' ')
      )
    )

  # more or less following broom, but note that brms offers both pars and groups
  # args that might be better to use, if not less computation
  # suppress warning about metadata
  suppressWarnings({
    re0 <-
      brms::as_draws_df(model, variable = '^r_', regex = TRUE) %>%
      dplyr::select(-(.chain:.draw))
  })
  # re0 <- brms::posterior_samples(model, pars = '^r_')

  # not sure this is necessary, would only happen if something very wrong with
  # model or no random effects.
  if (is.null(re0)) {
    stop("No parameter name matches the specified pattern.", call. = FALSE)
  }

  random_effects <- tibble::tibble(effect = names(re0))

  random_effects <- random_effects %>%
    dplyr::mutate(
      effect    = gsub("^r_", "", effect),
      group_var = gsub("\\[.*", "", effect),
      group     = gsub(".*\\[|,.*", "", effect),
      effect    = gsub(".*,|\\]", "", effect),
      value     = base::colMeans(re0),
      se        = purrr::map_dbl(re0, stats::sd)
    ) %>%
    dplyr::select(group_var, dplyr::everything())

  # deal with multiple components
  re_info <- model$ranef

  if (dplyr::n_distinct(re_info$dpar) > 1 | dplyr::n_distinct(re_info$resp) > 1) {

    random_effects <- random_effects %>%
      tidyr::separate(
        col    = group_var,
        into   = c('group_var', 'component'),
        sep    = '__',
        fill   = 'right',
        remove = FALSE
      ) %>%
      dplyr::mutate(component = dplyr::if_else(is.na(component), 'default', component)) %>%
      dplyr::select(component, group_var, dplyr::everything())
  }

  if (add_group_N) {
    grp_vars <- unique(random_effects$group_var)

    ns <- count_grps(model, grp_vars)

    # suppress warning of char vs. factor
    random_effects <- suppressWarnings({
      dplyr::left_join(random_effects, ns, by = c("group_var", "group"))
    })
  }


  # add_ci may add prob as arg in future
  if (ci_level > 0) {

    lower <- (1 - ci_level)/2
    upper <- 1 - lower

    ci <- t(apply(re0, 2, stats::quantile, probs = c(lower, upper)))

    colnames(ci) <- paste0(c('lower_', 'upper_'), c(lower, upper) * 100)

    random_effects <- dplyr::bind_cols(random_effects, data.frame(ci))

  }

  if (!is.null(re)) {
    random_effects <- random_effects %>%
      dplyr::filter(group_var == re)
  }

  if (!is.null(component)) {
    random_effects <- random_effects %>%
      dplyr::filter(grepl(component, pattern = {{component}}))
  }

  random_effects <- random_effects %>%
    dplyr::mutate(dplyr::across(\(x) is.numeric(x), round, digits = digits)) %>%
    dplyr::as_tibble()

  if (add_group_N) {
    random_effects <- random_effects %>%
      dplyr::select(-n, dplyr::everything())
  }

  random_effects
}

#' @rdname extract_random_effects
#' @export
extract_random_effects.stanreg <- function(
  model,
  re = NULL,
  ci_level = .95,
  digits = 3,
  add_group_N = FALSE,
  component = NULL,
  ...
) {

  assertthat::assert_that(
    rlang::is_installed('rstanarm'),
    msg = 'rstanarm package required'
  )

  # Structure is the same as lme4 if just a standard stanreg object
  if (!inherits(model, c('stanmvreg',' stanjm'))) {
    random_effects <- extract_random_effects.merMod(
      model,
      re = re,
      ci_level = ci_level,
      add_group_N = add_group_N,
      digits = digits,
      ...
    )
  }
  else if (inherits(model, 'stanmvreg')) {
    lower <- (1 - ci_level)/2
    upper <- 1 - lower

    random_effects <- dplyr::as_tibble(
      summary(model, pars = 'varying', probs = c(lower, upper)),
      rownames = 'effect'
    )

    random_effects <- random_effects %>%
      dplyr::mutate(
        effect =  gsub(effect, pattern = "b\\[|\\]", replacement = ''),
        effect = remove_parens(effect)
      ) %>%
      tidyr::separate(effect, into = c('component', 'effect', 'group_var', 'group')) %>%
      dplyr::select(-mcse, -n_eff,  -Rhat) %>%
      dplyr::select(component, group_var, dplyr::everything()) %>%
      dplyr::rename(se = sd)


    colnames(random_effects)[grepl(colnames(random_effects), pattern = '%')] <-
      paste0(c('lower_', 'upper_'), c(lower, upper) * 100)

    if (add_group_N) {
      grp_vars <- unique(random_effects$group_var)

      ns <- count_grps(model, grp_vars)

      ns <- ns %>%
        dplyr::mutate(group = as.character(group))

      # suppress warning of char vs. factor
      random_effects <- suppressWarnings({
        dplyr::left_join(random_effects, ns, by = c('component', "group_var", "group"))
      })
    }

    if (!is.null(re)) {
      random_effects <- random_effects %>%
        dplyr::filter(group_var == re)
    }

    if (!is.null(component)) {
      random_effects <- random_effects %>%
        dplyr::filter(component == {{component}})
    }


  }
  # else {
  #   warning('This model only has minimal support')
  #   as.data.frame(rstanarm::ranef(model))
  # }
  random_effects

}



#' @rdname extract_random_effects
#' @export
extract_random_effects.gam <- function(
  model,
  re = NULL,
  # component,
  ci_level = .95,
  digits = 3,
  add_group_N = FALSE,
  ...
) {
  # get the re variables and their levels
  re_terms <- purrr::map_lgl(model$smooth, \(x) inherits(x, "random.effect"))

  re_smooths <- model$smooth[re_terms]

  re_names <- purrr::map_chr(
    model$smooth[re_terms],
    \(x)
    ifelse(
      length(x$vn) == 1,
      x$vn,
      x$vn[length(x$vn)]
    )
  )

  re_levels <- vector("list", length(re_names))


  # add check on re name/type
  # check that re is factor as re smooth can be applied to continuous
  for (i in seq_along(re_names)) {
    if (!inherits(model$model[, re_names[i]], "factor")) {
      warning(
        paste0(re_names[i], ' is not a factor. No results provided for it.')
      )
      re_levels[[i]] <- NULL
    }
    else {
      re_levels[[i]] <- levels(model$model[, re_names[i]])
    }
  }

  if (purrr::is_empty(re_levels) | all(purrr::map_lgl(re_levels, is.null))) {
    stop('No factor random effects.')
  }

  # can put an re smooth on a continuous covariate for penalization, but don't
  # want that in output
  non_factors <- purrr::map_lgl(re_levels, is.null)
  re_idx  <- which(!non_factors)

  # this test is covered but covr ignores for some reason
  if (any(non_factors)) {
    re_terms[non_factors] <- FALSE
    re_names  <- re_names[re_idx]
  }

  if (!is.null(re) && !re %in% re_names)
    stop(
      paste0('re is not among the names of the random effects: ',
             paste0(re_names, collapse = ' ')
      )
    )

  re_labels <- purrr::map(re_smooths[re_idx], \(x) x$label)

  gam_coef <- stats::coef(model)

  # issue, parenthesis in the names means problematic regex matching, so remove
  # all but key part of pattern
  re_label_base <- gsub(re_labels, pattern = "s\\(", replacement = '') # remove first s
  re_label_base <- gsub(re_label_base, pattern = "\\(|\\)", replacement = '') # remove parenthesis

  re_coef  <- vector('list', length = length(re_idx))
  coef_idx <- vector('list', length = length(re_idx))

  for (i in re_idx) {
    first_para <- re_smooths[[i]]$first.para
    last_para  <- re_smooths[[i]]$last.para

    coef_idx[[i]] <- first_para:last_para
    re_coef[[i]]  <- gam_coef[coef_idx[[i]]]
  }

  # extract coefs and se
  re0 <- unlist(re_coef)
  gam_se <- sqrt(diag(model$Vp))[unlist(coef_idx)]

  # clean up and gather names
  names(re0) <- gsub(names(re0), pattern = "s\\(|\\)", replacement = '')
  names(re0) <- gsub(names(re0), pattern = "\\.[0-9]+", replacement = '')

  re_names   <- names(re0)
  re_effects <- purrr::map_chr(re_smooths, \(x) x$term[1])
  re_effects <- rep(re_effects, times = purrr::map_int(re_coef, length))

  # check to see if factors are the smooth terms (i.e. random cat slope), and
  # repeat levels of grouping variable the number of levels in the factor
  for (i in re_idx) {
    smooth_vars <-  re_smooths[[i]]$term
    smooth_term <- model$model[[smooth_vars[1]]] # it will be the first term, 2nd term is the RE var

    if (length(smooth_vars) > 1 & (is.factor(smooth_term) | is.character(smooth_term))) {
      n_levs <- dplyr::n_distinct(smooth_term)
      re_levels[[i]] <- rep(re_levels[[i]], each = n_levs) # note the each, this is how mgcv orders and confirmed via lme4
    }
  }

  random_effects <- dplyr::tibble(group_var = re_names) %>%
    dplyr::mutate(
      group_var = split_group_effect(group_var, which = 2),
      effect    = re_effects,
      effect    = ifelse(effect ==  group_var, 'Intercept', effect),
      group     = unlist(re_levels),
      value     = re0,
      se        = gam_se
    )

  if (add_group_N) {
    grp_vars <- unique(random_effects$group_var)

    ns <- count_grps(model, grp_vars)

    # suppress warning of char vs. factor
    random_effects <- suppressWarnings({
      dplyr::left_join(random_effects, ns, by = c("group_var", "group"))
    })
  }

  if (ci_level > 0) {

    lower <- (1 - ci_level)/2
    upper <- 1 - lower
    mult  <- stats::qnorm(upper)

    random_effects <- random_effects %>%
      dplyr::mutate(
        lower = value - mult * se,
        upper = value + mult * se
      )

    colnames(random_effects)[colnames(random_effects) %in% c('lower', 'upper')] <-
      paste0(c('lower_', 'upper_'), c(lower, upper) * 100)
  }

  if (!is.null(re)) {
    random_effects <- random_effects %>%
      dplyr::filter(group_var == re)
  }

  random_effects <- random_effects %>%
    dplyr::select(group_var, effect, dplyr::everything()) %>%
    dplyr::mutate(dplyr::across(\(x) is.numeric(x), round, digits = digits))

  if (add_group_N) {
    random_effects <- random_effects %>% dplyr::select(-n, dplyr::everything())
  }

  random_effects
}



#' @rdname extract_random_effects
#' @export
extract_ranef <- extract_random_effects
