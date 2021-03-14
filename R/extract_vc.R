#' Extract variance components
#'
#' @description This has functionality for simpler models from \code{lme4},
#'   \code{glmmTMB}, \code{nlme}, and \code{brms}.
#'
#' @param model An lme4, glmmTMB, nlme, mgcv, or brms model.
#' @param ci_level Confidence level < 1, typically above 0.90. A value of 0 will
#'   not report it (except for gam objects, which will revert to .95 due to
#'   \code{gam.vcomp}). Default is .95.
#' @param ci_args Additional arguments to the corresponding confint method.
#' @param ci_scale A character string of 'sd' or 'var' to note the scale of the
#'   interval estimate.  Default is 'sd'. at present.
#' @param component For glmmTMB objects, which of the three components 'cond' or
#'   'zi' to select. Default is 'cond'.  For brmsfit objects, this can filter
#'   results to a certain part of the output, e.g. 'sigma' or 'zi' of
#'   distributional models, or a specific outcome of a multivariate model.  In
#'   this case \code{component} is a regular expression that begins parameters
#'   of the output.
#' @param show_cor Return the intercept/slope correlations as a separate list
#'   element. Default is \code{FALSE}.
#' @param digits Rounding. Default is 3.
#' @param ... Other stuff to pass to the corresponding function. Unused/tested
#'
#' @details Returns a more usable (my opinion) version of variance components
#'   estimates including variance, standard deviation, the confidence interval
#'   for either, the relative proportion of variance, and all in a data frame
#'   with names that are clean and easy to use.
#'
#'   `extract_variance_components` and `extract_VarCorr` are aliases.
#'
#'
#' @note Right now, there are several issues with getting confidence intervals
#'   for `glmmTMB` objects
#'   (\href{https://github.com/glmmTMB/glmmTMB/issues/571}{for example}).  If
#'   you get an error, you should check by running `confint(my_tmb_model)` before
#'   posting an issue.  While I've attempted some minor hacks to deal with some
#'   of them, if the `glmmTMB` function doesn't work, this function won't
#'   either.
#'
#'
#' @return A data frame with output for variance components, or list that also
#' contains the correlations of the random effects.
#'
#' @seealso
#'   [lme4::confint.merMod()],
#'   [lme4::VarCorr()],
#'   [glmmTMB::VarCorr()],
#'   [nlme::intervals()],
#'   [nlme::VarCorr()],
#'   [brms::VarCorr()],
#'   [rstanarm::VarCorr()],
#'   [mgcv::gam.vcomp()]
#'
#' @examples
#' library(lme4)
#' library(mixedup)
#'
#' lmer_mod <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
#'
#' extract_vc(lmer_mod)
#' extract_vc(lmer_mod, ci_scale = 'var')
#'
#' @family extract
#'
#' @importFrom  stats confint
#'
#' @export
extract_vc <- function(
  model,
  ci_level = .95,
  ci_args = NULL,
  ci_scale = 'sd',
  show_cor = FALSE,
  digits = 3,
  component = 'cond',
  ...
) {
  if (!inherits(model, c('merMod', 'glmmTMB', 'lme', 'gam', 'brmsfit', 'stanreg')))
    stop('This is not a supported model class.')

  if (ci_level < 0 | ci_level >= 1)
    stop('Nonsensical confidence level for ci_level.  Must be between 0 and 1.')

  if (!ci_scale %in% c('var', 'sd'))
    stop("ci_scale must be 'var' or 'sd'")

  UseMethod('extract_vc')
}

#' @export
#' @rdname extract_vc
extract_vc.merMod <- function(
  model,
  ci_level = .95,
  ci_args = NULL,
  ci_scale = 'sd',
  show_cor = FALSE,
  digits = 3,
  # component,
  ...
) {

  vc_mat <- lme4::VarCorr(model)

  # make dataframe and add names
  vc <- data.frame(vc_mat)

  colnames(vc) <- c('group', 'effect', 'effect_2', 'variance', 'sd')

  vc <- vc %>%
    data.frame() %>%
    dplyr::filter(is.na(effect) | is.na(effect_2))

  if (ci_level > 0) {
    ci <- tryCatch(do.call(
      confint,
      c(
        list(
          object = model,
          parm = 'theta_',
          level = ci_level,
          oldNames = FALSE
          ),
          ci_args
        )
      ),
      error = function(c) {
        msg <- conditionMessage(c)
        invisible(structure(msg, class = "try-error"))
      })

    if (inherits(ci, 'try-error')) {
      warning('Intervals could not be computed')
    }
    else {
      if (ci_scale == 'var') {
        ci <- ci^2
        colnames(ci) <- paste0('var_', colnames(ci))
      }
      else {
        colnames(ci) <- paste0('sd_', colnames(ci))
      }

      colnames(ci) <- gsub(colnames(ci), pattern = ' %', replacement = '')

      # drop = F in case no residual, otherwise will lose structure for single
      # random effect
      ci <- ci[!grepl(rownames(ci), pattern = 'cor'), , drop = FALSE]

      vc <- cbind(vc, ci)
    }

  }

  # cleanup/add to results
  vc <- vc %>%
    # dplyr::filter(is.na(effect) | is.na(effect_2)) %>%
    dplyr::mutate(
      var_prop = variance / sum(variance),
      effect   = gsub(effect, pattern = '[\\(,\\)]', replacement = ''),
      effect   = ifelse(is.na(effect), '', effect)
    )

  vc <- dplyr::select(vc, -effect_2)

  vc <- dplyr::mutate_if(vc, is.numeric, round, digits = digits)

  # deal with correlations
  if (show_cor) {

    cormats <- vc_mat %>%
      purrr::map(attr, 'correlation') %>%
      purrr::map(remove_parens) %>%
      purrr::map(round, digits = digits)

    return(list(`Variance Components` = vc, Cor = cormats))
  }

  vc
}

#' @export
#' @rdname extract_vc
extract_vc.glmmTMB <- function(
  model,
  ci_level = .95,
  ci_args = NULL,
  ci_scale = 'sd',
  show_cor = FALSE,
  digits = 3,
  component = 'cond',
  ...
) {
  vc_mat <- glmmTMB::VarCorr(model)[[component]]

  # no re allowed for dispersion formula
  if (!component %in% c('cond', 'zi')) {
    stop('component must be one of "cond" or "zi".')
  }

  # make dataframe and add names
  variance <- purrr::map(vc_mat, diag)

  variance <- data.frame(
    group    = rep(names(variance), purrr::map_int(variance, length)),
    effect   = unlist(purrr::map(variance, names)),
    variance = unlist(variance)
  )

  if (model$modelInfo$family$family == 'gaussian') {
    resvar = data.frame(
      group    = 'Residual',
      effect   = '',
      variance = attr(vc_mat, 'sc')^2
    )

    variance <- rbind(variance, resvar)
  }

  vc <- data.frame(variance, sd = sqrt(variance$variance))

  # TODO: confint will not work for some tmb objects, e.g. with ar, so probably
  # need a trycatch;
  # see glmmTMB:::getCorSD (constructed within formatVC) for how to extract ar sd; basically just grabs the first
  # value of diag for sd and second value for cor

  if (ci_level > 0) {
    ci <-
      tryCatch(
        do.call(
          confint,
          c(
            list(
              object = model,
              # parm = 'theta_',  # this has been problematic in the past and doesn't work properly now https://github.com/bbolker/broom.mixed/issues/31
              level = ci_level,
              # component = component,    # will also throw an error
              estimate = FALSE
            ),
            ci_args
          )
        ),
        error = function(c) {
          msg <- conditionMessage(c)
          invisible(structure(msg, class = "try-error"))
        }
      )
    if (inherits(ci, 'try-error')) {
      warning('Intervals could not be computed. Returning basic result.')

      ci <- data.frame(
        lower = NA,
        upper = NA
      )
    }

    if (ci_scale == 'var') {
      ci <- ci^2
      colnames(ci) <- paste0('var_', colnames(ci))
    }
    else {
      colnames(ci) <- paste0('sd_', colnames(ci))
    }

    colnames(ci) <- gsub(colnames(ci), pattern = ' %', replacement = '')

    ci <- data.frame(ci)

    # to deal with zi/other component issues noted above, paste component to
    # search; however, in some cases, the component name is not part of the
    # confint output
    pat <- paste0(component, '.Std.Dev', '|^Std.Dev', '|sigma')

    ci <- dplyr::filter(ci, grepl(rownames(ci), pattern = pat))

    # the ci doesn't return anything for residual var, if it exists, so this is
    # an attempt to deal with it
    if (nrow(ci) < nrow(vc)) {
      # deal with try-error result
      if (nrow(ci) == 0) {
        vc <- vc
      } else{
        vc_ci = vc %>%
          dplyr::slice(-nrow(vc)) %>%    # remove residual
          dplyr::bind_cols(ci)

        vc <- dplyr::bind_rows(vc_ci, vc %>% dplyr::filter(group == 'Residual'))
      }
    } else {
      vc <- cbind(vc, ci)
    }
  }

  # cleanup/add to results

  vc <- dplyr::mutate(
    vc,
    var_prop = variance / sum(variance),
    effect   = gsub(effect, pattern = '[\\(,\\)]', replacement = ''),
    effect   = ifelse(is.na(effect), '', effect)
  )

  vc <- vc %>%
    dplyr::mutate_if(is.numeric, round, digits = digits) %>%
    dplyr::mutate_if(is.factor, as.character)


  # deal with correlations
  if (show_cor) {

    cormats <- vc_mat %>%
      purrr::map(attr, 'correlation') %>% purrr::map(remove_parens) %>%
      purrr::map(round, digits = digits)

    return(list(`Variance Components` = vc, Cor = cormats))
  }

  vc
}


#' @rdname extract_vc
#' @export
extract_vc.lme <- function(
  model,
  ci_level = .95,
  ci_args = NULL,
  ci_scale = 'sd',
  show_cor = FALSE,
  digits = 3,
  # component,
  ...
) {
  re_struct <- model$modelStruct$reStruct
  re_names  <- names(re_struct)

  vc_mat0 <- nlme::VarCorr(model)
  vc_mat  <- suppressWarnings(apply(vc_mat0, 2, as.numeric))

  # make dataframe and add names
  vc <- data.frame(vc_mat)

  if (length(re_names) == 1) {
    vc <- dplyr::mutate(
      vc,
      effect = rownames(vc_mat0),
      group  = re_names,
      group  = ifelse(effect == 'Residual', 'Residual', group),
      effect = ifelse(effect == 'Residual', '', effect)
    )
  }
  else {
    vc <- dplyr::mutate(
      vc,
      effect = rownames(vc_mat0),
      group  = ifelse(is.na(Variance), effect, NA),
      group  = ifelse(effect == 'Residual', 'Residual', group),
      group  = sapply(strsplit(group, ' '), function(x) x[1]),
      effect = ifelse(effect == 'Residual', '', effect)
    )
  }

  vc <- tidyr::fill(vc, group)
  vc <- dplyr::filter(vc, !is.na(Variance))

  # add trycatch for interval fail

  if (ci_level > 0) {

    ci <- tryCatch(
      nlme::intervals(model, level = ci_level),
      error = function(c) {
        msg <- conditionMessage(c)
        invisible(structure(msg, class = "try-error"))
      })

    if (inherits(ci, 'try-error')) {
      warning('Intervals could not be computed')
      ci <- NULL
    }
    else {
      ci_re <- purrr::map2(
        ci$reStruct,
        names(ci$reStruct),
        function(x, y) {
          x$group = y
          x
      })

      ci_re <- do.call(rbind, ci_re)
      ci_residual <- as.list(ci$sigma)
      ci_residual$group <- 'Residual'
      ci <- rbind(ci_re, data.frame(ci_residual))

      lower <- (1 - ci_level) / 2
      upper <- 1 - lower

      if (ci_scale == 'var') {
        ci <- ci[,1:3]^2

        colnames(ci)[colnames(ci) %in% c('lower', 'upper')] <-
          c(paste0('var_', 100 * lower), paste0('var_', 100 * upper))

        ci <- dplyr::rename(ci, var = est.)
      }
      else {
        colnames(ci)[colnames(ci) %in% c('lower', 'upper')] <-
          c(paste0('sd_', 100 * lower), paste0('sd_', 100 * upper))

        ci <- dplyr::rename(ci, sd = est.)
      }
      ci <- dplyr::filter(ci, !grepl(rownames(ci), pattern = '.cor\\('))
    }
  }

  if (!exists('ci')) ci = NULL

  # cleanup
  vc <- vc %>%
    dplyr::mutate(
      var_prop = Variance / sum(Variance),
      effect   = gsub(effect, pattern = '[\\(,\\)]', replacement = '')
    ) %>%
    dplyr::rename(sd = StdDev) %>%
    dplyr::rename_all(tolower)

  # reorder columns, with var_prop at the end
  if (is.null(ci)) {
    vc <- vc %>%
      dplyr::select(group, effect, dplyr::everything(), -var_prop, var_prop)
  }
  else {
    vc <- cbind(vc, dplyr::select(ci, dplyr::matches('var\\_|sd\\_'))) %>%
      dplyr::select(
        group,
        effect,
        dplyr::everything(),
        -var_prop,
        var_prop,
      )
  }

  vc <- dplyr::mutate_if(vc, is.numeric, round, digits = digits)
  vc$corr = NULL # leave to show_cor if it exists

  if (show_cor) {
    cormats <- re_struct %>%
      purrr::map(function(x) stats::cov2cor(as.matrix(x))) %>%
      purrr::map(remove_parens) %>%
      purrr::map(round, digits = digits)

    return(list(`Variance Components` = vc, Cor = cormats))
  }

  vc
}



#' @rdname extract_vc
#' @export
extract_vc.brmsfit <- function(
  model,
  ci_level = .95,
  ci_args = NULL,
  ci_scale = 'sd',
  show_cor = FALSE,
  digits = 3,
  component = NULL,
  ...
) {

  lower <-  (1 - ci_level)/2
  vc_0  <- brms::VarCorr(model, probs = c(lower, 1 - lower))

  vc_mat  <- purrr::map(vc_0, function(x) data.frame(x$sd))

  # make prettier names
  effect_names <- unlist(purrr::map(vc_mat, rownames))
  effect_names <- ifelse(effect_names == '1', '', effect_names)

  group_names <- rep(names(vc_mat), sapply(vc_mat, nrow))
  group_names <- gsub(group_names, pattern = '\\_\\_', replacement = '')
  group_names <- ifelse(group_names == 'residual', 'Residual', group_names)

  vc <- do.call(rbind, vc_mat)

  # create basic output with correct names
  vc <- vc %>%
    dplyr::mutate(
      effect   = effect_names,
      group    = group_names,
      variance = Estimate^2
    ) %>%
    dplyr::rename(
      sd = Estimate
    )

  if (ci_level > 0) {
    vc <-
      vc %>%
      dplyr::rename_at(dplyr::vars(dplyr::starts_with('Q')), function(x)
        gsub(x, pattern = 'Q', replacement = paste0(ci_scale, '_')))
    if (ci_scale == 'var') {
      vc <-
        vc %>%
        dplyr::mutate_at(dplyr::vars(dplyr::starts_with('var_')), `^`, 2)
    }
  }

  vc <- vc %>%
    dplyr::select(group, effect, variance, sd, dplyr::matches('\\_')) %>%
    dplyr::mutate(var_prop = variance / sum(variance)) %>%
    dplyr::mutate_if(is.numeric, round, digits = digits)

  if (!is.null(component)) {
    vc <- vc %>%
      dplyr::filter(grepl(effect, pattern = paste0('^', component)))
  }

  if (show_cor) {
    # rerun with VarCorr with summary FALSE and extract array of cor matrices
    cormats <-
      purrr::map(
        brms::VarCorr(
          model,
          probs = c(lower, lower + ci_level),
          summary = FALSE
        ),
        `[[`,
        'cor'
      )

    # remove NULL cor
    cormats <- cormats[!purrr::map_lgl(cormats, is.null)]

    # get mean matrix
    cormats <-
      purrr::map(cormats, function(x)
        round(apply(x, 2:3, mean), digits = digits))


    return(list(`Variance Components` = vc, Cor = cormats))
  }

  vc
}


#' @export
#' @rdname extract_vc
extract_vc.stanreg <- function(
  model,
  ci_level = .95,
  ci_args = NULL,
  ci_scale = 'sd',
  show_cor = FALSE,
  digits = 3,
  component = NULL,
  ...
) {

  vc_mat <- rstanarm::VarCorr(model)

  if (inherits(model, 'stanmvreg')) {
    message("Multivariate models not yet supported. VarCorr object returned.")
    return(vc_mat)
  }

  # make dataframe and add names
  vc <- data.frame(vc_mat)

  colnames(vc) <- c('group', 'effect', 'effect_2', 'variance', 'sd')

  vc <- vc %>%
    dplyr::filter(is.na(effect) | is.na(effect_2))

  # calculate CI; problem is that the names for objects containing the variance
  # component intervals do not in any way match the names from the VarCorr
  # object, and VarCorr doesn't provide interval estimates.

  if (ci_level == 0) {
    message('CI automatically provided for stanreg objects.
    ci_level set to .9, feel free to change to another value.')
    ci_level = .90
  }

  vc_ci <- rstanarm::posterior_interval(
    model,
    regex_pars = '^sigma$|^Sigma\\[',
    prob = ci_level
  )

  # the horror of name cleaning/matching
  vc_ci_clean = clean_rstanarm_vc(vc_ci, ci_level, ci_scale) %>%
    dplyr::as_tibble()

  if (ci_scale != 'var') {
    vc_ci_clean <- vc_ci_clean %>%
      dplyr::mutate_if(is.numeric, `^`, .5)
  }

  # cleanup/add to results
  vc <- vc %>%
    dplyr::mutate(
      var_prop = variance / sum(variance),
      effect   = gsub(effect, pattern = '[\\(,\\)]', replacement = ''),
    )

  vc <- vc %>%
    dplyr::select(-effect_2) %>%
    dplyr::mutate_if(is.character, function(x) ifelse(is.na(x), '', x)) %>%
    dplyr::left_join(vc_ci_clean, by = c('group', 'effect'))

  vc <- dplyr::mutate_if(vc, is.numeric, round, digits = digits) %>%
    dplyr::select(-var_prop, dplyr::everything())

  # deal with correlations
  if (show_cor) {

    cormats <- vc_mat %>%
      purrr::map(attr, 'correlation') %>%
      purrr::map(remove_parens) %>%
      purrr::map(round, digits = digits)

    return(list(`Variance Components` = vc, Cor = cormats))
  }

  vc

}



#' @rdname extract_vc
#' @export
extract_vc.gam <- function(
  model,
  ci_level = .95,
  ci_args = NULL,
  ci_scale = 'sd',
  show_cor = FALSE,
  digits = 3,
  # component = 'cond',
  ...
  ) {

  if (!grepl(model$method, pattern = "REML")) {
    stop("REML required. Rerun model with method = 'REML' for appropriate results.")
  }

  # keep from printing result
  invisible(
    utils::capture.output(
      vc <- mgcv::gam.vcomp(model, conf.lev = ci_level)
    )
  )

  vc <- data.frame(vc)

  # clean up names
  vc <- vc %>%
    dplyr::mutate(
      effect  = rownames(vc),
      effect  = gsub(effect, pattern = "s\\(|ti\\(|te\\(|\\)", replacement = '')
    )

  # if more two after split, suggests random slope
  vc <- vc %>%
    dplyr::mutate(
      group = split_group_effect(effect, which = 2),
      group = ifelse(group == 'scale', 'Residual', group),
      effect = split_group_effect(effect, which = 1),
      effect = ifelse(effect == 'scale', '', effect),
      effect = ifelse(effect ==  group, 'Intercept', effect)
      )

  # calc variance and scale
  vc <- vc %>%
    dplyr::mutate(
      variance = std.dev^2,
      var_prop = variance / sum(variance)
    ) %>%
    dplyr::rename(sd = std.dev)

  lower <- (1 - ci_level) / 2
  upper <- 1 - lower

  if (ci_scale == 'var') {
    vc <- vc %>%
      dplyr::mutate(lower = lower^2, upper = upper^2)

    colnames(vc)[colnames(vc) %in% c('lower', 'upper')] <-
      c(paste0('var_', 100 * lower), paste0('var_', 100 * upper))
  }
  else {
    colnames(vc)[colnames(vc) %in% c('lower', 'upper')] <-
      c(paste0('sd_', 100 * lower), paste0('sd_', 100 * upper))
  }

  vc %>%
    dplyr::select(group, effect, variance, dplyr::everything())  %>%
    dplyr::mutate_if(is.numeric, round, digits = digits)
}


#' @rdname extract_vc
#' @export
extract_variance_components <- extract_vc

#' @rdname extract_vc
#' @export
extract_VarCorr <- extract_vc
