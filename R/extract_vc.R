#' Extract variance components
#'
#' @description This has functionality for simpler models from \code{lme4}, \code{glmmTMB}, \code{nlme}, and \code{brms}.
#'
#' @param model an lme4, glmmTMB, nlme, mgcv, or brms model
#' @param ci_level confidence level < 1, typically above 0.90. A value of 0 will
#'   not report it (except for gam objects, which will revert to .95 due to
#'   \code{gam.vcomp}). Default is .95.
#' @param ci_args Additional arguments to the corresponding confint method.
#' @param ci_scale A character string of 'sd' or 'var' to note the scale of the
#'   interval estimate.  Default is 'sd'.
#'   at present.
#' @param component For glmmTMB objects, which of the three components 'cond',
#'   'zi' or 'other' to select. Default is cond. Minimal testing on other
#'   options.
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
#' @return A data frame with output for variance components or list that also
#' contains the correlations of the random effects.
#'
#' @seealso
#'   \code{\link[lme4]{confint.merMod}},
#'   \code{\link[lme4:VarCorr]{VarCorr.merMod}},
#'   \code{\link[glmmTMB]{confint.glmmTMB}},
#'   \code{\link[glmmTMB:VarCorr]{VarCorr.glmmTMB}},
#'   \code{\link[nlme:intervals]{intervals}},
#'   \code{\link[nlme:VarCorr]{VarCorr.lme}},
#'   \code{\link[brms:VarCorr]{VarCorr.brmsfit}}
#'   \code{\link[mgcv:gam.vcomp]{gam.vcomp}}
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
#' @importFrom  stats confint
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
  if (!inherits(model, c('merMod', 'glmmTMB', 'lme', 'gam', 'brmsfit')))
    stop('This only works for model objects from lme4, glmmTMB, brms,
         mgcv, and nlme.')

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

  if (ci_level > 0) {
    ci <- do.call(
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
    )

    if (ci_scale == 'var') {
      ci <- ci^2
      colnames(ci) <- paste0('var_', colnames(ci))
    }
    else {
      colnames(ci) <- paste0('sd_', colnames(ci))
    }

    colnames(ci) <- gsub(colnames(ci), pattern = ' %', replacement = '')

    ci <- data.frame(ci)

    # to deal with zi/other component issues noted above, paste component to search
    pat <- paste0(component, '.Std.Dev', '|sigma')

    ci <- dplyr::filter(ci, grepl(rownames(ci), pattern = pat))

    vc <- cbind(vc, ci)
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
  # component = 'cond',
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
    cormats <- purrr::map(cormats, function(x) apply(x, 2:3, mean))

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


