#' Extract variance components
#'
#' @description This has functionality for simpler models from \code{lme4}, \code{glmmTMB}, \code{nlme}, and \code{brms}.
#'
#' @param model an lme4, glmmTMB, nlme, or brms model
#' @param ci_level confidence level < 1, typically above 0.90. A value of 0 will
#'   not report it. Default is .95.
#' @param ci_args Additional arguments to the corresponding confint method.
#' @param ci_scale A character string of 'sd' or 'var' to note the scale of the
#'   interval estimate.  Default is 'sd'.
#'   at present.
#' @param component For glmmTMB objects, which of the three components 'cond',
#'   'zi' or 'other' to select. Default is cond. Minimal testing on other
#'   options.
#' @param show_cor Return the intercept/slope correlations. Default is
#'   \code{FALSE}.
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
#' @seealso \code{\link{confint.merMod}}, \code{\link{VarCorr.merMod}},
#'   \code{\link{confint.glmmTMB}}, \code{\link{VarCorr.glmmTMB}},
#'   \code{\link{intervals}}, \code{\link{VarCorr.lme}}, \code{\link{VarCorr.brmsfit}}
#'
#' @examples
#' library(lme4)
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
  component = 'cond',
  show_cor = FALSE,
  digits = 3,
  ...
) {
  if (!inherits(model, c('merMod', 'glmmTMB', 'lme', 'brmsfit')))
    stop('This only works for model objects from lme4, glmmTMB, brms, and nlme.')

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
  component,
  show_cor = FALSE,
  digits = 3,
  ...
) {
  vc_mat = lme4::VarCorr(model)

  # make dataframe and add names
  vc = data.frame(vc_mat)
  colnames(vc) = c('group', 'coefficient', 'coefficient_2', 'variance', 'sd')

  # as part of package, create better named and dataframe for ci output to add to results?
  if (ci_level > 0) {
    ci = do.call(
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
      )

    if (ci_scale == 'var') {
      ci = ci^2
      colnames(ci) = paste0('var_', colnames(ci))
    }
    else {
      colnames(ci) = paste0('sd_', colnames(ci))
    }

    colnames(ci) = gsub(colnames(ci), pattern = ' %', replacement = '')

    vc = cbind(vc, ci)
  }

  # cleanup/add to results
  vc = dplyr::filter(vc, is.na(coefficient) | is.na(coefficient_2))

  vc = dplyr::mutate(
    vc,
    var_prop    = variance / sum(variance),
    coefficient = gsub(coefficient, pattern = '[\\(,\\)]', replacement = ''),
    coefficient = ifelse(is.na(coefficient), '', coefficient)
  )

  vc = dplyr::select(vc, -coefficient_2)

  vc = dplyr::mutate_if(vc, is.numeric, round, digits = digits)

  # deal with correlations
  if (show_cor) {

    cormats = lapply(vc_mat, attr, 'correlation')

    remove_parens = function(x) {
      colnames(x) = gsub(colnames(x), pattern = '[\\(,\\)]', replacement = '')
      rownames(x) = colnames(x)
      x
    }

    cormats <- lapply(cormats, remove_parens)
    cormats <- lapply(cormats, round, digits = digits)

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
  component = 'cond',
  show_cor = FALSE,
  digits = 3,
  ...
) {
  vc_mat = glmmTMB::VarCorr(model)[[component]]

  # make dataframe and add names
  variance = lapply(vc_mat, diag)

  variance = data.frame(
    group = rep(names(variance), sapply(variance, length)),
    coefficient = unlist(lapply(variance, names)),
    variance = unlist(variance)
  )

  if (model$modelInfo$family$family == 'gaussian') {
    resvar = data.frame(
      group = 'Residual',
      coefficient = '',
      variance = attr(vc_mat, 'sc')
    )

    variance = rbind(variance, resvar)
  }

  # sds = unlist(sapply(vc_mat, attr, 'stddev'))  # not really necessary

  vc = data.frame(variance, sd = sqrt(variance$variance))

  # as part of package, create better named and dataframe for ci output to add to results?
  if (ci_level > 0) {
    ci = do.call(
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
      ci = ci^2
      colnames(ci) = paste0('var_', colnames(ci))
    }
    else {
      colnames(ci) = paste0('sd_', colnames(ci))
    }

    colnames(ci) = gsub(colnames(ci), pattern = ' %', replacement = '')

    ci = data.frame(ci)
    # to deal with zi/other component issues noted above, paste component to search
    pat = paste0(component, '.Std.Dev', '|sigma')
    ci = dplyr::filter(ci, grepl(rownames(ci), pattern = pat))

    vc = cbind(vc, ci)
  }

  # cleanup/add to results

  vc = dplyr::mutate(
    vc,
    var_prop    = variance / sum(variance),
    coefficient = gsub(coefficient, pattern = '[\\(,\\)]', replacement = ''),
    coefficient = ifelse(is.na(coefficient), '', coefficient)
  )

  vc = dplyr::mutate_if(vc, is.numeric, round, digits = digits)

  # deal with correlations
  if (show_cor) {

    cormats = lapply(vc_mat, attr, 'correlation')

    remove_parens = function(x) {
      colnames(x) = gsub(colnames(x), pattern = '[\\(,\\)]', replacement = '')
      rownames(x) = colnames(x)
      x
    }

    cormats <- lapply(cormats, remove_parens)
    cormats <- lapply(cormats, round, digits = digits)

    return(list(`Variance Components` = vc, Cor = cormats))
  }

  vc
}


#' @importFrom tidyr fill
#' @importFrom nlme ranef intervals
#' @rdname extract_vc
#' @export
extract_vc.lme <- function(
  model,
  ci_level = .95,
  ci_args = NULL,
  ci_scale = 'sd',
  component,
  show_cor = FALSE,
  digits = 3,
  ...
) {
  re_struct <- model$modelStruct$reStruct
  re_names <- names(re_struct)

  vc_mat0 <- nlme::VarCorr(model)
  vc_mat <- suppressWarnings(apply(vc_mat0, 2, as.numeric))

  # make dataframe and add names
  vc <- data.frame(vc_mat)

  if (length(re_names) == 1) {
    vc <- dplyr::mutate(
      vc,
      coefficient = rownames(vc_mat0),
      group = re_names,
      group = ifelse(coefficient == 'Residual', 'Residual', group),
      # group = sapply(strsplit(group, ' '), function(x) x[1]),
      coefficient = ifelse(coefficient == 'Residual', '', coefficient)
    )
  }
  else {
    vc <- dplyr::mutate(
      vc,
      coefficient = rownames(vc_mat0),
      group = ifelse(is.na(Variance), coefficient, NA),
      group = ifelse(coefficient == 'Residual', 'Residual', group),
      group = sapply(strsplit(group, ' '), function(x) x[1]),
      coefficient = ifelse(coefficient == 'Residual', '', coefficient)
    )
  }

  vc  <- tidyr::fill(vc, group)
  vc  <- dplyr::filter(vc, !is.na(Variance))

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
      ci_re <- mapply(function(x, y) {x$group = y; x},
                     ci$reStruct,
                     names(ci$reStruct),
                     SIMPLIFY = FALSE)
      ci_re <- do.call(rbind, ci_re)
      ci_residual <- as.list(ci$sigma)
      ci_residual$group <- 'Residual'
      ci <- rbind(ci_re, data.frame(ci_residual))

      if (ci_scale == 'var') {
        ci <- ci[,1:3]^2
        ci <- dplyr::rename(ci, var_lower = lower, var_upper = upper)
        ci <- dplyr::rename(ci, var = est.)
      }
      else {
        ci <- dplyr::rename(ci, sd_lower = lower, sd_upper = upper)
        ci <- dplyr::rename(ci, sd = est.)
      }

      ci <- dplyr::filter(ci, !grepl(rownames(ci), pattern = '.cor\\('))
    }
  }

  if (!exists('ci')) ci = NULL

  # cleanup
  vc <- dplyr::mutate(
    vc,
    var_prop = Variance / sum(Variance),
    coefficient = gsub(coefficient, pattern = '[\\(,\\)]', replacement = '')
  )

  vc <- dplyr::rename(vc, sd = StdDev)
  vc <- dplyr::rename_all(vc, tolower)

  # reorder columns
  if (is.null(ci)) {
    vc <- dplyr::select(vc, group, coefficient, dplyr::everything())
  }
  else {
    vc <- cbind(vc, dplyr::select(ci, dplyr::matches('upper|lower')))
    vc <- dplyr::select(vc,
                        group, coefficient, dplyr::matches('sd$|variance'),
                        dplyr::matches('upper|lower'), var_prop)
  }

  vc <- dplyr::mutate_if(vc, is.numeric, round, digits = digits)

  if (show_cor) {

    cormats <- lapply(re_struct, function(x) stats::cov2cor(as.matrix(x)))

    remove_parens <- function(x) {
      colnames(x) = gsub(colnames(x), pattern = '[\\(,\\)]', replacement = '')
      rownames(x) = colnames(x)
      x
    }

    cormats <- lapply(cormats, remove_parens)
    cormats <- lapply(cormats, round, digits = digits)

    return(list(`Variance Components` = vc, Cor = cormats))
  }

  vc
}



#' @importFrom dplyr %>%
#' @rdname extract_vc
#' @export
extract_vc.brmsfit <- function(
  model,
  ci_level = .95,
  ci_args = NULL,
  ci_scale = 'sd',
  component = 'cond',
  show_cor = FALSE,
  digits = 3,
  ...
) {

  if (ci_level > 0) {
    # convert ci level
    lower = (1-ci_level)/2
    vc_0 <- brms::VarCorr(model, probs = c(lower, lower + ci_level))
  }

  vc_mat  <- lapply(vc_0, function(x) data.frame(x$sd))

  # make prettier names
  effect_names <- unlist(lapply(vc_mat, rownames))
  effect_names <- ifelse(effect_names == '1', '', effect_names)

  group_names <- rep(names(vc_mat), sapply(vc_mat, nrow))
  group_names <- gsub(group_names, pattern = '\\_\\_', replacement = '')
  group_names <- ifelse(group_names == 'residual', 'Residual', group_names)

  vc = do.call(rbind, vc_mat)

  # create basic output with correct names
  vc = vc %>%
    dplyr::mutate(
      coefficient = effect_names,
      group = group_names,
      variance = Estimate^2
    ) %>%
    dplyr::rename(
      sd = Estimate
    )

  if (ci_level > 0) {
    vc <- dplyr::rename_at(dplyr::vars(dplyr::starts_with('Q')), function(x)
      gsub(x, pattern = 'Q', replacement = paste0(ci_scale, '_')))
    if (ci_scale == 'var') {
      vc <- vc %>%
        dplyr::mutate_at(dplyr::vars(dplyr::starts_with('var_')), `^`, 2)
    }
  }

  vc <- vc %>%
    dplyr::select(group, coefficient, variance, sd, dplyr::matches('\\_')) %>%
    dplyr::mutate(var_prop = variance / sum(variance)) %>%
    dplyr::mutate_if(is.numeric, round, digits = digits)

  if (show_cor) {
    # rerun with VarCorr with summary FALSE and extract array of cor matrices
    cormats <-
      lapply(brms::VarCorr(
        model,
        probs = c(lower, lower + ci_level),
        summary = FALSE
      ), `[[`, 'cor')

    # remove NULL cor
    cormats = cormats[!sapply(cormats, is.null)]

    # get mean matrix
    cormats = lapply(cormats, function(x) apply(x, 2:3, mean))

    return(list(`Variance Components` = vc, Cor = cormats))
  }

  vc
}
