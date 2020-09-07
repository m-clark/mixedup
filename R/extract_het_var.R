#' Extract heterogeneous variances
#'
#' @description Extract heterogeneous variances for nlme, glmmTMB, and brms
#'   models.
#'
#' @param model An appropriate mixed model.
#' @param digits Rounding. Default is 3.
#' @param ci_level  For brms objects, confidence level < 1, typically above
#'   0.90. A value of 0 will not report it. Default is .95.
#' @param return_all For brms class objects, return all fitted values (`TRUE`) or only
#'   the distinct ones. Default is `FALSE`.
#' @param scale Return result on original standard deviation scale ('sd') or as
#'   variance ('var'), the default.
#' @param ... Other arguments specific to the method. Unused at present.
#'
#' @details For nlme models with heterogeneous variance, i.e. that contain
#' something like `varIdent(form = ~1|Group)`, this returns a more presentable
#' version the estimates. Only tested with the `varIdent` case.
#'
#' For glmmTMB, this serves as a wrapper for \link{extract_cor_structure} for
#' models with for the `diag` function as part of the formula.  See that
#' function for details. For distributional models where the dispersion is
#' explicitly modeled separately via `disp = ~ `, use the `component` argument
#' of the other functions in this package.
#'
#' For brms distributional models with a `sigma ~ . formula`, this produces the
#' (unique) fitted values for the dispersion part of the model.  As this is
#' often just a single grouping variable to allow variance to vary over the
#' group levels, only the distinct fitted values, which would be one value per
#' group, are returned. If all fitted values are desired, set `return_all` to
#' `TRUE`.
#'
#' This function has not been tested except in the more simple model settings.
#' It's unclear how well it will work with other model complications added.
#'
#' @return A vector of the estimates on the variance scale
#'
#' @family extract
#'
#' @importFrom stats coef fitted
#'
#' @examples
#' library(nlme)
#' library(mixedup)
#'
#' model <- lme(
#'   distance ~ age + Sex,
#'   data = Orthodont,
#'   random = ~ 1|Subject,
#'   weights = varIdent(form = ~ 1 | Sex)
#' )
#'
#' summary(model)
#'
#' extract_het_var(model)
#'
#' library(glmmTMB)
#'
#' # does not get the same estimates as nlme, but would get similar if modeled
#' # using dispersion approach.
#' model <-
#'   glmmTMB(distance ~ age + Sex + (1 | Subject) + diag(Sex + 0 | Subject),
#'           data = Orthodont)
#'
#' extract_het_var(model)
#'
#' # compare with
#' model <-
#'   glmmTMB(distance ~ age + Sex + (1 | Subject), dispformula = ~ Sex,
#'           data = Orthodont)
#'
#' extract_fixed_effects(model, component = 'disp', exponentiate = TRUE)
#'
#' \dontrun{
#' library(brms)
#'
#' model <-
#'   brm(bf(distance ~ age + Sex + (1 | Subject), sigma ~ Sex),
#'       data = Orthodont)
#'
#' extract_het_var(model)
#' }
#'
#' @export
extract_het_var <- function(
  model,
  digits = 3,
  scale = 'var',
  ...
) {
  if (!inherits(model, c('lme', 'glmmTMB', 'brmsfit')))
    stop('This only works for model objects from nlme, glmmTMB, and brms.')

  UseMethod('extract_het_var')
}

#' @method extract_het_var lme
#' @export
#' @rdname extract_het_var
extract_het_var.lme <- function(
  model,
  digits = 3,
  scale = 'var',
  ...
) {

  init = coef(model$modelStruct$varStruct, unconstrained = FALSE)

  sigmas = (c(1.0, init) * model$sigma)

  if (scale == 'var') sigmas <- sigmas^2

  reflev = attributes(model$modelStruct$varStruct)$groupNames[1]

  names(sigmas)[1] = reflev

  data.frame(as.list(sigmas)) %>%
    dplyr::mutate_if(is.numeric, round, digits = digits)
}


#' @method extract_het_var glmmTMB
#' @export
#' @rdname extract_het_var
extract_het_var.glmmTMB <- function(
  model,
  digits = 3,
  scale = 'var',
  ...
) {
  sigmas <- extract_cor_structure(model, digits = digits, which_cor = 'diag', ...)

  # by default, the result is var
  if (scale == 'sd')
    sigmas <- sigmas %>% dplyr::mutate_if(is.numeric, sqrt)

  sigmas
}


#' @method extract_het_var brmsfit
#' @export
#' @rdname extract_het_var
extract_het_var.brmsfit <- function(
  model,
  digits = 3,
  scale = 'var',
  ...,
  ci_level = .95,
  return_all = FALSE
) {

  if (ci_level < 0 | ci_level >= 1)
    stop('Nonsensical confidence level for ci_level. Must be between 0 and 1.')

  lower <- (1 - ci_level)/2
  upper <- 1 - lower
  probs <- c(lower, upper)

  # no longer need, but left as a reminder of how to pull target variable
  # response <- attr(terms(as.formula(model$formula)), 'response')

  # get all predictor vars
  covariates <- all.vars(model$formula$pforms$sigma)
  covariates <- covariates[covariates != 'sigma']

  sigma_fits <-
    data.frame(fitted(model, probs = probs, dpar = 'sigma'))

  interval_names <- grepl(colnames(sigma_fits),
                          pattern = paste0('Q', 100*probs, collapse = '|'))

  colnames(sigma_fits)[interval_names] <-
    paste0(c('lower_', 'upper_'), probs * 100)

  if (scale == 'var') {
    sigma_fits <- sigma_fits %>%
      dplyr::mutate(Est.Error = 2*Estimate*Est.Error) %>%
      dplyr::mutate_at(c(1,3,4), `^`, 2)
  }

  sigma_fits <- extract_model_data(model)[, covariates] %>%
    dplyr::bind_cols(sigma_fits) %>%
    dplyr::rename(value = Estimate, se = Est.Error) %>%
    dplyr::mutate_if(is.numeric, round, digits = digits)

  if (!return_all)
    sigma_fits <- dplyr::distinct(sigma_fits)

  sigma_fits
}
