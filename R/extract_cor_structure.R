#' Extract correlation structure
#'
#' @description Extract residual correlation structure for nlme, brms, and
#' potentially other models.
#'
#' @inheritParams extract_het_var
#'
#' @param ci_level confidence level < 1, typically above 0.90. A value of 0 will
#'   not report it. Default is .95.
#' @param which_cor Required for glmmTMB.  Which correlation parameter to
#'   extract. Must be one of 'ar1', 'ou', 'cs', 'toep', or 'us'.
#' @param full_matrix For glmmTMB correlation, return the full residual
#'   covariance/correlation matrix (`TRUE`), or simplified output where possible
#'   (`FALSE`). Default is `FALSE`. See details.
#' @param component For glmmTMB objects, which of the three components 'cond',
#'   'zi' or 'disp' to select. Default is 'cond'.
#'
#' @details This function applies to models with residual correlation, i.e. that
#'   contain something like `corAR1(form = ~time)` for nlme, or brms
#'   models with an `autocor` argument.  This functions extracts the
#'   associated parameters (e.g. `Phi` in nlme, `ar[1]` in brms, etc.)
#'
#'   For glmmTMB objects, rather than the full matrix, simplified output is
#'   provided by default. For `ar1`, `ou`, `cs`, a single value; for `toep`
#'   (toeplitz) a single row/column; for `diag` structures just the diagonal.
#'   In addition, for `diag` the residual variance is added to the estimates.
#'
#'   For more detail, see this \href{https://bbolker.github.io/mixedmodels-misc/notes/corr_braindump.html}{'braindump'
#' from Ben Bolker}, and the
#' \href{https://cran.r-project.org/web/packages/glmmTMB/vignettes/covstruct.html}{glmmTMB
#' vignette}.
#'
#'   Most types of spatial models should work as well.
#'
#'
#' @return For nlme models, a data frame of the estimates. For brms, the
#'   parameters and related uncertainty, similar to
#'   \link{extract_fixed_effects}.
#'
#' @examples
#' \dontrun{
#' library(brms)
#' library(mixedup)
#'
#' brm_corAR <- brm(
#'   Reaction ~ Days + (Days | Subject),
#'   autocor = cor_ar( ~ Days | Subject),
#'   save_ranef = FALSE,
#'   cores = 4,
#'   thin = 40
#' )
#'
#' extract_cor_structure(brm_corAR)
#' }
#'
#' @family extract
#'
#' @export
extract_cor_structure <- function(
  model,
  digits = 3,
  ...
) {
  if (!inherits(model, c('lme', 'brmsfit', 'glmmTMB')))
    stop('This only works for model objects from nlme, brms, and glmmTMB.')

  UseMethod('extract_cor_structure')
}

#' @rdname extract_cor_structure
#' @export
extract_cor_structure.lme <- function(
  model,
  digits = 3,
  ...
) {

  cs <- model$modelStruct$corStruct

  if (inherits(cs,
               c(
                 'corAR1',
                 'corARMA',
                 'corCAR',
                 'corCompSymm',
                 'corSpher',
                 'corLin',
                 'corExp',
                 'corRatio',
                 'corGaus'
               ))) {

    cs <- t(round(coef(cs, unconstrained = FALSE), digits = digits))
    dplyr::as_tibble(cs)

  } else if (inherits(cs, c('corSymm'))) {

    # get the largest matrix; first if balanced
    cor_matrices <- as.matrix(cs)

    res <- as.data.frame(cor_matrices[[which.max(attr(cs, 'Dim')$len)[1]]])

    rownames(res) <- colnames(res)

    round(res, digits = digits)

  } else {
    message('This correlation structure may not be supported')
    cs <- t(round(coef(cs, unconstrained = FALSE), digits = digits))

    dplyr::as_tibble(cs)
  }

}


#' @rdname extract_cor_structure
#' @export
extract_cor_structure.brmsfit <- function(
  model,
  digits = 3,
  ...,
  ci_level = .95
) {

  cor_par <- summary(model, prob = ci_level)$cor_par

  lower <- (1 - ci_level)/2
  upper <- 1 - lower

  # rename intervals
  cor_par <- dplyr::as_tibble(cor_par, rownames = 'parameter') %>%
    dplyr::rename_at(dplyr::vars(dplyr::matches('l-')), function(x)
      paste0('lower_', lower*100)) %>%
    dplyr::rename_at(dplyr::vars(dplyr::matches('u-')), function(x)
      paste0('upper_', upper*100))

  # more cleanup/return
  cor_par %>%
    dplyr::select(parameter, Estimate, Est.Error,
                  dplyr::matches('lower|upper')) %>%
    dplyr::rename(
      value = Estimate,
      se = Est.Error
    ) %>%
    dplyr::mutate_if(is.numeric, round, digits = digits)

}


#' @importFrom purrr is_empty
#' @export
#' @rdname extract_cor_structure
extract_cor_structure.glmmTMB <- function(
  model,
  digits = 3,
  ...,
  component = 'cond',
  which_cor,
  full_matrix = FALSE
) {

  if (
    purrr::is_empty(which_cor) |
    !which_cor %in% c('ar1', 'ou', 'cs', 'toep', 'us', 'diag', 'mat', 'exp', 'gau')
  )
    stop('which_cor must be one of ar1, ou, cs, toep, us, or diag.')

  cor_init  <- glmmTMB::VarCorr(model)[[component]]

  if (which_cor == 'diag')
    cor_mats <-
    purrr::map(cor_init, function(x) attr(x, 'stddev') + glmmTMB::sigma(model) ^ 2)
  else
    cor_mats <- purrr::map(cor_init, function(x) attr(x, 'correlation'))

  cor_types <- purrr::map(cor_init, function(x) names(attr(x, 'blockCode')))

  extract <- which(unlist(cor_types) == which_cor)

  # in case an incorrect which_cor is supplied
  if(purrr::is_empty(extract)) {
    stop('No match between which_cor and model output.')
  } else {
    cor_par <- cor_mats[extract]
  }

  # extact first value when rest are determined
  if (!full_matrix) {
    if (which_cor %in% c('ar1', 'ou', 'cs')) {
      cor_par <- purrr::map_df(cor_par, function(x)
        round(x[1, 2], digits = digits)) %>%
        dplyr::mutate(parameter = which_cor) %>%
        dplyr::select(parameter, dplyr::everything())
    }
    # similar for diagonal
    else if (which_cor ==  'diag') {
      cor_par <- purrr::map_df(cor_par, function(x)
        data.frame(t(round(x, digits = digits))), .id = 'group')
    }
    # and toeplitz/spatial
    else if (which_cor %in%  c('toep', 'exp', 'mat', 'gau')) {
      cor_par <- purrr::map_df(cor_par, function(x)
        round(data.frame(x[1, , drop = FALSE]), digits = digits), .id = 'group')
    }
  }

  cor_par

}
