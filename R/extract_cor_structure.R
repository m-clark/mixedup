#' Extract correlation structure
#' @description Extract residual correlation structure for nlme, and potentially other models.
#' @inheritParams extract_het_var
#'
#' @details For models with correlation, i.e. that contain something like
#'   corAR1(form = ~time).  For more see this
#'   \href{https://bbolker.github.io/mixedmodels-misc/notes/corr_braindump.html}{'braindump'
#'   from Ben Bolker}.
#' @return A vector of the estimates.
#'
#' @export
extract_cor_structure <- function(
  model,
  ...
) {
  if (!inherits(model, c('lme')))
    stop('This only works for model objects from nlme at present.')

  UseMethod('extract_cor_structure')
}

#' @rdname extract_cor_structure
#' @export
extract_cor_structure.lme <- function(
  model,
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

    data.frame(t(coef(cs, unconstrained = FALSE)))

  } else if (inherits(cs, c('corSymm'))) {

    # get the largest matrix; first if balanced
    cor_matrices <- as.matrix(cs)
    res <- as.data.frame(cor_matrices[[which.max(attr(cs, 'Dim')$len)[1]]])
    rownames(res) <- colnames(res)
    res

  } else {
    message('This correlation structure may not be supported')
    data.frame(t(coef(cs, unconstrained = FALSE)))
  }

}
