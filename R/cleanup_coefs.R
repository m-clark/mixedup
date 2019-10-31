#' Clean up names
#'
#' Only used internally to clean up names.
#'
#' @param re_names Names of the randome effects
#' @param ran_coefs The coefficients
#' @param se The standard errors of the coefficients
#'
#' @return A data frame with clean names
#'
cleanup_coefs <- function(re_names, ran_coefs, se) {

  colnames(ran_coefs) = gsub(
    colnames(ran_coefs),
    pattern = '[\\(, \\)]',
    replacement = ''
  )

  if (!is.null(se)) {
    colnames(se) = paste0('se_', re_names)
    colnames(se) = gsub(
      colnames(se),
      pattern = '[\\(, \\)]',
      replacement = ''
    )
    out = data.frame(
      group = rownames(ran_coefs),
      ran_coefs,
      se
    )
  }
  else {
    out = data.frame(
      group = rownames(ran_coefs),
      ran_coefs
    )
  }


  rownames(out) = NULL # remove

  out
}
