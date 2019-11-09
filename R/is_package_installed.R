#' Test package installation
#'
#' @param package Character string of the package name to test
#'
is_package_installed <- function(package){
  requireNamespace(package, quietly = TRUE)
}
