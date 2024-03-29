#' Find typical values of a random effect
#'

#' @param value The random effect values
#' @param probs Desired probabilities passed to \code{quantile}

#' @details Finds indices of random effect values closest to a specific
#'   quantile. Used internally by `find_typical`.

#' @return A vector of the observed values closest to the desired quantiles.

#' @noRd
find_quants <- function(value, probs) {

  qs <- quantile(value, probs = probs)

  purrr::map_int(qs, function(q)
    which.min(abs(value - q)))
}
