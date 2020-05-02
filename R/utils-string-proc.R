# Mostly string functioning so as not to rely on stringr/stringi

totitle <- function(string) {
  gsub(string,
       pattern = '^([[:lower:]])',
       replacement = '\\U\\1',
       perl = T)
}

remove_parens <- function(x) {
  if (inherits(x, c('data.frame', 'matrix'))) {
    colnames(x) <- gsub(colnames(x), pattern = '[\\(,\\)]', replacement = '')
    rownames(x) <- colnames(x)
  } else {
    x = gsub(x, pattern = "\\(|\\)", replacement = '')
  }

  x
}

# remove_brackets <- function(x) {
#   if (inherits(x, c('data.frame', 'matrix'))) {
#     colnames(x) <- gsub(colnames(x), pattern = '[\\[,\\]', replacement = '')
#     rownames(x) <- colnames(x)
#   } else {
#     x = gsub(x, pattern = "\\[|\\]", replacement = '')
#   }
#
#   x
# }


split_group_effect <- function(x, which = 1) {
  init = strsplit(x, split = ",")
  purrr::map_chr(init, function(x) x[min(which, length(x))])
}

clean_rstanarm_vc <- function(vc_interval, ci_level, ci_scale){
  # christ, probably no way this holds up for much

  vc0 = vc_interval

  rn = rownames(vc_interval)

  residual = vc0[rn == 'sigma', , drop = FALSE]

  vc_interval = vc0[rn != 'sigma', , drop = FALSE]

  init = gsub(rownames(vc_interval), pattern = '.*\\[|\\].*', replacement = '')

  # get group and var/covar names
  separate_group_from_covvar = strsplit(init, split = ':')

  # extract groups
  groups = sapply(separate_group_from_covvar, `[`, 1)

  # extract covar/var
  covvars = sapply(separate_group_from_covvar, `[`, 2)

  # cleanup
  effects = sapply(covvars, function(x) strsplit(x, split = ',')[[1]][1])

  effects = gsub(effects, pattern = "\\(|\\)", replacement = '')

  # get var indices e.g. "[x,x]" -> one distinct value
  varidx = sapply(strsplit(covvars, split = ','), function(x) dplyr::n_distinct(x)==1)
  varidx = which(varidx)

  # clean interval names
  lower <- (1 - ci_level)/2
  upper <- 1 - lower

  colnames(vc_interval) <- paste0(paste0(ci_scale, '_'), c(lower, upper) * 100)

  vc_interval <- data.frame(
    group = groups[varidx],
    effect = effects[varidx],
    vc_interval[varidx, , drop = FALSE],
    stringsAsFactors = FALSE
  )

  if (nrow(residual) != 0) {

    colnames(residual) <- paste0(paste0(ci_scale, '_'), c(lower, upper) * 100)

    vc_res <- data.frame(
      group = 'Residual',
      effect = '',
      residual,
      stringsAsFactors = FALSE
    )

    vc_interval <- dplyr::bind_rows(vc_interval, vc_res)
  }

  vc_interval

}


