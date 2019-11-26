# Mostly string functioning so as not to rely on stringr/stringi

totitle <- function(string) {
  gsub(string,
       pattern = '^([[:lower:]])',
       replacement = '\\U\\1',
       perl = T)
}

remove_parens <- function(x) {
  colnames(x) <- gsub(colnames(x), pattern = '[\\(,\\)]', replacement = '')
  rownames(x) <- colnames(x)
  x
}


split_group_effect <- function(x, which = 1) {
  init = strsplit(x, split = ",")
  purrr::map_chr(init, function(x) x[min(which, length(x))])
}
