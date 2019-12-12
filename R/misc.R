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


# to work around glmmTMB:::summary.glmmTMB, which if not used will fail to
# provide fixed effects; plucked straight from the package with no alteration
# summary_tmb <- function (object, ...)
# {
#   if (length(list(...)) > 0) {
#     warning("additional arguments ignored")
#   }
#   sig <- sigma(object)
#   famL <- family(object)
#   mkCoeftab <- function(coefs, vcov) {
#     p <- length(coefs)
#     coefs <-
#       cbind(Estimate = coefs, `Std. Error` = sqrt(diag(vcov)))
#     if (p > 0) {
#       coefs <- cbind(coefs, (cf3 <- coefs[, 1] / coefs[,
#                                                        2]), deparse.level = 0)
#       statType <- "z"
#       coefs <- cbind(coefs, 2 * pnorm(abs(cf3), lower.tail = FALSE))
#       colnames(coefs)[3:4] <- c(paste(statType, "value"),
#                                 paste0("Pr(>|", statType, "|)"))
#     }
#     coefs
#   }
#   ff <- fixef(object)
#   vv <- vcov(object)
#   coefs <-
#     setNames(lapply(names(ff), function(nm)
#       if (trivialFixef(names(ff[[nm]]),
#                        nm))
#         NULL
#       else
#         mkCoeftab(ff[[nm]], vv[[nm]])), names(ff))
#   llAIC <- llikAIC(object)
#   varcor <- VarCorr(object)
#   structure(
#     list(
#       logLik = llAIC[["logLik"]],
#       family = famL$family,
#       link = famL$link,
#       ngrps = ngrps(object),
#       nobs = nobs(object),
#       coefficients = coefs,
#       sigma = sig,
#       vcov = vcov(object),
#       varcor = varcor,
#       AICtab = llAIC[["AICtab"]],
#       call = object$call
#     ),
#     class = "summary.glmmTMB"
#   )
# }
