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
