#' @importFrom dplyr %>% across mutate mutate_at select filter rename rename_at
#' rename_all slice group ungroup matches starts_with vars everything
#' @importFrom tibble rownames_to_column as_tibble
#' @importFrom purrr map map_int map2 map2_df
#' @importFrom tidyr pivot_longer drop_na fill
#' @importFrom rlang is_installed

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
