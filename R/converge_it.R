#' Converge an lme4 model
#'
#' @description Convergence is a common problem with mixed models of enough
#'   complexity, especially GLMM, and especially those fit by the `lme4` package.
#'   Assuming it's not a data or specification problem causing the issue, this
#'   function will run the model successively until convergence.
#'
#' @param model An `lme4` model.
#'
#' @details This simple function currently just works for `lme4` objects.  Its
#'   main purpose is to just save the trouble of guessing how long you might
#'   need to run something to get to the default convergence. It
#'   just continues running `update` with additional iterations until
#'   convergence or an additional stopping point (10 additional runs of
#'   `update`).  At that point you can just feed the updated model and continue
#'   further if desired, try a different optimizer, a different model, etc.
#'   While this function may get you to convergence, you still may have
#'   'singular' or other issues.  In addition, you will still see warnings as it
#'   iterates toward a converged model.
#'
#' @note While it is true that GLMMs generally are hard to fit, most convergence
#'   warnings with `lme4` seem really more about the underlying data, or a
#'   problematic model, rather than an issue with estimation.  Furthermore, it
#'   is often the case that the model with warnings will typically have no
#'   meaningful difference in results with those from a different optimizer, but
#'   this would need to be checked with some thing like `allFit`.  Also, if you
#'   are having a problem with a model fit with `lmer`, i.e. an LMM as opposed
#'   to a GLMM, this is usually a model that is too complex for the underlying
#'   data.
#'
#' @return A hopefully successfully converged `lme4` model, or one closer to
#'   convergence.
#'
#' @seealso \code{\link[lme4:convergence]{convergence}},
#'   \code{\link[lme4:allFit]{allFit}},
#'   \code{\link[lme4:isSingular]{isSingular}}
#'
#' @examples
#' \dontrun{
#' data(Salamanders, package = 'glmmTMB')
#'
#' library(lme4)
#' library(mixedup)
#'
#' # a possibly silly model
#' glmer_mod = glmer(count ~ spp + mined + DOP + Wtemp + DOY +
#'   (1 | sample) +
#'   (1 + DOY + DOP | site),
#'   data = Salamanders,
#'   family = poisson
#' )
#'
#'
#' converge_it(glmer_mod)
#'}
#' @importFrom stats update
#' @export
converge_it <- function(model) {
  if (!inherits(model, c('merMod')))
    stop('This is not a supported model class.')

  UseMethod('converge_it')
}

#' @rdname converge_it
#' @export
converge_it.merMod <- function(model) {

  # if(!"lme4" %in% (.packages())){
  #   message('loading lme4...')
  #   require("lme4")
  # }

  # update will call these under the hood, so this avoids having to actually
  # load the library.  Not sure which is the better approach relative to above.
  lmer  <- lme4::lmer
  glmer <- lme4::glmer

  count <- 0

  while (count < 10 && length(model@optinfo$conv$lme4$code) != 0) {

    params <- lme4::getME(model, c("theta", "fixef"))

    if (inherits(model, 'glmerMod')) {
      model  <- stats::update(
        object  = model,
        start   = params,
        control = lme4::glmerControl(optCtrl = list(maxfun = 2e5))
      )
    }
    else {
      model  <- stats::update(
        object  = model,
        start   = params,
        control = lme4::lmerControl(optCtrl = list(maxfun = 2e5))
      )
    }

    count <- count + 1

    # this can take a while to test
    if (count == 10) message('Stopping after 10 additional runs. Restart to continue if desired...')
  }

  model
}
