#' Summarize a mixed model
#'
#' @description This function prints fixed effects and variance components for a
#'   mixed model.
#'
#' @param model A supported model.
#' @param ci Whether to include a 95% uncertainty interval for the variance
#'   components. Default is TRUE.
#' @param show_cor_re Whether to include the correlations of the random effects.
#'   Default is FALSE.
#' @param show_cor_fe Whether to include the correlations of the fixed effects.
#'   Default is FALSE.
#' @param exponentiate Exponentiate the fixed-effect coefficient estimates and
#'   confidence intervals (common for logistic regression). If `TRUE`, also
#'   scales the standard errors by the exponentiated coefficient, transforming
#'   them to the new scale.
#' @param digits Digits to display.
#' @param component For glmmTMB objects, which of the three components 'cond',
#'   'zi' or 'other' to select. Default is cond. Minimal testing on other
#'   options.
#' @param ... Not used at present. May allow model-specific functionality.
#'
#' @details This basically does pretty printing of the results of [extract_vc()]
#'   and [extract_fixed_effects()].
#'
#' @note Not tested yet for complicated `stanreg` objects like multivariate or
#'   joint models.
#'
#' @return Prints the variance components, fixed effects, etc. Invisibly, a list
#'   of those.
#'
#' @seealso [extract_vc()], [extract_fixed_effects()]
#'
#' @examples
#' library(lme4)
#' library(mixedup)
#'
#' lmer_mod <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
#' summarize_model(lmer_mod)
#'
#' @importFrom stats vcov
#'
#' @export
summarize_model <- function(
  model,
  ci           = TRUE,
  show_cor_re  = FALSE,
  show_cor_fe  = FALSE,
  exponentiate = FALSE,
  digits       = 2,
  component    = NULL,
  ...
) {

  if (inherits(model, 'glmmTMB') & is.null(component))
    component = 'cond'

  vc <-
    extract_vc(
      model,
      ci_level  = ifelse(ci | inherits(model, 'gam'), .95, 0),
      digits    = digits,
      show_cor  = show_cor_re,
      component = component
    )

  if (!is.null(vc)) {

    if (show_cor_re == TRUE) {
      if (inherits(model, 'gam')) {
        cors <- 'Not estimated for gam.'
      } else {
        cors <- vc$Cor
        vc   <- vc$`Variance Components`
      }
    }

    vc <- vc %>%
      dplyr::rename_at(
        dplyr::vars(dplyr::matches('group|effect|^var')),
        totitle
      ) %>%
      dplyr::rename_at(dplyr::vars(dplyr::matches('^sd')), toupper)


    ### Print re part ----

    message("\nVariance Components:\n")

    print(format(data.frame(vc), nsmall = digits), row.names = FALSE)

    if (show_cor_re == TRUE) {
      # correlations
      message("\nCorrelation of Random Effects:\n")

      if (inherits(model, 'gam'))
        print(cors)
      else
        if (length(cors) == 1){

          print(format(data.frame(cors[[1]]), nsmall = digits))

        } else {
          # pretty printing of multiple matrices
          nams     = names(cors)
          nams[1]  = paste0(nams[1], '\n')
          nams[-1] = paste0('\n', nams[-1], '\n')

          purrr::map2(cors, nams, function(mat, name) {
            # cat('\n\n')
            message(name)
            print(format(data.frame(mat), nsmall = digits))
          })
        }

    }
  }


  ### Print fe part ----

  fe <-
    extract_fixed_effects(
      model,
      digits       = digits,
      component    = component,
      exponentiate = exponentiate
    ) %>%
    dplyr::rename_with(
      totitle,
      .cols = dplyr::matches('term|value|^z$|p_value|^low|^up')
    ) %>%
    dplyr::rename_with(toupper, .cols = dplyr::matches('se'))

  message("\nFixed Effects:\n")

  print(format(data.frame(fe), nsmall = digits), row.names = FALSE)

  if (show_cor_fe == TRUE) {
    # re part
    message("\nCorrelation of Fixed Effects:\n")

    # issues
    # merMod: as.matrix
    # glmmTMB: extract component (add as argument later)
    # mgcv: indexing to only include non-smooth terms for gam

    fe_vc_init <- stats::vcov(model)

    if (is.list(fe_vc_init))
      fe_vc_init <- fe_vc_init[[component]]

    fe_vc <- as.matrix(
      fe_vc_init[seq_along(fe$Term), seq_along(fe$Term), drop = FALSE]
    )

    fe_vc <- remove_parens(fe_vc)

    print(
      format(
        data.frame(
          round(
            stats::cov2cor(fe_vc),
            digits = digits
          )
        ),
        nsmall = digits
      )
    )

  }

  invisible(list(vc = vc, fe = fe))
}


#' @rdname summarize_model
#' @export
summarise_model <- summarize_model


