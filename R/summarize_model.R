#' Summarize a mixed model
#'
#' @param model A supported model.
#' @param ci Whether to include a 95% uncertainty interval. Default is TRUE.
#' @param cor_re Whether to include the correlations of the random effects.
#'   Default is FALSE.
#' @param cor_fe Whether to include the correlations of the fixed effects.
#'   Default is FALSE.
#' @param digits Digits to display.
#' @param component For glmmTMB objects, which of the three components 'cond',
#'   'zi' or 'other' to select. Default is cond. Minimal testing on other
#'   options.
#' @param ... Not used at present. May allow models specific functionality.
#'
#' @importFrom stats vcov
#' @examples
#' library(lme4)
#' library(mixedup)
#'
#' lmer_mod <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
#' summarize_model(lmer_mod)
#'
#'
#' @export
summarize_model <- function(
  model,
  ci = TRUE,
  cor_re = FALSE,
  cor_fe = FALSE,
  digits = 2,
  component = NULL,
  ...
) {

  if (inherits(model, 'glmmTMB') & is.null(component))
    component = 'cond'

  vc <-
    extract_vc(
      model,
      ci_level = ifelse(ci | inherits(model, 'gam'), .95, 0),
      digits = digits,
      show_cor = cor_re,
      component = component
    )

  if (cor_re == TRUE) {
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

  fe <- extract_fixed_effects(model, digits = digits, component = component) %>%
    dplyr::rename_at(
      dplyr::vars(dplyr::matches('term|value|^z$|p_value|^low|^up')),
      totitle
    ) %>%
    dplyr::rename_at(dplyr::vars(dplyr::matches('se')), toupper)

  ### Print re part ----

  message("\nVariance Components:\n")

  print(format(data.frame(vc), nsmall = digits), row.names = FALSE)

  if (cor_re == TRUE) {
    # correlations
    message("\nCorrelation of Random Effects:\n")

    if (inherits(model, 'gam'))
      print(cors)
    else
      if (length(cors) == 1){

        print(format(data.frame(cors[[1]]), nsmall = digits))

      } else {
        # pretty printing of multiple matrices
        nams = names(cors)
        nams[1] = paste0(nams[1], '\n')
        nams[-1] = paste0('\n', nams[-1], '\n')

        purrr::map2(cors, nams, function(mat, name) {
          # cat('\n\n')
          message(name)
          print(format(data.frame(mat), nsmall = digits))
        })
      }

  }

  ### Print fe part ----

  message("\n\nFixed Effects:\n")

  print(format(data.frame(fe), nsmall = digits), row.names = FALSE)

  if (cor_fe == TRUE) {
    # re part
    message("\nCorrelation of Fixed Effects:\n")

    # issues
    # merMod: as.matrix
    # glmmTMB: extract component (add as argument later)
    # mgcv: indexing to only include non-smooth terms for gam

    fe_vc_init <- stats::vcov(model)
    if (is.list(fe_vc_init)) fe_vc_init <- fe_vc_init[[component]]

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


