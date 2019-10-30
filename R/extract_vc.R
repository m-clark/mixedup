extract_vc <- function(model) {
  if (!inherits(model, c('merMod', 'glmmTMB')))
    stop('This only works for merMod objects from lme4 or models from glmmTMB.')

  UseMethod('extract_random_coef')
}

extract_vc.merMod <- function(model, ci = .95, show_cor = FALSE, digits = 3) {
  vc_mat = VarCorr(model)

  # if (ci > 0) # add check on level because...
  #   ci = confint(model) # I'm not sure what thought went into the rownames but this is almost useless output
  vc = data.frame(vc_mat)
  colnames(vc) = c('group', 'coefficient', 'coefficient_2', 'variance', 'sd')

  # cleanup/add
  vc = dplyr::filter(vc, is.na(coefficient) | is.na(coefficient_2))
  vc = dplyr::mutate(vc,
                     proportion = variance / sum(variance),
                     coefficient = gsub(coefficient, pattern = '[\\(,\\)]', replacement = ''),
                     coefficient = ifelse(is.na(coefficient), '', coefficient))
  vc = dplyr::select(vc, -coefficient_2)
  vc = dplyr::mutate_if(vc, is.numeric, round, digits = digits)

  if (show_cor) {
    cormats = lapply(vc_mat, attr, 'correlation')

    remove_parens = function(x) {
      colnames(x) = gsub(colnames(x), pattern = '[\\(,\\)]', replacement = '')
      rownames(x) = colnames(x)
      x
    }

    cormats <- lapply(cormats, remove_parens)
    cormats <- lapply(cormats, round, digits = digits)

    return(list(`Variance Components` = vc, Cor = cormats))
  }

  vc
}

library(lme4)
lmer_2 <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
debugonce(extract_vc.merMod)
extract_vc.merMod(lmer_2)
extract_vc.merMod(lmer_2, show_cor = T)


# nonsensically complex model
lmer_3 <- lmer(y ~ service + (1 + as.numeric(lectage)| s) + (1 + as.numeric(studage)| d) + (1|dept), data = InstEval[1:5000, ])
extract_vc.merMod(lmer_3)

extract_vc.merMod(lmer_3, pretty = T)

lmer_4 <- lmer(y ~ service + (1 + as.numeric(lectage) + as.numeric(studage) + service| d) , data = InstEval[1:5000, ])
extract_vc.merMod(lmer_4)

extract_vc.merMod(lmer_4, show_cor = T)
