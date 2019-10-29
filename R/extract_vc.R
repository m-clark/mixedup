extract_vc <- function(model) {
  if (!inherits(model, c('merMod', 'glmmTMB')))
    stop('This only works for merMod objects from lme4 or models from glmmTMB.')

  UseMethod('extract_random_coef')
}

extract_vc.merMod <- function(model, show_cor = FALSE, digits = 3) {
  vc_mat = VarCorr(model)


  if (show_cor) {
    vc = data.frame(VarCorr(lmer_4))
    colnames(vc) = c('group', 'variable_1', 'variable_2')
    vc
  }

  # remove covariances/correlations
  vc = formatVC(vc_mat)
  vc = data.frame(vc, stringsAsFactors = FALSE)
  vc$Name = gsub(vc$Name, pattern = '[\\(,\\)]', replacement = '')
  vc$Std.Dev. <- as.numeric(vc$Std.Dev.)
  vc$Corr <- as.numeric(vc$Corr)
  vc$variance <- vc$Std.Dev.^2
  vc$proportion <- vc$variance / sum(vc$variance)

  rownames(vc) = NULL

  vc = mutate(vc, Groups = ifelse(Groups == '', NA, Groups))
  vc = tidyr::fill(vc, Groups)

  vc = dplyr::rename(vc, sd = Std.Dev.)
  vc = dplyr::rename_all(vc, tolower)
  vc = dplyr::rename_at(vc,
                        vars(matches('^v[0-9]')),
                        function(x) gsub(x, pattern =  '^v', replacement =  'corr'))
  vc = dplyr::rename_at(vc,
                        vars(matches('^corr')),
                        function(x) paste0('cor_', vc$name[1:length(x)]))
  vc = dplyr::mutate(vc,
                     name = gsub(
                       name,
                       pattern = '[\\(,\\)]',
                       replacement = ''
                       )
                     )



  dplyr::mutate_if(vc, is.numeric, round, digits = digits)
}


lmer_2 <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
debugonce(extract_vc.merMod)
extract_vc.merMod(lmer_2)
extract_vc.merMod(lmer_2, pretty = T)


# nonsensically complex model
lmer_3 <- lmer(y ~ service + (1 + as.numeric(lectage)| s) + (1 + as.numeric(studage)| d) + (1|dept), data = InstEval[1:5000, ])
extract_vc.merMod(lmer_3)

extract_vc.merMod(lmer_3, pretty = T)

lmer_4 <- lmer(y ~ service + (1 + as.numeric(lectage) + as.numeric(studage) + service| d) , data = InstEval[1:5000, ])
extract_vc.merMod(lmer_4)

extract_vc.merMod(lmer_4, show_cor = T)
