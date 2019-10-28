extract_random_coef <- function(model, which_re = NULL) {
  
  if (!inherits(model, c('merMod', 'glmmTMB')))
    stop('This only works for merMod objects from lme4 or models from glmmTMB.')
  
  UseMethod('extract_random_coef')
}

extract_random_coef.merMod <- function(model, which_re = NULL) {
  # https://stackoverflow.com/questions/26198958/extracting-coefficients-and-their-standard-error-from-lme

  if (is.null(which_re)) {
    warning('No random effect specified, using first.')
    which_re = 1
  }
  
  # call method for merMod vs. glmmTMB
  ran_coefs = coef(model)[[which_re]]
  random_effects = ranef(model, condVar = TRUE)[[which_re]]
  random_effect_covar <- attr(random_effects, "postVar")
  
  # deal with single random effect
  if (is.null(dim(random_effect_covar[,,1]))) {
    random_effect_var = matrix(random_effect_covar[1,,], ncol = 1)
  }
  else {
    random_effect_var = t(apply(random_effect_covar, 3, diag))
  }
  
  # extract only pertinent fe
  fe_names = names(fixef(model))
  re_names = colnames(random_effects)
  fixed_effect_var = diag(vcov(model))
  fixed_effect_var = fixed_effect_var[fe_names %in% re_names]
  
  se <- sqrt(sweep(random_effect_var, 2, fixed_effect_var, "+"))
  colnames(se) = re_names
  
  se = as.data.frame(se) %>% 
    dplyr::rename_all(function(nam) paste0('se_', nam))
  
  out = cbind(ran_coefs, se)
  
  out %>% 
    dplyr::rename_all(
      function(nam) gsub(nam, pattern = '[\\(, \\)]', replacement = ''))
}

extract_random_coef.glmmTMB <- function(model, which_re = NULL, zi = FALSE) {
  # https://stackoverflow.com/questions/26198958/extracting-coefficients-and-their-standard-error-from-lme

  
  if (is.null(which_re)) {
    warning('No random effect specified, using first.')
    which_re = 1
  }
  
  
  # call method for merMod vs. glmmTMB?
  
  cond_zi = 1 + zi
  ran_coefs = coef(model)[[cond_zi]][[which_re]]
  random_effects = ranef(model, condVar = TRUE)[[cond_zi]][[which_re]]
  random_effect_covar <- attr(random_effects, "condVar")
  
  # deal with single random effect
  if (is.null(dim(random_effect_covar[,,1]))) {
    random_effect_var = matrix(random_effect_covar[1,,], ncol = 1)
  }
  else {
    random_effect_var = t(apply(random_effect_covar, 3, diag))
  }
  
  # extract only pertinent fe
  fe_names = names(fixef(model)[[cond_zi]])
  re_names = colnames(random_effects)
  fixed_effect_var = diag(vcov(model)[[cond_zi]])
  fixed_effect_var = fixed_effect_var[fe_names %in% re_names]
  
  se <- sqrt(sweep(random_effect_var, 2, fixed_effect_var, "+"))
  colnames(se) = re_names
  
  se = as.data.frame(se) %>% 
    dplyr::rename_all(function(nam) paste0('se_', nam))
  
  out = cbind(ran_coefs, se)
  
  out %>% 
    dplyr::rename_all(
      function(nam) gsub(nam, pattern = '[\\(, \\)]', replacement = ''))
}