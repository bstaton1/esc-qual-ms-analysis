
edit_full_model = function(model_lines, outfile, z_unit, sex_trend, age_trend, rand_age, keep_whitespace = F, keep_comments = F) {
  
  # 1.) replace function() with model {
  model_lines[model_lines == "function() {"] = "model {"
  
  # 2.) remove whitespace if requested
  if (!keep_whitespace) {
    model_lines = model_lines[-which(stringr::str_detect(model_lines, "^  +$"))]
  } 
  
  # 3.) remove comments if requested
  if (!keep_comments) {
    model_lines = model_lines[-which(stringr::str_detect(model_lines, "^  +#"))]
  } 
  
  # 4.) remove "%_%" instances, but keep the T() or I() part
  model_lines = stringr::str_remove(model_lines, " %_%")
  
  ### ALTERATIONS TO CODE BASED ON MODEL VERSION ###
  
  # handle reproductive unit
    # z_unit == "fish_count":  alpha ~ U(0, 20)
    # z_unit != "fish_count":  alpha ~ U(0, 0.1)
  if (z_unit != "fish_count") {
    match = model_lines[stringr::str_detect(model_lines, "alpha ~ ")]
    replacement = stringr::str_replace(match, pattern = "dunif\\(0, 20\\)", replacement = "dunif\\(0, 0.1\\)")
    model_lines[which(model_lines == match)] = replacement
  }
  
  # handle sex_trend: 
  # sex_trend == T: logistic sex composition regardless of age, time trend for brood year
  # sex_trend == F: time invariant sex composition (slope not estimated but fixed at zero)
  if (!sex_trend) {
    match = model_lines[stringr::str_detect(model_lines, "delta_1 ~ ")]
    white_space =  unlist(stringr::str_extract_all(match, "  +"))
    replace = "delta_1 <- 0"
    model_lines[which(model_lines == match)] = paste(white_space, replace, sep = "")
  }
  
  # handle age_trend: 
  # age_trend == T: baseline category logit maturation for each sex, time trend for brood year
  # age_trend == F: time invariant maturation (slopes not estimated but fixed at zero)
  if (!age_trend) {
    match = model_lines[stringr::str_detect(model_lines, "gamma_1\\[s,a\\] ~ ")]
    white_space = unlist(stringr::str_extract_all(match, "  +"))
    replace = "gamma_1[s,a] <- 0"
    model_lines[which(model_lines == match)] = paste(white_space, replace, sep = "")
  }
  
  # handle brood year random age variability
  # cuts the random link at the p node assignment
  # if rand_age == T: p[y,,] are dirichlet distributed around mu_pi_mat
  # if rand_age == F: p[y,,] are are set to mu_pi_mat[y,]
  if (!rand_age) {
    match = model_lines[stringr::str_detect(model_lines, "p\\[y,a,s\\] <- ")]
    white_space = unlist(stringr::str_extract_all(match, "  +"))
    replace = "p[y,a,s] <- mu_pi_mat[y,a,s]"
    model_lines[which(model_lines == match)] = paste(white_space, replace, sep = "")
    
    # remove D_scale, D_sum, gamma, and g
    D_scale_line = stringr::str_which(model_lines, "D_scale ~ ")
    D_sum_line = stringr::str_which(model_lines, "D_sum <- ")
    gamma_line = stringr::str_which(model_lines, "gamma\\[y,a,s\\] <- ")
    g_line = stringr::str_which(model_lines, "g\\[y,a,s\\] ~ ")
    model_lines = model_lines[-c(D_scale_line, D_sum_line, gamma_line, g_line)]
  }
  
  cat(paste0(model_lines, collapse = "\n"), file = outfile)
}

