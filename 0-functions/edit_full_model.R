edit_full_model = function(model_lines, outfile, eggs = T, eggs_trend = T, sex_trend = T, mat_trend = T, vuln_trend = T, keep_whitespace = F, keep_comments = F) {
  
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
  
  # 4.) remove "%_%" instances
  model_lines = stringr::str_remove(model_lines, " %_%")
  
  ### ALTERATIONS TO CODE BASED ON MODEL VERSION ###
  
  # handle vuln: 
    # vuln_trend == T: pearson coefficients informed using rlm from each individual year
    # vuln_trend == F: pearson coefficients informed using mean over years for rlm
  if (!vuln_trend) {
    match = model_lines[stringr::str_detect(model_lines, "v_term_2\\[t,a,s,m\\] <- ")]
    replacement = stringr::str_replace(match, pattern = "rlm\\[t,a,s,m\\]", replacement = "mean(rlm[1:nt,a,s,m])")
    model_lines[which(model_lines == match)] = replacement
  }
  
  # handle eggs: 
    # eggs == T: R = ricker(eggs) * noise
    # eggs == F: R = ricker(spawners) * noise
  if (!eggs) {
    match = model_lines[stringr::str_detect(model_lines, "log_Rmean1\\[y\\] <-")]
    model_lines[which(model_lines == match)] = stringr::str_replace_all(match, "eggs", "S")
  }
  
  # handle eggs_trend: 
    # eggs_trend == T: annual fecundity uses year-specific length data
    # eggs_trend == F: annual fecundity uses the mean across all years
  if (!eggs_trend) {
    match = model_lines[stringr::str_detect(model_lines, "eggs_tas\\[t,a,s\\] <-")]
    replacement = stringr::str_replace(match, pattern = "fecund\\[t,a,s\\]", replacement = "mean(fecund[1:nt,a,s])")
    model_lines[which(model_lines == match)] = replacement
  }
  
  # handle sex_trend: 
  # sex_trend == T: logistic sex composition regardless of age, time trend for brood year
  # sex_trend == F: time invariant sex composition (slope not estimated but fixed at zero)
  if (!sex_trend) {
    match = model_lines[stringr::str_detect(model_lines, "b1_sex ~ ")]
    white_space =  unlist(stringr::str_extract_all(match, "  +"))
    replace = "b1_sex <- 0"
    model_lines[which(model_lines == match)] = paste(white_space, replace, sep = "")
  }
  
  # handle sex_trend: 
  # mat_trend == T: logistic maturation for each sex, time trend for brood year
  # mat_trend == F: time invariant maturation (slopes not estimated but fixed at zero)
  if (!mat_trend) {
    match = model_lines[stringr::str_detect(model_lines, "b1_mat\\[s,a\\] ~ ")]
    white_space = unlist(stringr::str_extract_all(match, "  +"))
    replace = "b1_mat[s,a] <- 0"
    model_lines[which(model_lines == match)] = paste(white_space, replace, sep = "")
  }
  
  cat(paste0(model_lines, collapse = "\n"), file = outfile)
  
}
