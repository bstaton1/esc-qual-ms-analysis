edit_full_model = function(model_lines, outfile, len_trend, sex_trend, age_trend, keep_whitespace = F, keep_comments = F) {
  
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
  
  # handle len_trend: 
    # len_trend == T: annual z uses year-specific length data
    # len_trend == F: annual z uses the mean across all years
  if (!len_trend) {
    match = model_lines[stringr::str_detect(model_lines, "Z_tas\\[t,a,s\\] <-")]
    replacement = stringr::str_replace(match, pattern = "z\\[t,a,s\\]", replacement = "mean(z[1:nt,a,s])")
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
  
  # handle age_trend: 
  # age_trend == T: baseline category logit maturation for each sex, time trend for brood year
  # age_trend == F: time invariant maturation (slopes not estimated but fixed at zero)
  if (!age_trend) {
    match = model_lines[stringr::str_detect(model_lines, "b1_mat\\[s,a\\] ~ ")]
    white_space = unlist(stringr::str_extract_all(match, "  +"))
    replace = "b1_mat[s,a] <- 0"
    model_lines[which(model_lines == match)] = paste(white_space, replace, sep = "")
  }
  
  cat(paste0(model_lines, collapse = "\n"), file = outfile)
}
