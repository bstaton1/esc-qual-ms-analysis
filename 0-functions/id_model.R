
# assign a model identifier based the saved meta data
id_model = function(meta, include_rand_age = F, include_src = F) {
  with(meta, {
    
    # extract and name the unit
    unit = ifelse(z_unit == "fish_count", "N",
                  ifelse(z_unit == "egg_count", "E", "EM"))
    
    # extract and name the trends
    A = ifelse(age_trend, "A", "")
    S = ifelse(sex_trend, "S", "")
    L = ifelse(len_trend, "L", "")
    trends = paste0(A, S, L, collapse = "")
    trends = ifelse(trends == "", "0", trends)
    
    # build the basic id
    id = paste(unit, trends, sep = "-")
    
    # (if requested) include an identifier for if the model has dirichlet ages or not
    if (include_rand_age) {
      d = ifelse(rand_age, "D", "M")
      id = paste(id, d, sep = "-")
    }
    
    # (if requested) include an identifier for the source of the coefficients to inform length-based z
    if (include_source & unit != "N") {
      src = meta$z_src
      id = paste(id, src, sep = "-")
    }
    id
  })
}
