
# assign a model identifier based the saved meta data
id_model = function(meta, unit = T, trends = T, rand_age = F, src = F, ess = F) {
  with(meta, {
    
    # blank id
    id = ""
    
    # extract and name the unit
    z_unit = ifelse(z_unit == "fish_count", "N",
                  ifelse(z_unit == "egg_count", "E",
                         ifelse(z_unit == "egg_mass", "EM", NA)))
    

    # (if requested) include the z unit
    if (unit) {
      id = z_unit
    }
    
    # (if requested) include the trends
    if (trends) {
      # extract and name the trends
      A = ifelse(age_trend, "A", "")
      S = ifelse(sex_trend, "S", "")
      L = ifelse(len_trend, "L", "")
      trends = paste0(A, S, L, collapse = "")
      trends = ifelse(trends == "", "0", trends)
      id = paste(id, trends, sep = ifelse(nchar(id) != 0, "-", ""))
    }

    # (if requested) include an identifier for if the model has dirichlet ages or not
    if (rand_age) {
      d = ifelse(rand_age, "D", "M")
      id = paste(id, d, sep = ifelse(nchar(id) != 0, "-", ""))
    }
    
    # (if requested) include an identifier for the source of the coefficients to inform length-based z
    if (src & z_unit != "N") {
      src = meta$z_src
      id = paste(id, src, sep = ifelse(nchar(id) != 0, "-", ""))
    }
    
    # (if requested) include an identifer for the weighting of age composition data
    if (ess) {
      esc = paste0("E_", substr(meta$esc_n_eff, 1, 1), str_extract(meta$esc_n_eff, "[:digit:][:digit:][:digit:]$"))
      com = paste0("C_", substr(meta$com_n_eff, 1, 1), str_extract(meta$com_n_eff, "[:digit:][:digit:][:digit:]$"))
      sub = paste0("S_", substr(meta$sub_n_eff, 1, 1), str_extract(meta$sub_n_eff, "[:digit:][:digit:][:digit:]$"))
      ess = paste(esc, com, sub, sep = "-")
      id = paste(id, ess, sep = ifelse(nchar(id) != 0, "-", ""))
    }
    id
  })
}
