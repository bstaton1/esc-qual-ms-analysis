print_start_mcmc_message = function() {
  cat("\n  |--- Running JAGS on Model ", model, " ---|\n\n", sep = "")
  
  cat("    #-------------------------------#\n")
  cat("    Model Assumptions:\n")
  cat("      -> Spawing units: ", StatonMisc::capitalize(stringr::str_replace(z_unit, "_", " ")), "\n", sep = "")
  cat("      -> Length trend: ", ifelse(len_trend, "Yes", "No"), "\n", sep = "")
  cat("      -> Sex trend: ", ifelse(sex_trend, "Yes", "No"), "\n", sep = "")
  cat("      -> Age trend: ", ifelse(age_trend, "Yes", "No"), "\n", sep = "")
  cat("    #-------------------------------#\n")
  cat("    MCMC Settings:\n")
  cat("      -> Parallel processing: ", ifelse(parallel, "Yes", "No"), "\n", sep = "")
  cat("      -> Adapt: ", StatonMisc::prettify(nadapt), "\n", sep = "")
  cat("      -> Burn-in: ", StatonMisc::prettify(nburn), "\n", sep = "")
  cat("      -> Post Burn-in: ", StatonMisc::prettify(npost), "\n", sep = "")
  cat("      -> Thinning Interval: ", StatonMisc::prettify(nthin), "\n", sep = "")
  cat("      -> Chains: ", StatonMisc::prettify(nchain), "\n", sep = "")
  cat("      -> Saved: ", StatonMisc::prettify((npost/nthin) * nchain), "\n", sep = "")
  cat("    #-------------------------------#\n")
  
}
