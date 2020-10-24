print_start_mcmc_message = function() {
  cat("\n  |--- Running JAGS on Model ", model, " ---|\n\n", sep = "")
  
  cat("    #-------------------------------#\n")
  cat("    Model Assumptions:\n")
  cat("      -> Spawing units: ", capitalize(str_replace(z_unit, "_", " ")), "\n", sep = "")
  cat("      -> Length trend: ", ifelse(len_trend, "Yes", "No"), "\n", sep = "")
  cat("      -> Sex trend: ", ifelse(sex_trend, "Yes", "No"), "\n", sep = "")
  cat("      -> Age trend: ", ifelse(age_trend, "Yes", "No"), "\n", sep = "")
  cat("    #-------------------------------#\n")
  cat("    MCMC Settings:\n")
  cat("      -> Parallel processing: ", ifelse(parallel, "Yes", "No"), "\n", sep = "")
  cat("      -> Adapt: ", prettify(nadapt), "\n", sep = "")
  cat("      -> Burn-in: ", prettify(nburn), "\n", sep = "")
  cat("      -> Post Burn-in: ", prettify(npost), "\n", sep = "")
  cat("      -> Thinning Interval: ", prettify(nthin), "\n", sep = "")
  cat("      -> Chains: ", prettify(nchain), "\n", sep = "")
  cat("      -> Saved: ", prettify((npost/nthin) * nchain), "\n", sep = "")
  cat("    #-------------------------------#\n")
  
}
