prep_samples = function(post, keep_t = NULL, keep_y = NULL, eggs_trend = F, silent = F) {
  
  # keep all years if not specified
  if (is.null(keep_t)) keep_t = 1:nt
  if (is.null(keep_y)) keep_y = 1:ny
  # age and sex id vector
  as = c(paste("f", a_min:a_max, sep = ""), paste("m", a_min:a_max, sep = ""))
  
  # indicators for which indices correspond to which sexes/gears
  unr = 1
  res = 2
  female = 1
  male = 2
  
  # extract necessary SRA parameters
  sr_params = filter_post(post, c("alpha", "beta", "sigma_R_white", "phi"), "matrix")
  n_samp = nrow(sr_params)
  # extract the vulnerability schedules
  # averaging yearly values by age, sex, and gear over some time period
  v = filter_post(post, "^v", "matrix")
  
  if (!silent) cat("Calculating mean vulnerability by age, sex, and gear for keep_t years and each posterior sample\n")
  mean_v = t(sapply(1:nrow(sr_params), function(i) {
    if (!silent) cat("\r", " ", floor(i/n_samp * 100), "%", sep = "")
    
    v_out = NULL
    for (mesh in c(unr, res)) {
      for (sex in c(female, male)) {
        for (age in 1:4) {
          keep_columns = paste("v[", keep_t, ",", age, ",", sex, ",", mesh, "]", sep = "")
          
          v_out = c(v_out, mean(v[i,keep_columns]))
        }
      }
    }
    v_out[1:(na*2)] = v_out[1:(na*2)]/max(v_out[1:(na*2)]) 
    v_out[((na*2)+1):(na*4)] = v_out[((na*2)+1):(na*4)]/max(v_out[((na*2)+1):(na*4)]) 
    v_out
  }))
  colnames(mean_v) = c(paste(as, "v_unr", sep = "_"), paste(as, "v_res", sep = "_"))
  
  # extract the probability of maturation at age and sex
  mu_pi_mat = filter_post(post, "mu_pi_mat", "matrix")
  mu_pi_f = filter_post(post, "mu_pi_f", "matrix")
  
  if (!silent) cat("\nCalculating mean maturity by age, sex, for keep_y years and each posterior sample\n")
  mean_pi = t(sapply(1:nrow(sr_params), function(i) {
    if (!silent) cat("\r", " ", floor(i/n_samp * 100), "%", sep = "")
    
    nR = 1e10
    
    pi_mat = matrix(NA, na, 2)
    for (sex in c(female, male)) {
      for (age in 1:4) {
        keep_columns = paste("mu_pi_mat[", keep_y, ",", age, ",", sex, "]", sep = "")
        
        pi_mat[age,sex] = mean(mu_pi_mat[i,keep_columns])
      }
    }
    
    pi_f = mean(mu_pi_f[i,keep_y])
    
    nRs = c(pi_f, 1 - pi_f) * nR
    nRsa = t(apply(pi_mat, 1, function(x) x * nRs))
    pRsa = as.numeric(nRsa/sum(nRsa))
    pRsa
  }))
  colnames(mean_pi) = paste(as, "pi", sep = "_")
  if (!silent) cat("\n")
  # extract average fecundity over the time period of interest
  if (eggs_trend) {
    mean_fecund = colMeans(fecund[keep_t,,1])
  } else {
    mean_fecund = colMeans(fecund[1:nt,,1])
  }
  mean_fecund = c(mean_fecund, rep(0, na))
  mean_fecund = matrix(mean_fecund, nrow(sr_params), na * 2, byrow = T)
  colnames(mean_fecund) = paste(as, "fecund", sep = "_")
  
  # combine them all together
  post.samp = cbind(sr_params, mean_fecund, mean_pi, mean_v)
  post.samp
}
