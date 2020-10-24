# keep_t = all_keep_t
# keep_y = all_keep_y
# len_trend = F
# silent = F
# post = post$samples

prep_samples = function(post, keep_t = NULL, keep_y = NULL, silent = F) {
  
  # keep all years if not specified
  if (is.null(keep_t)) keep_t = 1:nt
  if (is.null(keep_y)) keep_y = 1:ny
  # age and sex id vector
  as = c(paste("f", a_min:a_max, sep = ""), paste("m", a_min:a_max, sep = ""))
  
  # indicators for which indices correspond to which sexes/gears
  mesh8 = 1
  mesh6 = 2
  female = 1
  male = 2
  
  # extract necessary SRA parameters
  sr_params = post_subset(post, c("alpha", "^beta$", "sigma_R_white", "phi"), T)
  n_samp = as.list(post_dim(post))$saved
  # extract the vulnerability schedules
  # averaging yearly values by age, sex, and gear over some time period
  v = post_subset(post, "^v", T)
  
  mean_v = t(sapply(1:n_samp, function(i) {
    if (!silent) StatonMisc::progress_updater(i, n_samp, grp = "Mean Vulnerability", indent = 5)
    
    v_out = NULL
    for (mesh in c(mesh8, mesh6)) {
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
  colnames(mean_v) = c(paste(as, "v_mesh8", sep = "_"), paste(as, "v_mesh6", sep = "_"))
  
  # extract the probability of maturation at age and sex
  pi = post_subset(post, "pi", T)
  psi = post_subset(post, "psi", T)
  
  mean_pi = t(sapply(1:n_samp, function(i) {
    if (!silent) StatonMisc::progress_updater(i, n_samp, grp = "Mean Maturity", indent = 5)
    
    nR = 1e10
    
    pi_mat = matrix(NA, na, 2)
    for (sex in c(female, male)) {
      for (age in 1:4) {
        keep_columns = paste("pi[", keep_y, ",", age, ",", sex, "]", sep = "")
        
        pi_mat[age,sex] = mean(pi[i,keep_columns])
      }
    }
    
    psi = mean(psi[i,keep_y])
    
    nRs = c(psi, 1 - psi) * nR
    nRsa = t(apply(pi_mat, 1, function(x) x * nRs))
    pRsa = as.numeric(nRsa/sum(nRsa))
    pRsa
  }))
  colnames(mean_pi) = paste(as, "pi", sep = "_")
  if (!silent) cat("\n")
  
  # extract average z value over the time period of interest
  mean_z = rep(NA, 2 * na)
  mean_z[1:na] = colMeans(z[keep_t,,1])
  mean_z[(na+1):(na*2)] = colMeans(z[keep_t,,2])
  mean_z = matrix(mean_z, n_samp, na * 2, byrow = T)
  colnames(mean_z) = paste(as, "z", sep = "_")
  
  # combine them all together
  post.samp = cbind(sr_params, mean_z, mean_pi, mean_v)
  post.samp
}
