gen_inits = function(eggs = T, sex_trend = T, mat_trend = T) {
  
  inits_list = list(
    D_scale = runif(1, 0.1, 0.2),
    beta = 1/runif(1, 180000, 250000),
    log_resid_0 = runif(1, -0.1, 0.1),
    log_mean_R0 = log(200000) + rnorm(1, 0, 0.2),
    phi = runif(1, 0.3, 0.5),
    tau_R_white = runif(1, 4, 16),
    tau_R0 = runif(1, 4, 16),
    Vtau = runif(1, 1.9, 1.95),
    Vsig = runif(1, 0.175, 0.225),
    Vtha = runif(1, 0.55, 0.65),
    Vlam = runif(1, -0.6, -0.4),
    b0_mat = matrix(c(runif(3, c(-1.5, 1.5, 1.5), c(-0.5, 2.5, 2.5)), NA), 2, na, byrow = T),
    b0_sex = runif(1, -0.1, 0.1)
  )
  
  if (eggs) {
    log_alpha = list(log_alpha = log(0.002) + rnorm(1, 0, 0.001))
  } else {
    log_alpha = list(log_alpha = log(6) + rnorm(1, 0, 0.1))
  }
  
  inits_list = append(inits_list, log_alpha)
  
  if (mat_trend) {
    b1_mat = list(b1_mat = matrix(c(runif(3, c(0, 0, 0.05), c(0.1, 0.1, 0.15)), NA), 2, na, byrow = T))
    inits_list = append(inits_list, b1_mat)
  }
  
  if (sex_trend) {
    b1_sex = list(b1_sex = runif(1, -0.1, 0.1))
    inits_list = append(inits_list, b1_sex)
  }
  
  inits_list
}

