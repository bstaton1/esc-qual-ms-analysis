gen_inits = function(z_unit, sex_trend, age_trend, rand_age) {
  
  inits_list = list(
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
    gamma_0 = matrix(c(runif(3, c(-1.5, 1.5, 1.5), c(-0.5, 2.5, 2.5)), NA), 2, na, byrow = T),
    delta_0 = runif(1, -0.1, 0.1)
  )
  
  if (rand_age) {
    inits_list = append(inits_list, list(D_scale = runif(1, 0.1, 0.2)))
  }
  
  if (z_unit %in% c("egg_count", "egg_mass")) {
    alpha = list(alpha = exp(log(0.002) + rnorm(1, 0, 0.001)))
  } else {
    if (z_unit == "fish_count") {
      alpha = list(alpha = exp(log(6) + rnorm(1, 0, 0.1)))
    } else {
      stop ("z_unit must be one of 'fish_count', 'egg_count', or 'egg_mass'")
    }
  }
  
  inits_list = append(inits_list, alpha)
  
  if (age_trend) {
    gamma_1 = list(gamma_1 = matrix(c(runif(3, c(0, 0, 0.05), c(0.1, 0.1, 0.15)), NA), 2, na, byrow = T))
    inits_list = append(inits_list, gamma_1)
  }
  
  if (sex_trend) {
    delta_1 = list(delta_1 = runif(1, -0.1, 0.1))
    inits_list = append(inits_list, delta_1)
  }
  
  inits_list
}

