
yield = function(log_F_max, i, post.samp, vuln) {  
  
  # extract column names
  cn = colnames(post.samp)
  
  # extract and name the relevant SRA paramters
  alpha = unname(post.samp[i,"alpha"])
  beta = unname(post.samp[i,"beta"])
  phi = unname(post.samp[i,"phi"])
  sigma = unname(post.samp[i,"sigma_R_white"])
  
  # extract and name the relevant age/sex-structured quantities
  pi_as = unname(post.samp[i,stringr::str_detect(cn, "pi")])
  vuln_as = unname(post.samp[i,stringr::str_detect(cn, vuln)])
  z_as = unname(post.samp[i,stringr::str_detect(cn, "z")])
  
  # get unfished z per recruit (zPR0 - average reproductive units per spawner in unfished condition, z_as weighted by maturity)
  zPR0_as = z_as * pi_as
  zPR0 = sum(zPR0_as)
  
  # get unfished equilibrium recruitment (R0)
  alphac = exp(log(alpha) + (sigma^2)/2/(1 - phi^2)) # corrected alpha
  R0 = log(alphac * zPR0)/(beta * zPR0)
  
  # get fishing mortality at age (F_a) and fished z per recruit at age (zPRO_f_a)
  F_max = exp(log_F_max)
  F_as = F_max * vuln_as
  U_as = 1 - exp(-F_as)
  zPR_F_as = (1 - U_as) * z_as * pi_as
  zPR_F = sum(zPR_F_as)
  
  # get fished equilibrium recruits, N_a, esc_a, harv_a
  RF = log(alphac * zPR_F)/(beta * zPR_F)
  N_as = RF * pi_as
  S_as = N_as * (1 - U_as)
  S = sum(S_as)
  Z_as = S_as * z_as
  Z = sum(Z_as)
  H_as = N_as * U_as
  H = sum(H_as)
  
  output = c(
    H = H,
    S = S,
    R = RF,
    Z_million = Z/1e6
    )
  
  return(output)
}

yield_min = function(log_F_max, i, post.samp, vuln) {
  yield(log_F_max, i, post.samp, vuln)["H"] * -1
}

msy_search = function(post.samp, silent = F) {
  n_samp = nrow(post.samp)
  out = array(NA, dim = c(n_samp, 4, 2))
  v_scenarios = c("unr", "res")
  for (v in 1:2) {
    for (i in 1:n_samp) {
      if (!silent) cat("\r", " ", floor(i/n_samp * 100), "% (", v_scenarios[v], ")", sep = "")
      fit_log_F_max = 
        optim(
          par = log(0.5), 
          fn = yield_min, 
          method = "Brent", lower = -10, upper = 3,
          i = i, post.samp = post.samp, vuln = v_scenarios[v]
        )$par
      
      out[i,,v] = 
        yield(
          log_F_max = fit_log_F_max, 
          i = i, post.samp = post.samp,
          vuln = v_scenarios[v]
        )
    }
  }
  
  if (!silent) cat("\n")
  
  out = array(
    c(
      apply(out[,,1], 2, StatonMisc::summ, p = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975))[3:9,],
      apply(out[,,2], 2, StatonMisc::summ, p = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975))[3:9,]
    ),
    dim = c(7, 4, 2)
  )
  
  dimnames(out) = list(
    names(StatonMisc::summ(rnorm(10), p = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)))[3:9],
    c("H", "S", "R", "Z_million"),
    v_scenarios
  )
  out
}
