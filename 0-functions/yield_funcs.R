
# Function to calculate yield at a given F for a particular posterior sample and vuln scenario
yield = function(log_F_max, i, post.samp, vuln) {  
  
  # extract column names
  cn = colnames(post.samp)
  
  # extract and name the relevant SRA paramters
  alpha = unname(post.samp[i,"alpha"])
  beta = unname(post.samp[i,"beta"])
  phi = unname(post.samp[i,"phi"])
  sigma = unname(post.samp[i,"sigma_R"])
  
  # extract and name the relevant age/sex-structured quantities
  pi_as = unname(post.samp[i,str_detect(cn, "pi")])
  z_as = unname(post.samp[i,str_detect(cn, "z")])
  if (vuln %in% c("mesh8", "mesh6")) {
    vuln_as = unname(post.samp[i,str_detect(cn, vuln)])
  } else {
    vuln_as = rep(1, 8)
  }
  
  # get unfished z per recruit (zPR0 - average reproductive units per spawner in unfished condition, z_as weighted by maturity)
  zPR0_as = z_as * pi_as
  zPR0 = sum(zPR0_as)
  
  # get unfished equilibrium recruitment (R0)
  alphac = exp(log(alpha) + (sigma^2)/2/(1 - phi^2)) # corrected alpha
  R0 = log(alphac * zPR0)/(beta * zPR0)
  
  # get fishing mortality at age (F_a) and fished z per recruit at age (zPR_f_a)
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
  
  # proportion of run at age/sex
  names(pi_as) = paste0("pi_", rep(c("F", "M"), each = 4), rep(4:7, 2))
  
  # proportion of spawners at age/sex
  p_as = S_as/sum(S_as)
  names(p_as) = paste0("p_", rep(c("F", "M"), each = 4), rep(4:7, 2))
  
  # exploitation rate of spawners at age/sex
  U_as = H_as/N_as
  names(U_as) = paste0("U_", rep(c("F", "M"), each = 4), rep(4:7, 2))
  
  # overall exploitation rate
  U_tot = H/sum(N_as)
  
  # exploitation rate by sex
  U_s = c(U_F = sum(H_as[1:4])/sum(N_as[1:4]), U_M = sum(H_as[5:8])/sum(N_as[5:8]))
  
  output = c(
    H = H,
    S = S,
    R = RF,
    p_female = sum(S_as[1:4])/S,
    U_tot = U_tot,
    U_s,
    pi_as,
    p_as,
    U_as,
    F_max = F_max,
    Z_million = Z/1e6
    )

  return(output)
}

### functions to minimize: returns negative values because optim minimizes the objective
# pass this function to optim to find equilibrium states at at maximum yield (q = "H") or recruitment (q = "R")
# option to include a sex penalty - won't consider F's which place the ratio below
# some number - 1 is used below
func_to_min = function(log_F_max, i, q, post.samp, vuln) {
  yield(log_F_max, i, post.samp, vuln)[q] * -1
}

# wrapper to find equilibrium quantities
# searches for fishing mortality that maximizes harvest (q = "H") or recruitment (q = "R")
# for each posterior sample and each vulnerability scenario
eq_search = function(post.samp, q, silent = F) {
  
  # how many posterior samples?
  n_samp = nrow(post.samp)
  
  # what are the names of the output?
  nms = names(yield(
    log_F_max = log(1), 
    i = 1, post.samp = post.samp,
    vuln = "mesh8"
  ))
  
  # build a container for output: [sample,quantity,vuln]
  out = array(NA, dim = c(n_samp, length(nms), 3))
  
  # the vuln scenarios
  v_scenarios = c("mesh8", "mesh6", "flat")
  
  # loop over scenarios and samples
  for (v in 1:length(v_scenarios)) {
    for (i in 1:n_samp) {
      if (!silent) progress_updater(i, n_samp, v_scenarios[v], indent = 5)
      
      # obtain F that maximizes objective (R or H)
      fit_log_F_max = 
        optim(
          par = log(0.5), 
          fn = func_to_min, 
          method = "Brent", lower = -10, upper = 3,
          q = q, i = i, post.samp = post.samp, vuln = v_scenarios[v]
        )$par
      
      # plug this in to get all eq. quantities
      out[i,,v] = 
        yield(
          log_F_max = fit_log_F_max, 
          i = i, post.samp = post.samp,
          vuln = v_scenarios[v]
        )
    }
  }
  
  if (!silent) cat("\n")
  
  # summarize: extract posterior quantiles
  out = array(
    c(
      apply(out[,,1], 2, summ, p = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975), na.rm = T)[3:9,],
      apply(out[,,2], 2, summ, p = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975), na.rm = T)[3:9,],
      apply(out[,,3], 2, summ, p = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975), na.rm = T)[3:9,]
    ),
    dim = c(7, length(nms), 3)
  )
  
  # set the names of the output
  dimnames(out) = list(
    names(summ(rnorm(10), p = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)))[3:9],
    nms,
    v_scenarios
  )
  
  # return the output
  out
}
