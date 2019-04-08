

yield = function(log_F_max, i, post.samp, vuln, spawn_units = "fish") {  
  
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
  if (spawn_units %in% c("fish", "eggs")) {
    if (spawn_units == "fish") {
      fec_as = rep(1, 8)
    } else {
      fec_as = unname(post.samp[i,stringr::str_detect(cn, "fecund")])
    }
  } else {
    stop("'spawn_units' must be one of 'fish' or 'eggs'")
  }
  
  F_max = exp(log_F_max)
  
  # get unfished eggs per recruit (EPR0)
  EPR0_as = fec_as * pi_as
  EPR0 = sum(EPR0_as)
  
  # get unfished equilibrium recruitment (R0)
  alpha.c = exp(log(alpha) + (sigma^2)/2/(1 - phi^2))
  R0 = log(alpha.c * EPR0)/(beta * EPR0)
  
  # get fishing mortality at age (F_a) and fished eggs per recruit at age (EPRO_f_a)
  F_as = F_max * vuln_as
  U_as = 1 - exp(-F_as)
  EPR_f_as = (1 - U_as) * fec_as * pi_as
  EPR_f = sum(EPR_f_as)
  
  # get fished equilibrium recruits, N_a, esc_a, harv_a
  Rf = log(alpha.c * EPR_f)/(beta * EPR_f)
  N_as = Rf * pi_as
  esc_as = N_as * (1 - U_as)
  esc = sum(esc_as)
  eggs_as = esc_as * fec_as
  eggs = sum(eggs_as)
  harv_as = N_as * U_as
  harv = sum(harv_as)
  
  output = c(
    harv = harv,
    esc = esc,
    recruits = Rf,
    million_eggs = eggs/1e6
    )
  
  return(output)
}

yield_min = function(log_F_max, i, post.samp, vuln, spawn_units = "fish") {
  yield(log_F_max, i, post.samp, vuln, spawn_units)["harv"] * -1
}

msy_search = function(post.samp, spawn_units = "fish", silent = F) {
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
          i = i, post.samp = post.samp, vuln = v_scenarios[v], spawn_units = spawn_units
        )$par
      
      out[i,,v] = 
        yield(
          log_F_max = fit_log_F_max, 
          i = i, post.samp = post.samp, vuln = v_scenarios[v], spawn_units = spawn_units
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
    c("harv", "esc", "recruits", "million_eggs"),
    v_scenarios
  )
  
  out
}



