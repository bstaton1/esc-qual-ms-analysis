get_EG = function(post.samp, spawn_units, vuln, Rmax_p = 0.9, Rmax_prob = 0.9,
                  msy_p = 0.9, msy_prob = 0.9, S_breaks = seq(0, 300000, 1000), 
                  silent = F) {
  
  # will calculate equilibrium yield, escapement, and recruitment at each F
  nF = 1000
  F_range = seq(0.0001, 5, length = nF)
  
  # number of posterior samples
  n_samp = nrow(post.samp)
  
  # containers for output
  Rout = matrix(NA, n_samp, length(S_breaks) - 1); colnames(Rout) = 1:(length(S_breaks) - 1)
  Hout = matrix(NA, n_samp, length(S_breaks) - 1); colnames(Hout) = 1:(length(S_breaks) - 1)
  
  for (i in 1:n_samp) {
    if (!silent) cat("\r", round(i/n_samp, 2) * 100, "%", sep = "")
    
    # calculate equilibrium H, S, and R at each F for this MCMC sample
    tmp = matrix(NA, nF, 3)
    for (f in 1:nF) {
      tmp[f,] = yield(
        log_F_max = log(F_range[f]), i = i, 
        post.samp = post.samp, 
        vuln = vuln, spawn_units = spawn_units
      )[c("harv", "recruits", "esc")]
    }
    colnames(tmp) = c("harv", "recruits", "esc")
    
    # create a bin for the resulting escapements at each F  
    tmp = cbind(tmp, esc_bin = as.numeric(cut(tmp[,"esc"], breaks = S_breaks)))
    
    # determine if each escapement would result in the desired R or H criteria
    tmp = cbind(
      tmp, 
      Rmeet = ifelse(tmp[,"recruits"] > (Rmax_p * max(tmp[,"recruits"])), 1, 0),
      Hmeet = ifelse(tmp[,"harv"] > (msy_p * max(tmp[,"harv"])), 1, 0)
    )
    
    Rtmp = tapply(tmp[,"Rmeet"], tmp[,"esc_bin"], function(x) as.numeric(all(x == 1)))
    Htmp = tapply(tmp[,"Hmeet"], tmp[,"esc_bin"], function(x) as.numeric(all(x == 1)))
    
    Rout[i,names(Rtmp)] = Rtmp
    Hout[i,names(Htmp)] = Htmp
  }
  if (!silent) cat("\n")
  
  pR = colMeans(Rout, na.rm = T)
  pH = colMeans(Hout, na.rm = T)
  
  goals = cbind(
    Rmax = range(which(pR >= Rmax_prob)) * 1000,
    msy = range(which(pH >= msy_prob)) * 1000); rownames(goals) = c("lower", "upper")
  
  probs = cbind(Rmax = pR, msy = pH); rownames(probs) = as.numeric(colnames(Rout)) * 1000
  list(
    goals = goals,
    probs = probs
  )
}
