
get_PP = function(post.samp, vuln, silent = F) {
  
  # will calculate equilibrium yield, escapement, and recruitment at each F
  nU = 1000
  U_range = seq(0.0001, 0.9999, length = nU)
  F_range = log(1/(1 - U_range))
  
  # number of posterior samples
  n_samp = nrow(post.samp)
  
  # containers for output
  S_breaks = seq(0, 300000, 1000); nS_breaks = length(S_breaks)
  
  # get profiles for having recruits be greater than 90%, 80%, and 70% of Rmax
  outmat = matrix(0, n_samp, nS_breaks - 1); colnames(outmat) = 1:(nS_breaks - 1)
  Rout_0.9 = Rout_0.8 = Rout_0.7 = outmat

  # get profiles for having harvest be greater than 90%, 80%, and 70% of msy
  msyout_0.9 = msyout_0.8 = msyout_0.7 = outmat

  # get profiles for having harvest be greater than 100K, 90K, and 80K
  Hout_100 = Hout_90 = Hout_80 = outmat

  # loop through posterior samples, and determine whether desired criteria would be met at each U
  for (i in 1:n_samp) {
    if (!silent) cat("\r","      ", round(i/n_samp, 2) * 100, "%", sep = "")
    
    # calculate equilibrium H, S, and R at each F for this MCMC sample
    tmp = matrix(NA, nU, 3)
    for (f in 1:nU) {
      tmp[f,] = yield(
        log_F_max = log(F_range[f]), i = i, 
        post.samp = post.samp, 
        vuln = vuln
      )[c("H", "R", "S")]
    }
    colnames(tmp) = c("H", "R", "S")
    
    # assign a bin for the resulting escapement at each F  
    tmp = cbind(tmp, S_bin = as.numeric(cut(tmp[,"S"], breaks = S_breaks)))
    
    # determine if each fishing mortality would result in the desired R or H criteria
    tmp = cbind(
      tmp, 
      Rmeet_0.9 = ifelse(tmp[,"R"] > (0.9 * max(tmp[,"R"])), 1, 0),
      Rmeet_0.8 = ifelse(tmp[,"R"] > (0.8 * max(tmp[,"R"])), 1, 0),
      Rmeet_0.7 = ifelse(tmp[,"R"] > (0.7 * max(tmp[,"R"])), 1, 0),
      msymeet_0.9 = ifelse(tmp[,"H"] > (0.9 * max(tmp[,"H"])), 1, 0),
      msymeet_0.8 = ifelse(tmp[,"H"] > (0.8 * max(tmp[,"H"])), 1, 0),
      msymeet_0.7 = ifelse(tmp[,"H"] > (0.7 * max(tmp[,"H"])), 1, 0),
      Hmeet_100 = ifelse(tmp[,"H"] > 100000, 1, 0),
      Hmeet_90 = ifelse(tmp[,"H"] > 90000, 1, 0),
      Hmeet_80 = ifelse(tmp[,"H"] > 80000, 1, 0)
    )
    
    # convert to escapement levels: if all of the fishing mortality levels within an escapement bin would result in success, call it a success at that escapement
    Rtmp_0.9 = tapply(tmp[,"Rmeet_0.9"], tmp[,"S_bin"], function(x) as.numeric(all(x == 1)))
    Rtmp_0.8 = tapply(tmp[,"Rmeet_0.8"], tmp[,"S_bin"], function(x) as.numeric(all(x == 1)))
    Rtmp_0.7 = tapply(tmp[,"Rmeet_0.7"], tmp[,"S_bin"], function(x) as.numeric(all(x == 1)))
    msytmp_0.9 = tapply(tmp[,"msymeet_0.9"], tmp[,"S_bin"], function(x) as.numeric(all(x == 1)))
    msytmp_0.8 = tapply(tmp[,"msymeet_0.8"], tmp[,"S_bin"], function(x) as.numeric(all(x == 1)))
    msytmp_0.7 = tapply(tmp[,"msymeet_0.7"], tmp[,"S_bin"], function(x) as.numeric(all(x == 1)))
    Htmp_100 = tapply(tmp[,"Hmeet_100"], tmp[,"S_bin"], function(x) as.numeric(all(x == 1)))
    Htmp_90 = tapply(tmp[,"Hmeet_90"], tmp[,"S_bin"], function(x) as.numeric(all(x == 1)))
    Htmp_80 = tapply(tmp[,"Hmeet_80"], tmp[,"S_bin"], function(x) as.numeric(all(x == 1)))
    
    # save whether criteria were met for this iteration at each escapement level
    Rout_0.9[i,names(Rtmp_0.9)] = Rtmp_0.9
    Rout_0.8[i,names(Rtmp_0.8)] = Rtmp_0.8
    Rout_0.7[i,names(Rtmp_0.7)] = Rtmp_0.7
    msyout_0.9[i,names(msytmp_0.9)] = msytmp_0.9
    msyout_0.8[i,names(msytmp_0.8)] = msytmp_0.8
    msyout_0.7[i,names(msytmp_0.7)] = msytmp_0.7
    Hout_100[i,names(Htmp_100)] = Htmp_100
    Hout_90[i,names(Htmp_90)] = Htmp_90
    Hout_80[i,names(Htmp_80)] = Htmp_80
  }
  if (!silent) cat("\n")
  
  # calculate the probability of meeting criterion at each escapement level
  pR_0.9 = colMeans(Rout_0.9, na.rm = T)
  pR_0.8 = colMeans(Rout_0.8, na.rm = T)
  pR_0.7 = colMeans(Rout_0.7, na.rm = T)
  pmsy_0.9 = colMeans(msyout_0.9, na.rm = T)
  pmsy_0.8 = colMeans(msyout_0.8, na.rm = T)
  pmsy_0.7 = colMeans(msyout_0.7, na.rm = T)
  pH_100 = colMeans(Hout_100, na.rm = T)
  pH_90 = colMeans(Hout_90, na.rm = T)
  pH_80 = colMeans(Hout_80, na.rm = T)
  
  # combine into a matrix
  probs = cbind(
    Rmax_0.9 = pR_0.9, Rmax_0.8 = pR_0.8, Rmax_0.7 = pR_0.7, 
    MSY_0.9 = pmsy_0.9, MSY_0.8 = pmsy_0.8, MSY_0.7 = pmsy_0.7,
    H_100 = pH_100, H_90 = pH_90, H_80 = pH_80
    ); rownames(probs) = as.numeric(colnames(outmat)) * 1000
  
  # return
  probs
}


