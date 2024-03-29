jags_model_code = function() {
  
  # Priors for SR portion
  alpha ~ dunif(0, 20)
  log_alpha <- log(alpha)
  beta ~ dunif(0, 0.5)
  beta_e10 <- beta * 1e10
  phi ~ dunif(-1, 0.99)
  tau_R ~ dgamma(0.01, 0.01)
  sigma_R <- 1 / sqrt(tau_R)
  log_resid_0 ~ dnorm(0, tau_R * (1 - phi * phi))
  
  ### PART 1: BIOLOGICAL PROCESS SUBMODEL: BROOD YEAR RECRUITMENT PROCESS #####
  
  #1a) Brood year returns without SR link; drawn from a common lognormal dist
  log_mean_R0 ~ dnorm(0,0.0001)#%_%I(0,)        
  tau_R0 ~ dgamma(0.01,0.01)      
  sigma_R0 <- 1 / sqrt(tau_R0)
  for(y in 1:a_max){ 
    log_R[y] ~ dnorm(log_mean_R0, tau_R0)   
    R[y] <- exp(log_R[y])
    log_resid[y] <- log_R[y] - log_mean_R0
  }
  
  #1b) Stock-Recruitment with Autocorrelated lag-1 residuals: for years with spawner/recruit link
  for (y in (na+a_min):ny) {
    log_R[y] ~ dnorm(log_Rmean2[y],tau_R)
    R[y] <- exp(log_R[y])
    log_Rmean1[y] <- log_alpha + log(Z_t[y-a_max]) - beta * Z_t[y-a_max]
    log_resid[y] <- log_R[y] - log_Rmean1[y]
  }
  log_Rmean2[na+a_min] <- log_Rmean1[na+a_min] + phi * log_resid_0
  for (y in (na+a_min+1):ny) {
    log_Rmean2[y] <- log_Rmean1[y] + phi * log_resid[y-1]
  }
  
  ### PART 2: MATURITY PROCESS SUBMODEL ###
  # 2a) brood-year specific sex ratio
  delta_0 ~ dnorm(0,1e-6)
  delta_1 ~ dnorm(0,1e-6)
  for (y in 1:ny) {
    logit(psi[y]) <- delta_0 + delta_1 * y
    R_sex[y,1] <- R[y] * psi[y]
    R_sex[y,2] <- R[y] * (1 - psi[y])
  }
  
  # 2b) brood year-specific return-at-age schedules by sex, with separate estimated time trends for each sex
  D_scale ~ dunif(0.01,1)
  D_sum <- 1/D_scale^2
  for (s in 1:2) {
    
    # set coefs for last age
    gamma_0[s,na] <- 0
    gamma_1[s,na] <- 0
    
    # estimate coefficients for the rest of the ages
    for (a in 1:(na-1)) {
      gamma_0[s,a] ~ dnorm(0,1e-6)
      gamma_1[s,a] ~ dnorm(0,1e-6)
    }
    
    # create brood year-specific return-at-age/sex vectors
    for (y in 1:ny) {
      for (a in 1:na) {
        eta_mat[y,a,s] <- exp(gamma_0[s,a] + gamma_1[s,a] * y)
        pi[y,a,s] <- eta_mat[y,a,s] / sum(eta_mat[y,1:na,s])
        gamma[y,a,s] <- D_sum * pi[y,a,s]
        g[y,a,s] ~ dgamma(gamma[y,a,s],0.1)
        p[y,a,s] <- g[y,a,s]/sum(g[y,1:na,s])
      }
    }
  }
  
  ##### PART 3: HARVEST PROCESS MODEL #####
  # 3a) Apportion sex-specific recruits to each age and calendar year
  for (t in 1:nt) {
    for (a in 1:na) {
      for (s in 1:2) {
        N_tas[t,a,s] <- R_sex[t+na-a,s] * p[t+na-a,a,s]
      }
      N_ta[t,a] <- sum(N_tas[t,a,1:2])
    }
    N_t[t] <- sum(N_ta[t,1:na])
  }
  
  # 3b) Create age/sex/year/mesh-specific selectivity schedule
  Vtau ~ dnorm(Vtau_prior[1], 1/(Vtau_prior[2] * 10)^2)
  Vsig ~ dnorm(Vsig_prior[1], 1/(Vsig_prior[2] * 10)^2) %_% I(0,)
  Vtha ~ dnorm(Vtha_prior[1], 1/(Vtha_prior[2] * 10)^2) %_% I(0,)
  Vlam ~ dnorm(Vlam_prior[1], 1/(Vlam_prior[2] * 10)^2)
  
  for (t in 1:nt) {
    for (s in 1:2) {
      for (m in 1:2) {
        for (a in 1:na) {
          # the pearson model
          v_term_1[t,a,s,m] <- (1 + Vlam^2/(4 * Vtha^2))^Vtha
          v_term_2[t,a,s,m] <- rlm[t,a,s,m] - (Vsig * Vlam)/(2 * Vtha) - Vtau
          v_term_3[t,a,s,m] <- (1 + v_term_2[t,a,s,m]^2/Vsig^2)^-Vtha
          v_term_4[t,a,s,m] <- exp(-Vlam * (atan(v_term_2[t,a,s,m]/Vsig) + atan(Vlam/(2 * Vtha))))
          v_raw[t,a,s,m] <- v_term_1[t,a,s,m] * v_term_3[t,a,s,m] * v_term_4[t,a,s,m]
          
          # standardize within year and mesh so one age/sex combo is fully selected
          v[t,a,s,m] <- v_raw[t,a,s,m]/max(v_raw[t,1:na,1:2,m])
        }
      }
    }
  }
  
  # 3c: calculate harvest by age, sex, year, and fishery
  for (t in 1:nt) {
    Fsub[t] ~ dunif(0,10)  # instantaneous subsistence fishing mortality of fully-selected age/sex by year
    Fcom[t] ~ dunif(0,10)  # instantaneous commercial fishing mortality of fully-selected age/sex by year
    for (a in 1:na) {
      for (s in 1:2) {
        # fishing mortality by year/age/sex/fishery
        Fcom_tas[t,a,s] <- Fcom[t] * v[t,a,s,com_mesh[t]]
        Fsub_tas[t,a,s] <- Fsub[t] * v[t,a,s,sub_mesh[t]]
        
        # total fishing mortality by year/age/sex
        Ftot_tas[t,a,s] <- Fcom_tas[t,a,s] + Fsub_tas[t,a,s]
        
        # harvest by year/age/sex/fishery
        Hsub_tas[t,a,s] <- N_tas[t,a,s] * (Fsub_tas[t,a,s]/Ftot_tas[t,a,s]) * (1 - exp(-Ftot_tas[t,a,s]))
        Hcom_tas[t,a,s] <- N_tas[t,a,s] * (Fcom_tas[t,a,s]/Ftot_tas[t,a,s]) * (1 - exp(-Ftot_tas[t,a,s]))
        
        # escapement by year/age/sex
        S_tas[t,a,s] <- N_tas[t,a,s] - Hsub_tas[t,a,s] - Hcom_tas[t,a,s]
        
        # exploitation rate by year/age/sex
        Utot_tas[t,a,s] <- (N_tas[t,a,s] - S_tas[t,a,s])/N_tas[t,a,s]
        
        # total reproductive output by year/age/sex
        Z_tas[t,a,s] <- S_tas[t,a,s] * z[t,a,s]
      }
      # harvest by fishery, escapement, and exploitation rate by year/age
      Hsub_ta[t,a] <- sum(Hsub_tas[t,a,1:2])
      Hcom_ta[t,a] <- sum(Hcom_tas[t,a,1:2])
      S_ta[t,a] <- sum(S_tas[t,a,1:2])
      Utot_ta[t,a] <- (N_ta[t,a] - S_ta[t,a])/N_ta[t,a]
    }
    # harvest by fishery, escapement, and exploitation rate by year
    Hsub[t] <- sum(Hsub_ta[t,1:na])
    Hcom[t] <- sum(Hcom_ta[t,1:na])
    S_t[t] <- sum(S_ta[t,1:na])
    Utot_t[t] <- (N_t[t] - S_t[t])/N_t[t]
    
    # reproductive units, summarized in different useful ways
    Z_t[t] <- sum(Z_tas[t,1:na,1:2])
    Z_per_S_t[t] <- Z_t[t]/S_t[t]
    Z_per_female_t[t] <- Z_t[t]/sum(S_tas[t,1:na,1])
    
    # exploitation rate by year/sex
    for (s in 1:2) {
      Utot_ts[t,s] <- (sum(N_tas[t,1:na,s]) - sum(S_tas[t,1:na,s]))/sum(N_tas[t,1:na,s])
    }
  }
  
  ### PART 4: OBSERVATION SUB MODEL ###
  for (t in 1:nt) {
    # observe aggregate abundance fates
    S_obs[t] ~ dlnorm(log(S_t[t]), 1/S_obs_sig[t]^2)
    Hcom_obs[t] ~ dlnorm(log(Hcom[t]), 1/Hcom_obs_sig[t]^2)
    Hsub_obs[t] ~ dlnorm(log(Hsub[t]), 1/Hsub_obs_sig[t]^2)
    
    # turn age/sex/year structured 3-d arrays into 2-d
    # [,1:na] is females (s == 1); [,(na+1):(2*na)] is males
    # allows fitting multinomial where each age/sex is a category
    
    N_tas_2d[t,1:na] <- N_tas[t,1:na,1]
    N_tas_2d[t,(na+1):(2*na)] <- N_tas[t,1:na,2]
    
    S_tas_2d[t,1:na] <- S_tas[t,1:na,1]
    S_tas_2d[t,(na+1):(2*na)] <- S_tas[t,1:na,2]
    
    Hcom_tas_2d[t,1:na] <- Hcom_tas[t,1:na,1]
    Hcom_tas_2d[t,(na+1):(2*na)] <- Hcom_tas[t,1:na,2]
    
    Hsub_tas_2d[t,1:na] <- Hsub_tas[t,1:na,1]
    Hsub_tas_2d[t,(na+1):(2*na)] <- Hsub_tas[t,1:na,2]
    
    for (as in 1:(na*2)) {
      q_esc[t,as] <- S_tas_2d[t,as]/S_t[t]
      q_com[t,as] <- Hcom_tas_2d[t,as]/Hcom[t]
      q_sub[t,as] <- Hsub_tas_2d[t,as]/Hsub[t]
      q_run[t,as] <- N_tas_2d[t,as]/N_t[t]
    }
    
    # observe composition vectors by fate
    x_esc[t,1:(2*na)] ~ dmulti(q_esc[t,1:(2*na)], n_esc[t])
    x_com[t,1:(2*na)] ~ dmulti(q_com[t,1:(2*na)], n_com[t])
    x_sub[t,1:(2*na)] ~ dmulti(q_sub[t,1:(2*na)], n_sub[t])
    
    ### ppd calculations (for WAIC) ###
    # lppd: aggregate abundance state observations
    lppd_S[t] <- logdensity.lnorm(S_obs[t], log(S_t[t]), 1/S_obs_sig[t]^2)
    lppd_Hcom[t] <- logdensity.lnorm(Hcom_obs[t], log(Hcom[t]), 1/Hcom_obs_sig[t]^2)
    lppd_Hsub[t] <- logdensity.lnorm(Hsub_obs[t], log(Hsub[t]), 1/Hsub_obs_sig[t]^2)
    
    # lppd: composition observations
    lppd_x_esc[t] <- logdensity.multi(x_esc[t,1:(2*na)], q_esc[t,1:(2*na)], n_esc[t])
    lppd_x_com[t] <- logdensity.multi(x_com[t,1:(2*na)], q_com[t,1:(2*na)], n_com[t])
    lppd_x_sub[t] <- logdensity.multi(x_sub[t,1:(2*na)], q_sub[t,1:(2*na)], n_sub[t])
    
    # ppd: totaled across obs types; assumes all data sources are independent - already assumed in fitting model
    ppd_total[t] <- exp(lppd_S[t] + lppd_Hcom[t] + lppd_Hsub[t] + lppd_x_esc[t] + lppd_x_com[t] + lppd_x_sub[t])
  }
}
