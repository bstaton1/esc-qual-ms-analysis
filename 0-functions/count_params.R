
count_params_in_model = function(age_trend, sex_trend) {
  # packages
  require(magrittr)
  require(stringr)
  
  # create model code
  edit_full_model(
    model_lines = readLines(file.path("2-model-fit", "model-files", "full-model.txt")),
    outfile = "tmp.txt",
    z_unit = "fish_count",
    age_trend = F, 
    sex_trend = F,
    rand_age = F,
    ppd = F
  )
  
  # isolate the nodes that are stochastic
  stoch = readLines("tmp.txt") %>%
    .[str_detect(., "~")] %>%
    str_remove_all("[:space:]+") %>%
    str_extract("^.+~") %>%
    str_remove("~")
  
  # delete the temporary file
  unlink("tmp.txt")
  
  # drop out the observed stochastic nodes
  stoch = stoch[-which(stoch %in% c("Hcom_obs[t]", "Hsub_obs[t]", "S_obs[t]", "x_esc[t,1:(2*na)]", "x_com[t,1:(2*na)]", "x_sub[t,1:(2*na)]"))]
  
  # log_R is found twice - drop this
  stoch = unique(stoch)
  
  # build an identifier key: includes params for the full model
  key = data.frame(
    param = c("alpha", "beta", "phi", "tau_R_white", "log_resid_0", "log_mean_R0", "tau_R0",
              "log_R[y]", "delta_0", "delta_1", "gamma_0[s,a]", "gamma_1[s,a]", 
              "Vtau", "Vsig", "Vtha", "Vlam", "Fsub[t]", "Fcom[t]"),
    n_nodes = c(1, 1, 1, 1, 1, 1, 1, ny, 1, 1, (na - 1) * 2, (na - 1) * 2, 1, 1, 1, 1, nt, nt),
    group = c(rep("Spawner-Recruit", 7), "Latent R States", "Sex-at-Return", "Sex-at-Return", "Age-at-Return", "Age-at-Return",
              rep("Selectivity", 4), "Fishing Mortality", "Fishing Mortality")
  )
  key$group = factor(key$group, levels = c("Spawner-Recruit", "Latent R States", "Age-at-Return", "Sex-at-Return", "Selectivity", "Fishing Mortality"))
  
  # keep only params found in the model code
  key_use = key[key$param %in% stoch,]
  
  # calculate summaries by group
  counts = tapply(key_use$n_nodes, key_use$group, sum)
  
  # calculate totals
  counts = c(counts, Total = sum(counts))
  
  # return
  counts
}
