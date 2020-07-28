
# clear the workspace
rm(list = ls(all = T))

# settings
args = commandArgs(trailingOnly = T)
model = as.numeric(args[1])
model = 10

# location of data files
data_dir = "inputs"

# compile the data
source("1-compile-data.R")

# load in additional functions
source("../load-functions.R")

# starttime of everything
starttime_all = Sys.time()

# location of output files
out_dir = "../model-output/"

nchain =      4  # number of chains
parallel =    T  # run chains in parallel?
verbose =     T  # print JAGS messages to console?
silent =      F  # print post processing progress?
seed =        9  # seed for initial value and mcmc sampling
mcmc_vshort = T  # run with very short mcmc settings?
mcmc_lshort = F  # run with less short mcmc settings?
mcmc_medium = F  # run with medium mcmc settings?
mcmc_long =   F  # run with long mcmc settings?
calc_eq =     T  # calculate equilibrium quantities (based on fishing mortialities that provide msy and Rmax)?
save_files =  T  # save output?
rand_age =    F  # use dirichlet-distributed ages?
Vprior = "kusko" # which data set to use as priors for selectivity parameters ('yukon' or 'kusko' - doesn't make a difference b/c SE multiplied by 10)?

# make sure only one MCMC setting was specified
if (sum(c(mcmc_vshort, mcmc_lshort, mcmc_medium, mcmc_long)) != 1) {
  stop("you incorrectly specified how long to run the MCMC algorithm for")
}

# make sure a correct Vprior was specified
if (!(Vprior %in% c("yukon", "kusko"))) {
  stop("Vprior must be one of 'yukon' or 'kusko'")
}

# which time effects are included?
len_trend = as.logical(mod_key$length_trend[mod_key$model == model])
sex_trend = as.logical(mod_key$sex_trend[mod_key$model == model])
age_trend = as.logical(mod_key$age_trend[mod_key$model == model])

# names for output
if (!dir.exists("model-files")) dir.create("model-files")
model_file = file.path("model-files", paste("model-", model, ".txt", sep = ""))
if (!dir.exists(out_dir) & save_files) dir.create(out_dir)
post_name = paste("post-", model, ".rds", sep = "")
meta_name = paste("meta-", model, ".rds", sep = "")
msy_name = paste("msy-", model, ".rds", sep = "")
Rmax_name = paste("Rmax-", model, ".rds", sep = "")

# set mcmc per chain dimensions
if (mcmc_vshort)  {npost = 100; nburn = 500; nthin = 1; nadapt = 50}
if (mcmc_lshort)  {npost = 5000; nburn = 1000; nthin = 2 * nchain; nadapt = 1000}
if (mcmc_medium)  {npost = 50000; nburn = 20000; nthin = 40; nadapt = 10000}
if (mcmc_long)    {npost = 1000000; nburn = 100000; nthin = 200; nadapt = 10000}

# set the selectivity priors
jags_dat = append(jags_dat, list(
  Vtau_prior = {if (Vprior == "kusko") Vtau_kusko else Vtau_yukon},
  Vsig_prior = {if (Vprior == "kusko") Vsig_kusko else Vsig_yukon},
  Vtha_prior = {if (Vprior == "kusko") Vtha_kusko else Vtha_yukon},
  Vlam_prior = {if (Vprior == "kusko") Vlam_kusko else Vlam_yukon}
))

# set nodes to monitor
jags_params = c(
  # SRA params
  "alpha", "beta", "beta_e10", "phi", "sigma_R_white", "sigma_R0",
  
  # states
  "N_t", "S_t", "Z_t", "R", "log_mean_R0", "Hcom", "Hsub",
  
  # demographic parameters
  "b0_sex", "b1_sex", "b0_mat", "b1_mat", "p", "mu_pi_f",
  "mu_pi_mat",
  
  # derived quantities
  "q_sub", "q_com", "q_esc", "q_run", 
  "Utot_tas", "Utot_ta", "Utot_ts", "Utot_t",
  "Z_per_S_t", "Z_per_female_t",
  
  # fishery/selectivity parameters
  "Fcom", "Fsub", "v", "Vtau", "Vsig", "Vtha", "Vlam"
)
if (rand_age) jags_params = c(jags_params, "D_sum")

# set nodes to monitor diagnostics for
diag_nodes = c("alpha", "beta", "beta_e10", "R", "b0_sex", 
               "b1_sex", "b0_mat", "b1_mat", "p",
               "phi", "sigma_R_white", "sigma_R0", 
               "Fcom", "Fsub", "Vtau", "Vsig", "Vtha", "Vlam", "log_mean_R0"
)
if (rand_age) diag_nodes = c(jags_params, "D_sum")

## write the model file
# the full model - this one gets simplified based on the specific trend assumptions
# NOTE: when executed from the command line, this writes out a file
# with the correct contents, but with improper formatting. I have no idea why dput works differently in these two cases
# This causes edit_full_model() to bomb. Thus, you need to write out the full model 
# BEFORE calling this script via Rscript
# write_full_model()

# stringr::str_magic!!
edit_full_model(
  model_lines = readLines(file.path("model-files", "full-model.txt")),
  outfile = model_file,
  z_unit = z_unit,
  age_trend = age_trend, 
  sex_trend = sex_trend,
  rand_age = rand_age
  )

# create initial values
set.seed(seed)
jags_inits = lapply(1:nchain, function(i) gen_inits(z_unit = z_unit, sex_trend = sex_trend, age_trend = age_trend, rand_age = rand_age))

## run the sampler
starttime_mcmc = Sys.time()

print_start_mcmc_message()

cat("\n    Started MCMC at:", format(starttime_mcmc), "\n")

post_info = jagsUI::jags(
  data = jags_dat,
  inits = jags_inits,
  parameters.to.save = jags_params,
  model.file = model_file,
  n.chains = nchain,
  n.adapt = nadapt,
  n.iter = npost + nburn,
  n.burnin = nburn,
  n.thin = nthin,
  parallel = parallel,
  verbose = verbose,
  DIC = TRUE
)
post = post_info$samples
stoptime_mcmc = Sys.time()
cat("    Completed MCMC at:", format(stoptime_mcmc), "\n")
cat("    Elapsed MCMC time:", format(round(stoptime_mcmc - starttime_mcmc, 2)), "\n")

# summarize diagnostics
diag_summ = function(x) {
  c(mean = mean(x, na.rm = T),
    min = min(x, na.rm = T),
    quantile(x, c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975), na.rm = T),
    max = max(x, na.rm = T))
}

R.hat = suppressWarnings(t(as.data.frame(lapply(post_info$Rhat[diag_nodes], diag_summ))))
n.eff = suppressWarnings(t(as.data.frame(lapply(post_info$n.eff[diag_nodes], diag_summ))))

cat("  Summarized R.hat values:\n")
round(R.hat, 2)
cat("\n  Summarized n.eff values:\n")
round(n.eff)
cat("\n")

starttime_after = Sys.time()

### process output: equilibrium calculations
if (calc_eq) {
  early_keep_t = 1:10; early_keep_y = 1:10
  late_keep_t = nt - 9:0; late_keep_y = ny - 9:0
  all_keep_t = 1:nt; all_keep_y = 1:ny

  cat("  |--- Extracting Samples for Equilibrium Calculations ---|\n")
  starttime = Sys.time()
  cat("    Started at:", format(starttime), "\n")
  samps_early = prep_samples(post, keep_t = early_keep_t, keep_y = early_keep_y, silent = silent)
  samps_late = prep_samples(post, keep_t = late_keep_t, keep_y = late_keep_y, silent = silent)
  samps_all = prep_samples(post, keep_t = all_keep_t, keep_y = all_keep_y, silent = silent)
  stoptime = Sys.time()
  cat("    Elapsed time:", format(round(stoptime - starttime, 1)), "\n")
}

if (calc_eq) {
  cat("  |--- Performing MSY Calculations ---|\n")
  starttime = Sys.time()
  cat("    Started at:", format(starttime), "\n")
  msy_early = eq_search(samps_early, q = "H", silent = silent)
  msy_late = eq_search(samps_late, q = "H", silent = silent)
  msy_all = eq_search(samps_all, q = "H", silent = silent)
  msy = abind::abind(msy_early, msy_all, msy_late, along = 4)
  dimnames(msy)[[4]] = c("early", "all", "late")
  stoptime = Sys.time()
  cat("    Elapsed time:", format(round(stoptime - starttime, 1)), "\n")
  
  cat("  |--- Performing Rmax Calculations ---|\n")
  starttime = Sys.time()
  cat("    Started at:", format(starttime), "\n")
  Rmax_early = eq_search(samps_early, q = "R", silent = silent)
  Rmax_late = eq_search(samps_late, q = "R", silent = silent)
  Rmax_all = eq_search(samps_all, q = "R", silent = silent)
  Rmax = abind::abind(Rmax_early, Rmax_all, Rmax_late, along = 4)
  dimnames(Rmax)[[4]] = c("early", "all", "late")
  stoptime = Sys.time()
  cat("    Elapsed time:", format(round(stoptime - starttime, 1)), "\n")
  
}

stoptime_after = Sys.time()

# save files
if (save_files) {
  cat("  |--- Saving Output Files ---|\n")
  if (calc_eq) {
    saveRDS(msy, file.path(out_dir, msy_name))
    saveRDS(Rmax, file.path(out_dir, Rmax_name))
  } 
  saveRDS(post, file.path(out_dir, post_name))
  saveRDS(
    append(
      extract_jags_metadata(post_info),
      list(
        model = model,
        z_unit = z_unit,
        z_src = mod_key[mod_key$model == model,"src_abbrev"],
        esc_n_eff = mod_key[mod_key$model == model,"esc_n_eff"],
        com_n_eff = mod_key[mod_key$model == model,"com_n_eff"],
        sub_n_eff = mod_key[mod_key$model == model,"sub_n_eff"],
        len_trend = len_trend,
        sex_trend = sex_trend,
        age_trend = age_trend,
        rand_age = rand_age,
        R.hat = R.hat,
        n.eff = n.eff,
        Vprior = Vprior
      )), file.path(out_dir, meta_name))
  
}
stoptime_all = Sys.time()

cat("\n", "#-----------------------------------#", "\n")
cat("  Elapsed time (Total): ", format(round(stoptime_all - starttime_all, 1)), "\n")
cat("  Elapsed time (MCMC):  ", format(round(stoptime_mcmc - starttime_mcmc, 1)), "\n")
cat("  Elapsed time (Output):", format(round(stoptime_after - starttime_after, 1)), "\n")
cat(" #-----------------------------------#", "\n")
cat("\n  Model", model, "Done.\n")
