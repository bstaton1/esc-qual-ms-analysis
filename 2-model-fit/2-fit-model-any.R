
# clear the workspace
rm(list = ls(all = T))

# settings
args = commandArgs(trailingOnly = T)
model = as.numeric(args[1])

# location of data files
data_dir = "inputs"

# compile the data
source("1-compile-data.R")

# load in additional functions
source("../new-func-source.R")

# starttime of everything
starttime_all = Sys.time()

# location of output files
out_dir = "../model-output/"

nchain =      4  # number of chains
parallel =    T  # run chains in parallel?
verbose =     T  # print JAGS messages to console?
silent =      F  # print post processing progress?
seed =        9  # seed for initial value and mcmc sampling
mcmc_vshort = F  # run with very short mcmc settings?
mcmc_lshort = F  # run with less short mcmc settings?
mcmc_medium = F  # run with medium mcmc settings?
mcmc_long =   T  # run with long mcmc settings?
calc_msy =    T  # calculate msy-based quantities?
save_files =  T  # save output?
rand_age =    F  # use dirichlet-distributed ages?

# make sure only one MCMC setting was specified
if (sum(c(mcmc_vshort, mcmc_lshort, mcmc_medium, mcmc_long)) != 1) {
  stop("you incorrectly specified how long to run the MCMC algorithm for")
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

# set mcmc per chain dimensions
if (mcmc_vshort)  {npost = 100; nburn = 500; nthin = 1; nadapt = 50}
if (mcmc_lshort)  {npost = 5000; nburn = 1000; nthin = 2 * nchain; nadapt = 1000}
if (mcmc_medium)  {npost = 50000; nburn = 20000; nthin = 40; nadapt = 10000}
if (mcmc_long)    {npost = 1000000; nburn = 100000; nthin = 200; nadapt = 10000}

# set nodes to monitor
jags_params = c(
  # SRA params
  "alpha", "beta", "beta_e10", "phi", "sigma_R_white", "sigma_R0",
  
  # states
  "N_t", "S_t", "Z_t", "R", "log_mean_R0", "Hcom", "Hsub",
  
  # demographic parameters
  "b0_sex", "b1_sex", "b0_mat", "b1_mat", "p", "mu_pi_f",
  "mu_pi_mat", "D_sum",
  
  # derived quantities
  "q_sub", "q_com", "q_esc", "q_run", 
  "Utot_tas", "Utot_ta", "Utot_ts", "Utot_t",
  "Z_per_S_t", "Z_per_female_t",
  
  # fishery/selectivity parameters
  "Fcom", "Fsub", "v", "Vtau", "Vsig", "Vtha", "Vlam",
  "Vtau_prior", "Vsig_prior", "Vtha_prior", "Vlam_prior",
  "Vtau_yukon", "Vsig_yukon", "Vtha_yukon", "Vlam_yukon",
)

# set nodes to monitor diagnostics for
diag_nodes = c("alpha", "beta", "beta_e10", "R", "b0_sex", 
               "b1_sex", "b0_mat", "b1_mat", "p",
               "D_sum", "phi", "sigma_R_white", "sigma_R0", 
               "Fcom", "Fsub", "Vtau", "Vsig", "Vtha", "Vlam", "log_mean_R0"
)

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
  age_trend = age_trend, 
  sex_trend = sex_trend,
  len_trend = len_trend,
  rand_age = rand_age
  )

# create initial values
set.seed(seed)
jags_inits = lapply(1:nchain, function(i) gen_inits(z_unit = z_unit, sex_trend = sex_trend, age_trend = age_trend))

## run the sampler
# read gelman et al. 2004 for more information about DIC
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
  DIC = TRUE,
  seed = seed
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

### process output: MSY calculations
if (calc_msy) {
  early_keep_t = 1:10; early_keep_y = 1:10
  late_keep_t = nt - 9:0; late_keep_y = ny - 9:0
  all_keep_t = 1:nt; all_keep_y = 1:ny

  cat("  |--- Extracting Samples for MSY Calculations ---|\n")
  starttime = Sys.time()
  cat("    Started at:", format(starttime), "\n")
  samps_early = prep_samples(post, keep_t = early_keep_t, keep_y = early_keep_y, len_trend = len_trend, silent = silent); cat("    Early period complete\n")
  samps_late = prep_samples(post, keep_t = late_keep_t, keep_y = late_keep_y, len_trend = len_trend, silent = silent); cat("    Late period complete\n")
  samps_all = prep_samples(post, keep_t = all_keep_t, keep_y = all_keep_y, len_trend = len_trend, silent = silent); cat("    All period complete\n")
  stoptime = Sys.time()
  cat("    Elapsed time:", format(round(stoptime - starttime, 1)), "\n")
}

if (calc_msy) {
  cat("  |--- Performing MSY Calculations ---|\n")
  starttime = Sys.time()
  cat("    Started at:", format(starttime), "\n")
  msy_early = msy_search(samps_early, silent = silent); cat("      Early period complete\n")
  msy_late = msy_search(samps_late, silent = silent); cat("      Late period complete\n")
  msy_all = msy_search(samps_all, silent = silent); cat("      All period complete\n")
  msy = abind::abind(msy_early, msy_all, msy_late, along = 4)
  dimnames(msy)[[4]] = c("early", "all", "late")
  stoptime = Sys.time()
  cat("    Elapsed time:", format(round(stoptime - starttime, 1)), "\n")
}

stoptime_after = Sys.time()

# save files
if (save_files) {
  cat("  |--- Saving Output Files ---|\n")
  if (calc_msy) saveRDS(msy, file.path(out_dir, msy_name))
  saveRDS(post, file.path(out_dir, post_name))
  saveRDS(
    append(
      extract_jags_metadata(post_info),
      list(
        model = model,
        z_unit = z_unit,
        len_trend = len_trend,
        sex_trend = sex_trend,
        age_trend = age_trend,
        rand_age = rand_age,
        R.hat = R.hat,
        n.eff = n.eff
      )), file.path(out_dir, meta_name))
  
}
stoptime_all = Sys.time()

cat("\n", "#-----------------------------------#", "\n")
cat("  Elapsed time (Total): ", format(round(stoptime_all - starttime_all, 1)), "\n")
cat("  Elapsed time (MCMC):  ", format(round(stoptime_mcmc - starttime_mcmc, 1)), "\n")
cat("  Elapsed time (Output):", format(round(stoptime_after - starttime_after, 1)), "\n")
cat(" #-----------------------------------#", "\n")
cat("\n  Model", model, "Done.\n")
