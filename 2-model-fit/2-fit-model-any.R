# CRAN Packages: install these using the typical install.packages("pkgname")
suppressMessages(suppressWarnings(library(jagsUI)))
suppressMessages(suppressWarnings(library(abind)))

# Staton Packages: install these using devtools::install_github("bstaton1/pkgname")
suppressMessages(suppressWarnings(library(StatonMisc)))
suppressMessages(suppressWarnings(library(codaTools)))

# clear the workspace
rm(list = ls(all = T))

# compile the data
source("1-compile-data.R")

# load in additional functions
source("../new-func-source.R")

# settings
args = commandArgs(trailingOnly = T)
model = as.numeric(args[1])
# model = 1

nchain =      2  # number of chains
parallel =    T  # run chains in parallel?
verbose =     F  # print JAGS messages to console?
silent =      T  # print post processing progress?
seed =        1  # seed for initial value and mcmc sampling
mcmc_short =  T  # run with short mcmc settings?
mcmc_medium = F  # run with medium mcmc settings?
mcmc_long =   F  # run with long mcmc settings?
calc_msy =    T  # calculate msy-based quantities?
calc_EG =     T  # calculate escapement goal endpoints based on probability profiles?

# model assumptions
mod_key = read.csv("model-key.csv")
eggs = as.logical(mod_key$eggs[mod_key$model == model])
eggs_trend = as.logical(mod_key$eggs_trend[mod_key$model == model])
sex_trend = as.logical(mod_key$sex_trend[mod_key$model == model])
vuln_trend = as.logical(mod_key$vuln_trend[mod_key$model == model])
mat_trend = as.logical(mod_key$mat_trend[mod_key$model == model])

# names for output
if (!dir.exists("model-files")) dir.create("model-files")
model_file = file.path("model-files", paste("model-", model, ".txt", sep = ""))
out_dir = "../../model-output/"
if (!dir.exists(out_dir)) dir.create(out_dir)
post_name = paste("post-", model, ".rds", sep = "")
meta_name = paste("meta-", model, ".rds", sep = "")
msy_name = paste("msy-", model, ".rds", sep = "")
EG_name = paste("goals-", model, ".rds", sep = "")

# set mcmc per chain dimensions
if (mcmc_short)  {npost = 100; nburn = 500; nthin = 1; nadapt = 50}
if (mcmc_medium) {npost = 50000; nburn = 20000; nthin = 10 * nchain; nadapt = 10000}
if (mcmc_long)   {npost = 250000; nburn = 20000; nthin = 100 * nchain/4; nadapt = 10000}

# set nodes to monitor
jags_params = c(
  "alpha", "beta", "N_t", "S_t", "eggs_t", "S_tas", "R", "log_mean_R0", "R_sex", "Hcom", "Hsub",
  "b0_sex", "b1_sex", "b0_mat", "b1_mat", "p", "mu_pi_f",
  "mu_pi_mat", "q_sub", "q_com", "q_esc", "q_run", "D_sum",
  "phi", "sigma_R_white", "sigma_R0", "Fcom", "Fsub", "v", "Vtau", "Vsig", "Vtha", "Vlam",
  "Vtau_prior", "Vsig_prior", "Vtha_prior", "Vlam_prior",
  "Vtau_yukon", "Vsig_yukon", "Vtha_yukon", "Vlam_yukon",
  "Ucom_tas", "Usub_tas", "Utot_tas", "Utot_ta", "Utot_t", "Utot_ts", "eggs_per_S_t")

# set nodes to monitor diagnostics for
diag_nodes = c("alpha", "beta", "R", "b0_sex", 
               "b1_sex", "b0_mat", "b1_mat", "p",
               "D_sum", "phi", "sigma_R_white", "sigma_R0", 
               "Fcom", "Fsub", "Vtau", "Vsig", "Vtha", "Vlam", "log_mean_R0"
)

## write the model file
# dput(jags_model_code, file = "model-files/full-model.txt", control = "all")
edit_full_model(
  model_lines = readLines(file.path("model-files", "full-model.txt")),
  outfile = model_file,
  eggs = eggs, eggs_trend = eggs_trend, sex_trend = sex_trend,
  vuln_trend = vuln_trend, mat_trend = mat_trend
  )

## create initial values
set.seed(seed)
jags_inits = list()
for (c in 1:nchain) {
  jags_inits[[c]] = gen_inits(eggs = eggs, sex_trend = sex_trend, mat_trend = mat_trend)
}

## run the sampler
# read gelman et al. 2004 for more information about DIC
starttime = Sys.time()
cat("\n  |--- Running JAGS on Model ", model, " ---|\n\n", sep = "")

cat("    #-------------------------------#\n")
cat("    Model Assumptions:\n")
cat("      -> Spawing units: ", ifelse(eggs, "Eggs", "Fish"), "\n", sep = "")
cat("      -> Fecundity trend: ", ifelse(eggs_trend, "Yes", "No"), "\n", sep = "")
cat("      -> Sex trend: ", ifelse(sex_trend, "Yes", "No"), "\n", sep = "")
cat("      -> Maturity trend: ", ifelse(mat_trend, "Yes", "No"), "\n", sep = "")
cat("      -> Vulnerability trend: ", ifelse(vuln_trend, "Yes", "No"), "\n", sep = "")
cat("    #-------------------------------#\n")
cat("    MCMC Settings:\n")
cat("      -> Parallel processing: ", ifelse(parallel, "Yes", "No"), "\n", sep = "")
cat("      -> Adapt: ", prettify(nadapt), "\n", sep = "")
cat("      -> Burn-in: ", prettify(nburn), "\n", sep = "")
cat("      -> Post Burn-in: ", prettify(npost), "\n", sep = "")
cat("      -> Thinning Interval: ", prettify(nthin), "\n", sep = "")
cat("      -> Chains: ", prettify(nchain), "\n", sep = "")
cat("      -> Saved: ", prettify((npost/nthin) * nchain), "\n", sep = "")
cat("    #-------------------------------#\n")

cat("\n    Started MCMC at:", format(starttime), "\n")

post = jags(
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
stoptime = Sys.time()
cat("    Completed MCMC at:", format(stoptime), "\n")
cat("    Elapsed MCMC time:", format(round(stoptime - starttime, 2)), "\n")

# summarize diagnostics
diag_summ = function(x) {
  c(mean = mean(x, na.rm = T),
    min = min(x, na.rm = T),
    quantile(x, c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975), na.rm = T),
    max = max(x, na.rm = T))
}

R.hat = t(as.data.frame(lapply(post$Rhat[diag_nodes], diag_summ)))
n.eff = t(as.data.frame(lapply(post$n.eff[diag_nodes], diag_summ)))

cat("  Summarized R.hat values:\n")
round(R.hat, 2)
cat("\n  Summarized n.eff values:\n")
round(n.eff)
cat("\n")

### process output: MSY calculations and EG derivation
if (calc_EG | calc_msy) {
  early_keep_t = 1:10; early_keep_y = 1:10
  late_keep_t = nt - 9:0; late_keep_y = ny - 9:0
  all_keep_t = 1:nt; all_keep_y = 1:ny
  
  early_keep_t = 1:10; early_keep_y = 1:10
  late_keep_t = nt - 9:0; late_keep_y = ny - 9:0
  all_keep_t = 1:nt; all_keep_y = 1:ny
  
  cat("  |--- Extracting Samples for MSY Calculations ---|\n")
  starttime = Sys.time()
  cat("    Started at:", format(starttime), "\n")
  samps_early = prep_samples(post$samples, keep_t = early_keep_t, keep_y = early_keep_y, eggs_trend = T, silent = silent); cat("      Early period complete\n")
  samps_late = prep_samples(post$samples, keep_t = late_keep_t, keep_y = late_keep_y, eggs_trend = T, silent = silent); cat("      Late period complete\n")
  samps_all = prep_samples(post$samples, keep_t = all_keep_t, keep_y = all_keep_y, eggs_trend = T, silent = silent); cat("      All period complete\n")
  stoptime = Sys.time()
  cat("    Elapsed time:", format(round(stoptime - starttime, 1)), "\n")
}

if (calc_msy) {
  cat("  |--- Performing MSY Calculations ---|\n")
  starttime = Sys.time()
  cat("    Started at:", format(starttime), "\n")
  msy_early = msy_search(samps_early, spawn_units = ifelse(eggs, "eggs", "fish"), silent = silent); cat("      Early period complete\n")
  msy_late = msy_search(samps_late, spawn_units = ifelse(eggs, "eggs", "fish"), silent = silent); cat("      Late period complete\n")
  msy_all = msy_search(samps_all, spawn_units = ifelse(eggs, "eggs", "fish"), silent = silent); cat("      All period complete\n")
  msy = abind::abind(msy_early, msy_all, msy_late, along = 4)
  dimnames(msy)[[4]] = c("early", "all", "late")
  stoptime = Sys.time()
  cat("    Elapsed time:", format(round(stoptime - starttime, 1)), "\n")
}

if (calc_EG) {
  cat("  |--- Estimating Escapement Goals ---|\n")
  starttime = Sys.time()
  cat("    Started at:", format(starttime), "\n")
  EG_unr_early = get_EG(post.samp = samps_early, spawn_units = ifelse(eggs, "eggs", "fish"), vuln = "unr", silent = silent); cat("      Early (UNR) period complete\n")
  EG_unr_late = get_EG(post.samp = samps_late, spawn_units = ifelse(eggs, "eggs", "fish"), vuln = "unr", silent = silent); cat("      Late (UNR) period complete\n")
  EG_unr_all = get_EG(post.samp = samps_all, spawn_units = ifelse(eggs, "eggs", "fish"), vuln = "unr", silent = silent); cat("      All (UNR) period complete\n")
  EG_res_early = get_EG(post.samp = samps_early, spawn_units = ifelse(eggs, "eggs", "fish"), vuln = "res", silent = silent); cat("      Early (RES) period complete\n")
  EG_res_late= get_EG(post.samp = samps_late, spawn_units = ifelse(eggs, "eggs", "fish"), vuln = "res", silent = silent); cat("      Late (RES) period complete\n")
  EG_res_all = get_EG(post.samp = samps_all, spawn_units = ifelse(eggs, "eggs", "fish"), vuln = "res", silent = silent); cat("      All (RES) period complete\n")
  
  EG_early = abind(EG_unr_early$goals, EG_res_early$goals, along = 3)
  EG_late = abind(EG_unr_late$goals, EG_res_late$goals, along = 3)
  EG_all = abind(EG_unr_all$goals, EG_res_all$goals, along = 3)
  EG = abind(EG_early, EG_all, EG_late, along = 4)
  dimnames(EG)[[3]] = c("unr", "res")
  dimnames(EG)[[4]] = c("early", "all", "late")
  
  probs_early = abind(EG_unr_early$probs, EG_res_early$probs, along = 3)
  probs_late = abind(EG_unr_late$probs, EG_res_late$probs, along = 3)
  probs_all = abind(EG_unr_all$probs, EG_res_all$probs, along = 3)
  probs = abind(probs_early, probs_all, probs_late, along = 4)
  dimnames(probs)[[3]] = c("unr", "res")
  dimnames(probs)[[4]] = c("early", "all", "late")
  
  EGinfo = list(EG = EG, probs = probs)
  
  stoptime = Sys.time()
  cat("    Elapsed time:", format(round(stoptime - starttime, 1)), "\n")
}

# save files
cat("  |--- Saving Output Files ---|\n")
if (calc_msy) saveRDS(msy, file.path(out_dir, msy_name))
if (calc_EG) saveRDS(EGinfo, file.path(out_dir, EG_name))
saveRDS(post$samples, file.path(out_dir, post_name))
saveRDS(
  append(
    extract_jags_metadata(post),
    list(
      model = model,
      eggs = eggs,
      eggs_trend = eggs_trend,
      sex_trend = sex_trend,
      mat_trend = mat_trend,
      vuln_trend = vuln_trend,
      R.hat = R.hat,
      n.eff = n.eff
    )), file.path(out_dir, meta_name))

cat("\n  Model", model, "Done.\n")
