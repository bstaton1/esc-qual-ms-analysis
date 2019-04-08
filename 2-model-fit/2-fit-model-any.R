# CRAN Packages: install these using the typical install.packages("pkgname")
suppressMessages(suppressWarnings(library(jagsUI)))
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

beep = F
nchain = 2
parallel = T
verbose = F
seed = 1
silent = T
mcmc_short = T
mcmc_medium = F
mcmc_long = F

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
post_name = paste("testpost-", model, ".rds", sep = "")
meta_name = paste("testmeta-", model, ".rds", sep = "")
msy_name = paste("testmsy-", model, ".rds", sep = "")

# set mcmc per chain dimensions
if (mcmc_short)  {npost = 100; nburn = 50; nthin = 1; nadapt = 50}
if (mcmc_medium) {npost = 50000; nburn = 20000; nthin = 10 * nchain; nadapt = 10000}
if (mcmc_long)   {npost = 500000; nburn = 50000; nthin = 100 * nchain/2; nadapt = 10000}

# set nodes to monitor
jags_params = c(
  "alpha", "beta", "N_t", "S_t", "eggs_t", "S_tas", "R", "R_sex", "Hcom", "Hsub",
  "b0_sex", "b1_sex", "b0_mat", "b1_mat", "p", "mu_pi_f",
  "mu_pi_mat", "q_sub", "q_com", "q_esc", "q_run", "D_sum",
  "phi", "sigma_R_white", "sigma_R0", "Fcom", "Fsub", "v", "Vtau", "Vsig", "Vtha", "Vlam",
  "Vtau_prior", "Vsig_prior", "Vtha_prior", "Vlam_prior",
  "Vtau_yukon", "Vsig_yukon", "Vtha_yukon", "Vlam_yukon",
  "Ucom_tas", "Usub_tas", "Utot_tas", "Utot_ta", "Utot_t", "Utot_ts", "eggs_per_S_t")

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
cat("\n  |--- Running JAGS on Model ", model, " ---|\n", sep = "")

cat("    Started MCMC at:", format(starttime), "\n")

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
cat("    Elapsed MCMC time:", format(round(stoptime - starttime)), "\n")

## iteratively change F_max to find S that gives max H
early_keep_t = 1:10; early_keep_y = 1:10
late_keep_t = nt - 9:0; late_keep_y = ny - 9:0
all_keep_t = 1:nt; all_keep_y = 1:ny

cat("  |--- Extracting Samples for MSY Calculations ---|\n")
samps_early = prep_samples(post$samples, keep_t = early_keep_t, keep_y = early_keep_y, eggs_trend = eggs_trend, silent = silent)
samps_late = prep_samples(post$samples, keep_t = late_keep_t, keep_y = late_keep_y, eggs_trend = eggs_trend, silent = silent)
samps_all = prep_samples(post$samples, keep_t = all_keep_t, keep_y = all_keep_y, eggs_trend = eggs_trend, silent = silent)

cat("  |--- Performing MSY Calculations ---|\n")
msy_early = msy_search(samps_early, spawn_units = ifelse(eggs, "eggs", "fish"), silent = silent)
msy_late = msy_search(samps_late, spawn_units = ifelse(eggs, "eggs", "fish"), silent = silent)
msy_all = msy_search(samps_all, spawn_units = ifelse(eggs, "eggs", "fish"), silent = silent)
msy = abind::abind(msy_early, msy_all, msy_late, along = 4)
dimnames(msy)[[4]] = c("early", "all", "late")

# save files
cat("  |--- Saving Output Files ---|\n")
saveRDS(msy, file.path(out_dir, msy_name))
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
      vuln_trend = vuln_trend
    )), file.path(out_dir, meta_name))

if (beep) beepr::beep()

cat("\n  Model", model, "Done\n.")
