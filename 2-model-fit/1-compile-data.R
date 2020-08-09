
# THIS SCRIPT REQUIRES AN OBJECT CALLED "model"
# to be defined prior to sourcing it

# location of data files
data_dir = "2-model-fit/inputs"

### obtain meta info for all models
mod_key = read.csv("2-model-fit/model-key.csv", stringsAsFactors = F)

# stop if model is not found
if (!(model %in% mod_key$model)) stop("Info for requested model not found.")

# extract the meta info for this model
mod_info = mod_key[mod_key$model == model,]

### dimensional variables
ft = 1976
lt = 2019
years = ft:lt
nt = length(years)

# age dimensions
a_min = 4                   # first age at maturity
a_max = 7                   # last age at maturity
ages = a_min:a_max
na = length(ages)     # number of ages at maturity

# recruitment year dimensions: with or without brood year spawners
fy = ft - a_max      # first brood year in model (no spawners observed)
ly = lt - a_min      # last brood year in model (has spawners)
ny = length(fy:ly)

# age sex combinations
A = paste(rep(c("f", "m"), each = na), rep(a_min:a_max, 2), sep = "")
f_ind = which(stringr::str_detect(A, "f"))
m_ind = which(stringr::str_detect(A, "m"))

## abundance-related states
states = read.csv(file.path(data_dir, "run-harv-ests-cv.csv"))

# calculate escapement
states$S_tot_obs = states$N_tot_obs - states$H_com_tot_obs - states$H_sub_tot_obs

# calculate CV of escapement: sum the log-normal variances of the components
states$S_tot_obs_cv = 
  StatonMisc::sig2cv(
    sqrt(
      StatonMisc::cv2sig(states$N_tot_obs_cv)^2 +
       StatonMisc::cv2sig(states$H_com_tot_obs_cv)^2 + 
       StatonMisc::cv2sig(states$H_sub_tot_obs_cv)^2
    )
  )
  
## mean length at age
ldat = read.csv(file.path(data_dir, "esc-mean-length.csv"))
ldat = as.matrix(round(ldat[,-1]))

# calculate perimeter of net types
mesh8_perim = 8 * 2 * 25.4  
mesh6_perim = 6 * 2 * 25.4

# calculate rlm (ratio of length to net perimeter)
rlm = array(NA, dim = c(nt, na, 2, 2))
rlm[,,1,1] = ldat[,f_ind]/mesh8_perim  # females mesh8
rlm[,,1,2] = ldat[,f_ind]/mesh6_perim  # females mesh6
rlm[,,2,1] = ldat[,m_ind]/mesh8_perim  # males mesh8
rlm[,,2,2] = ldat[,m_ind]/mesh6_perim  # males mesh6

## gear types
mesh = read.csv(file.path(data_dir, "mesh-types.csv"))

## reproductive units per individual each year/age/sex
z_unit = mod_info$z_unit

# depending on the unit, create z
if (z_unit == "fish_count") {
  # if fish_count, all elements equal 1
  z_mat = ldat
  z_mat[,c(f_ind,m_ind)] = 1
} else {
  # otherwise, it is a power function of length
  # unless fish is a male, then it is zero
  z_mat = ldat
  z_mat[,m_ind] = 0
  z_mat = mod_info$a_coef * z_mat ^ mod_info$b_coef
  
  # if not including a length trend, replace z value each year with the mean across all years
  if (mod_info$length_trend == 0) {
    z_mat = matrix(colMeans(z_mat), nt, na * 2, byrow = T)
  }
}

# create the output z object as an array: females are [,,1], males [,,2]
z = abind::abind(
  z_mat[,f_ind],
  z_mat[,m_ind],
  along = 3)
dimnames(z) = NULL

### function to prep the age/sex composition data by fate and year
# n_eff_method are different ways of calculating effective sample size
create_x_data = function(x_ages, n_eff_method = "scale_100") {
  p_ages = x_ages[,A]
  n_samp = x_ages[,"n_aged"]
  
  valid_methods = c(
    "scale_50", "scale_100", "scale_200", "min_100", "min_200", "n_samp", "n_samp_div2", "n_samp_div4", "sqrt"
  )
  
  if (!(n_eff_method %in% valid_methods)) {
    stop ("method '", n_eff_method, "' is invalid. Accepted options are: \n", StatonMisc::list_out(valid_methods, per_line = 1, indent = "  "))
  }
  
  if (n_eff_method == "scale_50") {
    n_eff = n_samp/max(n_samp) * 50
  }
  
  if (n_eff_method == "scale_100") {
    n_eff = n_samp/max(n_samp) * 100
  }
  
  if (n_eff_method == "scale_200") {
    n_eff = n_samp/max(n_samp) * 200
  }
  
  if (n_eff_method == "min_100") {
    n_eff = ifelse(n_samp > 100, 100, n_samp)
  }
  
  if (n_eff_method == "min_200") {
    n_eff = ifelse(n_samp > 200, 200, n_samp)
  }
  
  if (n_eff_method == "n_samp_div2") {
    n_eff = n_samp/2
  }
  
  if (n_eff_method == "n_samp_div4") {
    n_eff = n_samp/4
  }
  
  if (n_eff_method == "n_samp") {
    n_eff = n_samp
  }
  
  if (n_eff_method == "sqrt") {
    n_eff = sqrt(n_samp)
  }
  
  x_dat = apply(p_ages, 2, function(x) round(x * n_eff))
  x_dat[is.na(x_dat)] = 0
  x_dat
}

## escapement age/sex composition
e_ages = read.csv(file.path(data_dir, "esc-age-sex-comp.csv"))
colnames(e_ages)[colnames(e_ages) == "N_aged_tot"] = "n_aged"
e_ages = e_ages[,c("year", A, "n_aged")]
x_esc_tas = create_x_data(e_ages, n_eff_method = mod_info[,"esc_n_eff"])
n_esc = rowSums(x_esc_tas)

## commercial age/sex composition
c_ages = read.csv(file.path(data_dir, "com-age-sex-comp.csv"))
x_com_tas = create_x_data(c_ages, n_eff_method = mod_info[,"com_n_eff"])
n_com = rowSums(x_com_tas)

## subsistence age/sex composition
s_ages = read.csv(file.path(data_dir, "sub-age-sex-comp.csv"))
x_sub_tas = create_x_data(s_ages, n_eff_method = mod_info[,"sub_n_eff"])
n_sub = rowSums(x_sub_tas)

## SELECTIVITY PARAMETERS: FOR PRIORS
Vtau_yukon = c(1.920, 0.012)
Vsig_yukon = c(0.204, 0.021)
Vtha_yukon = c(0.622, 0.033)
Vlam_yukon = c(-0.547,0.075)

Vtau_kusko = c(1.897, 0.046)
Vsig_kusko = c(0.236, 0.081)
Vtha_kusko = c(0.756, 0.186)
Vlam_kusko = c(-1.049,0.382)

## bundle into a list for jags
jags_dat = list(
  # dimensions
  nt = nt, na = na, ny = ny, a_min = a_min, a_max = a_max,
  
  # escapement estimates
  S_obs = states$S_tot_obs,
  S_obs_sig = StatonMisc::cv2sig(states$S_tot_obs_cv),
  
  # commerical harvest estimates
  Hcom_obs = states$H_com_tot_obs,
  Hcom_obs_sig = StatonMisc::cv2sig(states$H_com_tot_obs_cv),
  com_mesh = ifelse(mesh$com == "mesh8", 1, 2),
  
  # subsistence harvest estimates
  Hsub_obs = states$H_sub_tot_obs,
  Hsub_obs_sig = StatonMisc::cv2sig(states$H_sub_tot_obs_cv),
  sub_mesh = ifelse(mesh$sub == "mesh8", 1, 2),
  
  # ratio of fish length to mesh perimeter
  rlm = rlm,
  
  # reproductive units
  z = z,
  
  # escapement age/sex comp
  x_esc = x_esc_tas,
  n_esc = n_esc,
  
  # commercial age/sex comp
  x_com = x_com_tas,
  n_com = n_com,
  
  # subsistence age/sex comp
  x_sub = x_sub_tas,
  n_sub = n_sub
)

# clean up workspace
rm(list = setdiff(ls(), c("jags_dat", "data_dir", "years", "mod_key", "model", "z_unit", "ages",
                          "Vtau_yukon","Vsig_yukon","Vtha_yukon","Vlam_yukon",
                          "Vtau_kusko","Vsig_kusko","Vtha_kusko","Vlam_kusko")))

# get all elements in as objects in workspace
for (i in 1:length(jags_dat)) assign(x = names(jags_dat)[i], value = jags_dat[[i]])
rm(i)

# cat("\n\n Data Files Prepared Successfully.\n All needed information is in the list 'jags_dat' and as local objects. \n See ls(). \n\n")
