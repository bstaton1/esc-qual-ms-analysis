
##### DATA COMPILATION FILE FOR ESCAPEMENT QUALITY MODEL #####

# location of data
data_dir = "inputs"

### dimensional variables
ft = 1976
lt = 2017
nt = length(ft:lt)

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
states = read.csv(file.path(data_dir, "esc-harv-ests-cv.csv"))

## mean length at age
ldat = read.csv(file.path(data_dir, "esc-mean-length.csv"))
ldat = as.matrix(round(ldat[,-1]))

# calculate perimeter of net types
unr_perim = 8 * 2* 25.4  
res_perim = 6 * 2 * 25.4

# calculate rlm (ratio of length to net perimeter)
rlm = array(NA, dim = c(nt, na, 2, 2))
rlm[,,1,1] = ldat[,f_ind]/unr_perim  # females unr
rlm[,,1,2] = ldat[,f_ind]/res_perim  # females res
rlm[,,2,1] = ldat[,m_ind]/unr_perim  # males unr
rlm[,,2,2] = ldat[,m_ind]/res_perim  # males res

## gear types
mesh = read.csv(file.path(data_dir, "mesh-types.csv"))

## fecundity
fecund = as.matrix(read.csv(file.path(data_dir, "predicted-fecundity.csv"))[,c(paste("f", ages, sep = ""))])
fecund_m = matrix(0, nt, na)
fecund = array(c(fecund, fecund_m), dim = c(nt, na, 2))

## escapement age/sex composition
e_ages = read.csv(file.path(data_dir, "esc-age-sex-comp.csv"))
e_ages$ess = e_ages$N_aged_tot/max(e_ages$N_aged_tot) * 100
x_esc_tas = apply(e_ages[,A], 2, function(x) round(x * e_ages$ess))
x_esc_tas[is.na(x_esc_tas)] = 0
n_esc = rowSums(x_esc_tas)

# x_esc_tas_new = matrix(NA, nt, na)
# x_esc_tas_new[,1] = rowSums(x_esc_tas[,c(1,5)])
# x_esc_tas_new[,2] = rowSums(x_esc_tas[,c(2,6)])
# x_esc_tas_new[,3] = rowSums(x_esc_tas[,c(3,7)])
# x_esc_tas_new[,4] = rowSums(x_esc_tas[,c(4,8)])

## commercial age/sex composition
c_ages = read.csv(file.path(data_dir, "com-age-sex-comp.csv"))
c_ages$ess = c_ages$n_aged/max(c_ages$n_aged) * 100
x_com_tas = apply(c_ages[,A], 2, function(x) round(x * c_ages$ess))
x_com_tas[is.na(x_com_tas)] = 0
n_com = rowSums(x_com_tas)

## commercial age/sex composition
s_ages = read.csv(file.path(data_dir, "sub-age-sex-comp.csv"))
s_ages$ess = s_ages$n_aged/max(s_ages$n_aged) * 100
x_sub_tas = apply(s_ages[,A], 2, function(x) round(x * s_ages$ess))
x_sub_tas[is.na(x_sub_tas)] = 0
n_sub = rowSums(x_sub_tas)

## bundle into a list for jags
jags_dat = list(
  nt = nt, na = na, ny = ny, a_min = a_min, a_max = a_max, ages = ages,
  
  S_obs = states$S_tot_obs,
  S_obs_sig = states$S_tot_obs_cv,
  
  Hcom_obs = states$H_com_tot_obs,
  Hcom_obs_sig = StatonMisc::cv2sig(states$H_com_tot_obs_cv),
  com_mesh = ifelse(mesh$com == "unr", 1, 2),
  
  Hsub_obs = states$H_sub_tot_obs,
  Hsub_obs_sig = StatonMisc::cv2sig(states$H_sub_tot_obs_cv),
  sub_mesh = ifelse(mesh$sub == "unr", 1, 2),
  
  rlm = rlm,
  
  fecund = fecund,
  
  x_esc = x_esc_tas,
  n_esc = n_esc,
  
  x_com = x_com_tas,
  n_com = n_com,
  
  x_sub = x_sub_tas,
  n_sub = n_sub
)

# clean up workspace
rm(list = setdiff(ls(), c("jags_dat", "data_dir")))

# get all elements in as objects in workspace
for (i in 1:length(jags_dat)) assign(x = names(jags_dat)[i], value = jags_dat[[i]])
rm(i)

cat("\n\n Data Files Prepared Successfully.\n All needed information is in the list 'jags_dat' and as local objects. \n See ls(). \n\n")

## prepare csv for matt
# states_dat = cbind(
#   S_obs, S_obs_sig, Hcom_obs, Hcom_obs_sig, com_mesh, Hsub_obs, Hsub_obs_sig, sub_mesh)
# 
# colnames(x_esc) = paste("esc", colnames(x_esc), sep = "_")
# colnames(x_com) = paste("com", colnames(x_com), sep = "_")
# colnames(x_sub) = paste("sub", colnames(x_sub), sep = "_")
# 
# full_df = cbind(year = 1976:2017, states_dat, x_esc, x_com, x_sub)
# 
# write.csv(full_df, "esc-qual-dataframe.csv", row.names = F)