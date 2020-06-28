

##### SESSION SETUP #####
rm(list = ls(all = T))

# needed packages
suppressMessages(library(dplyr))
suppressMessages(library(reshape2))
suppressMessages(library(StatonMisc))

source("0-functions.R")

setwd("C:/Users/bas0041/Dropbox/PhD Project/Manuscripts/Escapement Quality/esc-qual-ms-analysis/1-data-prep/b-length-data")

# the range of years observed across all stocks
all_years = 1976:2017

write = T

# directories
asl_dir = "inputs/asl"
esc_dir = "inputs/daily-esc/"
out_dir = "outputs"

# create the output directory if it doesn't exist
if (!dir.exists(out_dir)) dir.create(out_dir)

# files
asl_files = dir(asl_dir)
esc_files = dir(esc_dir)

# the stock names in the correct order for this analysis
stocks = unname(sapply(asl_files, function(x) unlist(strsplit(x, "_"))[1]))

# the length of a temporal stratum (days)
stratum_length = 14

##### GET MEAN LENGTH AT AGE AND SEX AND STOCK AND YEAR THAT HAVE IT #####
s = 1
dat_all = NULL
for (s in 1:length(stocks)) {
  
  cat("Crunching data for stock:", stocks[s], "\n")
  
  # start with escapement data
  tmp = read.csv(
    paste(esc_dir, esc_files[s], sep = "/"),
    stringsAsFactors = F
  )
  esc_raw = esc_data_prep(tmp)
  
  # then do asl data
  tmp = read.csv(
    paste(asl_dir, asl_files[s], sep = "/"),
    stringsAsFactors = F
  )
  asl_raw = asl_data_prep(tmp); rm(tmp)
  
  # get min and max observed days
  min_doy = min(c(asl_raw$doy, esc_raw$doy))
  max_doy = max(c(asl_raw$doy, esc_raw$doy))
  cat("DOY Range:", min_doy, "-", max_doy, "\n")
  
  # obtain a strata key
  strata_key = 
    create_strata_key(min_doy, max_doy, stratum_length) %>%
    rename(doy = x)
  
  # merge it with the escapement data set
  esc_raw = esc_raw %>%
    merge(strata_key, by = "doy") %>%
    arrange(year, doy)
  
  # merge it with the asl data set
  asl_raw = asl_raw %>%
    merge(strata_key, by = "doy") %>%
    arrange(year, doy, sex, age)
  
  # format again for analysis
  esc = esc_data_prep2(esc_raw)
  asl = asl_data_prep2(asl_raw)
  
  # apply weighted average calculations to each year separately
  out = t(sapply(all_years, function(x) get_wt_avg(yr = x, asl, esc)))
  
  # combine into a data.frame
  dat = data.frame(year = all_years, stock = stocks[s], out)
  colnames(dat) = c("year", "stock", "f4", "f5", "f6", "f7", "m4", "m5", "m6", "m7", "n_aged")
  
  # write the output
  if (write) {
    write.csv(dat, paste("outputs/", stocks[s], "_mean_length.csv", sep = ""), row.names = F)
  }
  
  dat_all = rbind(dat_all, dat)
  head(dat); tail(dat)
}

##### COMBINE STOCK-SPECIFIC ESTIMATES INTO A DRAINAGE-WIDE ESTIMATE #####
weir_counts = read.csv("inputs/weir_counts.csv")
colnames(weir_counts)[1] = "year"
weir_counts = weir_counts %>% melt(id.vars = "year", value.name = "passage", variable.name = "stock")

dat_ave = NULL

yr = all_years[1]

A = c(paste("f", 4:7, sep = ""), paste("m", 4:7, sep = ""))
for (yr in all_years) {
  age_y = filter(dat_all, year == yr & n_aged > 0)
  weir_y = filter(weir_counts, year == yr & stock %in% age_y$stock)
  dat_y = merge(age_y, weir_y, by = c("year", "stock"))
  z = numeric(8); names(z) = A
  for (a in 1:8) {
    dat_ya = dat_y[,c(A[a], "passage")]
    dat_ya$passage[is.na(dat_ya[,A[a]])] = NA
    dat_ya$p_passage = dat_ya$passage/sum(dat_ya$passage, na.rm = T)
    
    z[A[a]] = sum(dat_ya[,A[a]] * dat_ya$p_passage, na.rm = T)
  }

  z = t(data.frame(c(year = yr, z))); rownames(z) = NULL
  z
  
  dat_ave = rbind(dat_ave, z)
  
}
dat_ave[dat_ave == 0] = NA
dat_ave = as.data.frame(dat_ave)

# remove the questionable looking f4 in 1991
dat_ave[dat_ave$year == 1991,"f4"] = NA

x = dat_ave[,c("year", "f4")] 

A = paste(rep(c("f", "m"), each = 4), rep(4:7, 2), sep = "")
dat_ave_interp = dat_ave
dat_ave_interp[,A] = NA
for (a in 1:8) {
  dat_ave_interp[,A[a]] = interp(dat_ave[,c("year", A[a])])
}

write.csv(dat_ave, file.path(out_dir, "esc-mean-length-no-interp.csv"), row.names = F)
write.csv(dat_ave_interp, file.path(out_dir, "esc-mean-length.csv"), row.names = F)
