
##### SESSION SETUP #####
rm(list = ls(all = T))

# WORKING DIRECTORY SHOULD BE SET TO PROJECT DIRECTORY

# needed packages
suppressMessages(library(dplyr))
suppressMessages(library(reshape2))
suppressMessages(library(stringr))
suppressMessages(library(StatonMisc))
source("1-data-prep/a-age-data/2-sub/0-functions.R")

# do you want to write the output?
write = T

# the range of years observed across all stocks
all_years = 1976:2019

# directories
dat_dir = "1-data-prep/a-age-data/2-sub/inputs"
out_dir = "1-data-prep/a-age-data/2-sub/outputs"

# create the output directory if it doesn't exist
if (!dir.exists(out_dir)) dir.create(out_dir)

##### CALCULATE AGE/SEX COMPOSITION WEIGHTED BY HOW MANY FISH CAPTURED IN EACH 2WEEK PERIOD #####

# prepare asl data (see 0-functions.R)
asl_raw = read.csv(file.path(dat_dir, "SubsistenceASL.csv"), stringsAsFactors = F)

# proof that 2014 was first year of net restrictions:
mesh = data.frame(year = asl_raw$year, mesh = asl_raw$mesh)
mesh = mesh[asl_raw$species == "Chinook",]
mesh$le6 = ifelse(mesh$mesh <= 6, 1, 0)
p_le6 = tapply(mesh$le6, mesh$year, mean, na.rm = T)
barplot(p_le6, las = 2)

asl_raw = asl_data_prep(asl_raw)

strata_key = create_strata_key(min(asl_raw$doy), max(asl_raw$doy), 14)
colnames(strata_key)[1] = "doy"
asl_raw = merge(asl_raw, strata_key, by = "doy")

asl_yrs = unique(asl_raw$year)
villages = list()
for (y in 1:length(asl_yrs)) {
  villages[[y]] = unique(asl_raw$location[asl_raw$year == asl_yrs[y]])
}
names(villages) = asl_yrs

asl = asl_data_prep2(asl_raw)

## prepare calendar data
cal_raw = read.csv(file.path(dat_dir, "Calendar Data.csv"), stringsAsFactors = F)
cal = data.frame(t(sapply(asl_yrs, function(y) cal_prep(cal_raw, yr = y, v = villages[[as.character(y)]]))))
colnames(cal) = str_remove(colnames(cal), "X")

cal$year = unlist(cal$year)
cal = cal[order(cal$year),]

# apply weighted average calculations to each year separately
out = t(sapply(all_years, function(x) get_wt_avg(yr = x, asl, cal)))

# combine into a data.frame
dat = data.frame(year = all_years, out)
colnames(dat) = c("year", "f4", "f5", "f6", "f7", "m4", "m5", "m6", "m7", "n_aged")

if (write) {
  write.csv(dat, file.path(out_dir, "sub-age-sex-comp.csv"), row.names = F)
}
