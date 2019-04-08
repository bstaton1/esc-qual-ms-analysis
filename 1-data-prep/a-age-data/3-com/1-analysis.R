
##### SESSION SETUP #####
rm(list = ls(all = T))

# needed packages
suppressMessages(library(dplyr))
suppressMessages(library(reshape2))
source("0-functions.R")

setwd("C:/Users/bas0041/Dropbox/PhD Project/Manuscripts/Escapement Quality/esc-qual-ms-analysis/1-data-prep/a-age-data/3-com/")

# do you want to write the output?
write = T

# the range of years observed across all stocks
all_years = 1976:2017

# directories
dat_dir = "inputs"
out_dir = "outputs"

# create the output directory if it doesn't exist
if (!dir.exists(out_dir)) dir.create(out_dir)

##### CALCULATE AGE/SEX COMPOSITION WEIGHTED BY HOW MANY FISH CAPTURED IN EACH 2WEEK PERIOD #####

# prepare asl data (see 0-functions.R)
asl_raw = read.csv(file.path(dat_dir, "Commercial ASL_W1.csv"), stringsAsFactors = F)

# proof that 1984 was first year of net restrictions
with(asl_raw, table(mesh, year))

asl = asl_data_prep2(asl_data_prep(asl_raw))

# prepare harvest data: number caught each period
harv_raw = read.csv(file.path(dat_dir, "weekly commercial harvest.csv"))
harv = data.frame(year = harv_raw$year,
                  "1" = harv_raw$wk3 + harv_raw$wk4,
                  "2" = harv_raw$wk5 + harv_raw$wk6,
                  "3" = harv_raw$wk7 + harv_raw$wk8 + harv_raw$wk9)

# apply weighted average calculations to each year separately
out = t(sapply(all_years, function(x) get_wt_avg(yr = x, asl, harv)))

# combine into a data.frame
dat = data.frame(year = all_years, out)
colnames(dat) = c("year", "f4", "f5", "f6", "f7", "m4", "m5", "m6", "m7", "n_aged")

write.csv(dat, file.path(out_dir, "com-age-sex-comp.csv"), row.names = F)
