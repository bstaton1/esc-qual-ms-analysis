
asl_data_prep = function(dat) {
  
  wk3 = paste("6", 10:16, sep = "/")
  wk4 = paste("6", 17:23, sep = "/")
  wk5 = paste("6", 24:30, sep = "/")
  wk6 = paste("7", 1:7, sep = "/")
  wk7 = paste("7", 8:14, sep = "/")
  wk8 = paste("7", 15:21, sep = "/")
  wk9 = paste("7", 22:28, sep = "/")
  
  
  dat = dat %>%
    # remove other species and keep only years in range
    filter(species == "Chinook" & year %in% all_years) %>%
    # create a day of the year variable
    group_by(year) %>%
    mutate(doy = date2doy(date)) %>%
    ungroup %>%
    # remove fish that weren't aged or sexed successfully
    filter(!is.na(fwa) & !is.na(swa) & sex %in% c("male", "female")) %>%
    mutate(date = substr(date, 1, nchar(date) - 5)) %>%

    # at a total age variable
    mutate(age = fwa + swa + 1) %>%
    
    # add an age_sex variable
    mutate(age_sex = paste(substr(sex, 1, 1), age, sep = "")) %>%
    # keep only the variables we are interested in
    select(year, date, doy, age, sex, age_sex, length) %>%
    # keep only ages we are interested in
    filter(age %in% 4:7)
  
  dat$week = ifelse(dat$date %in% wk3, "wk3", 
         ifelse(dat$date %in% wk4, "wk4", 
                ifelse(dat$date %in% wk5, "wk5", 
                       ifelse(dat$date %in% wk6, "wk6", 
                              ifelse(dat$date %in% wk7, "wk7", 
                                     ifelse(dat$date %in% wk8, "wk8", 
                                            ifelse(dat$date %in% wk9, "wk9", NA)))))))
  
  # this excludes ~500 obs, mostly before wk3 and in early years
  dat = filter(dat, !is.na(week))
  
  # create a stratum variable
  dat$stratum = ifelse(dat$week %in% c("wk3", "wk4"), 1,
                       ifelse(dat$week %in% c("wk5", "wk6"), 2, 
                              ifelse(dat$week %in% c("wk7", "wk8", "wk9"), 3, NA)))
  
  dat
}
asl_data_prep2 = function(asl) {
  
  # calculate age and sex composition by year and stratum
  asl = asl %>%
    group_by(year, stratum, age_sex) %>%
    summarize(n_age_sex = n()) %>%
    ungroup %>%
    group_by(year, stratum) %>%
    mutate(n_tot = sum(n_age_sex)) %>%
    ungroup() %>%
    mutate(age_sex_comp = round(n_age_sex/n_tot, 2))
  
  comp = asl %>%
    dcast(year + stratum ~ age_sex, value.var = "age_sex_comp")
  
  n = asl %>% 
    dcast(year + stratum ~ age_sex, value.var = "n_age_sex") %>%
    select(-year) %>% select(-stratum) %>% rowSums(na.rm = T)
  
  comp$n = n
  comp[is.na(comp)] = 0
  comp
}

get_wt_avg = function(yr, asl, harv) {
  if (yr %in% harv$year & yr %in% asl$year) {
    asl_strata = filter(asl, year == yr) %>%
      select(stratum) %>%
      unlist %>% unname %>%
      unique
    
    n_tot = asl %>% filter(year == yr) %>%
      select(n) %>% unlist %>% unname %>% sum(na.rm = T)
    
    harv_j = filter(harv, year == yr) %>%
      select(-year) %>% select(asl_strata) %>% unlist
    
    if (sum(harv_j) == 0) {
      pi_j = 1
    } else {
      pi_j = harv_j/sum(harv_j)
    }
   
    age_comp_j = filter(asl, year == yr) %>%
      select(-year) %>% select(-n) %>%
      melt(id.vars = "stratum", variable.name = "age_sex", value.name = "comp") %>%
      dcast(age_sex ~ stratum, value.var = "comp") %>% select(-age_sex)
    
    out = apply(age_comp_j, 1, function(x) sum(x * pi_j))
    out = out/sum(out)
    out = c(out, n_tot)
  } else {
    out = c(rep(NA, 8), 0)
  }
  
  out
}

