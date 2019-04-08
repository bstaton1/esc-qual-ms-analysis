
create_strata_key = function(min_x, max_x, len) {
  
  # number of total x elements (e.g., days)
  n_x = length(min_x:max_x)
  
  # the starting and ending days of each stratum
  s_start = seq(min_x, max_x, len)
  s_end = c(seq(min_x + len, max_x, len), max_x)
  
  # number of strata
  n_s = length(s_start)
  
  # build the key
  strata_key = data.frame(x = min_x:max_x)
  strata_key$stratum = NULL
  
  for (i in 1:n_x) {
    for (j in 1:n_s) {
      if (strata_key[i,"x"] >= s_start[j] & strata_key[i,"x"] < s_end[j]) strata_key[i,"stratum"] = j
    }
  }
  
  strata_key[n_x,"stratum"] = n_s
  
  strata_key
}

asl_data_prep = function(dat) {
  
  dat %>%
    # remove other species and keep only years in range
    filter(species == "Chinook" & year %in% all_years) %>%
    # create a day of the year variable
    group_by(year) %>%
    mutate(doy = StatonMisc::date2doy(date)) %>%
    ungroup %>%
    # remove fish that weren't aged or sexed successfully
    filter(!is.na(fwa) & !is.na(swa) & sex %in% c("male", "female")) %>%
    mutate(date = substr(date, 1, nchar(date) - 5)) %>%
    mutate(location = str_remove(location, " \\(Village/City\\)")) %>%

    # at a total age variable
    mutate(age = fwa + swa + 1) %>%
    
    # add an age_sex variable
    mutate(age_sex = paste(substr(sex, 1, 1), age, sep = "")) %>%
    
    # keep only the variables we are interested in
    select(year, location, date, doy, age, sex, age_sex, length) %>%
    
    # keep only ages we are interested in
    filter(age %in% 4:7)
  
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

cal_prep = function(cal, yr, v) {
  cal = cal[cal$year == yr,]
  
  if (nrow(cal) > 0) {
    # determine which village calendars to keep
    cal$location[str_detect(cal$location, "Kalskag")] = "Kalskag"
    cal_loc = unique(cal$location)
    keep_loc = cal_loc[cal_loc %in% v]
    if (length(keep_loc) == 0) keep_loc = cal_loc
    cal = cal[cal$location %in% keep_loc,]
    
    # fix dates
    cal$date = paste(substr(cal$date, 1, nchar(cal$date) - 5), cal$year, sep = "/")
    
    # extract only important variables
    cal = cal[,c("year", "date", "chinook")]
    cal = cal[!is.na(cal$chinook),]
    
    # add a doy variable
    cal$doy = StatonMisc::date2doy(cal$date)
    
    # merge in the stratum_key
    cal = merge(x = cal, y = strata_key, by = "doy", all.y = T)
    
    cal %>% group_by(stratum) %>% summarize(catch = sum(chinook, na.rm = T)) %>% mutate(year = yr) %>%
      dcast(year ~ stratum, value.var = "catch")
  } else {
    data.frame(year = yr, "1" = NA, "2" = NA, "3" = NA, "4" = NA, "5" = NA)
  }
}

get_wt_avg = function(yr, asl, cal) {
  if (yr %in% cal$year & yr %in% asl$year) {
    asl_strata = filter(asl, year == yr) %>%
      select(stratum) %>%
      unlist %>% unname %>%
      unique
    
    n_tot = asl %>% filter(year == yr) %>%
      select(n) %>% unlist %>% unname %>% sum(na.rm = T)
    
    cal_j = filter(cal, year == yr) %>%
      select(-year) %>% select(asl_strata) %>% unlist
    
    if (sum(cal_j) == 0) {
      pi_j = 1
    } else {
      pi_j = cal_j/sum(cal_j)
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

