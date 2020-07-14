
##### CREATE STRATA KEY FROM A RANGE #####
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

##### PREPARE THE RAW ESCAPEMENT FILES #####
esc_data_prep = function(dat) {
  
  # reformat dates
  colnames(dat)[1] = "date"
  dat$date = paste(dat$date, dat$year, sep = "/")
  
  # big data manipulation
  dat %>% 
    # remove other species and keep only years in range
    filter(species == "Chinook" & year %in% all_years) %>%
    # create the doy variable
    group_by(year) %>%
    mutate(doy = date2doy(date)) %>%
    ungroup %>% 
    # get a total passage estimate, estimates + observed
    group_by(year, doy) %>%
    summarise(count = sum(count, na.rm = T), .groups = "drop")
}

##### PREPARE THE RAW ASL FILES #####
asl_data_prep = function(dat) {
  colnames(dat)[1] = "date"
  dat %>%
    # remove other species and keep only years in range
    filter(species == "Chinook" & year %in% all_years) %>%
    # create a day of the year variable
    group_by(year) %>%
    mutate(doy = date2doy(date)) %>%
    ungroup %>%
    # remove fish that weren't aged or sexed successfully
    filter(!is.na(fw_age) & !is.na(sw_age) & sex %in% c("male", "female") & !is.na(length)) %>%
    # at a total age variable
    mutate(age = fw_age + sw_age + 1) %>%
    # keep only the variables we are interested in
    select(year, doy, age, sex, length) %>%
    # keep only ages we are interested in
    filter(age %in% 4:7)
}


##### PREPARE THE PREP-ED ESCAPEMENT DATA #####
esc_data_prep2 = function(esc) {
  # calculates counts by year and strata
  esc = esc %>% 
    group_by(year, stratum) %>% 
    summarise(count = sum(count), .groups = "drop") %>%
    dcast(year ~ stratum, value.var = "count")
  esc[is.na(esc)] = 0
  esc
}

##### PREPARE THE PREP-ED ASL DATA #####

asl_data_prep2 = function(asl) {
  
  # create an age/sex variable
  asl$age_sex = paste(substr(asl$sex, 1, 1), asl$age, sep = "")
  
  # calculate age and sex composition by year and stratum
  mean_length = asl %>%
    group_by(year, stratum, age_sex) %>%
    summarize(mean_length = mean(length), .groups = "drop") %>%
    dcast(year + stratum ~ age_sex, value.var = "mean_length")
  
  # calculate number of fish aged/sexed/lengthed successfully
  n = asl %>% group_by(year, stratum) %>% summarize(n = n(), .groups = "drop")
  
  merge(mean_length, n, by = c("year", "stratum"), all = T)
}

##### PERFORM THE TEMPORAL WEIGHTED AVERAGE #####

get_wt_avg = function(yr, asl, esc) {
  if (yr %in% esc$year & yr %in% asl$year) {
    asl_strata = filter(asl, year == yr) %>%
      select(stratum) %>%
      unlist %>% unname %>%
      unique
    
    n_tot = asl %>% filter(year == yr) %>%
      select(n) %>% unlist %>% unname %>% sum(na.rm = T)
    
    esc_j = filter(esc, year == yr) %>%
      select(-year) %>% select(asl_strata) %>% unlist
    
    
    mean_length_j = filter(asl, year == yr) %>%
      select(-year) %>% select(-n) %>%
      melt(id.vars = "stratum", variable.name = "age_sex", value.name = "mean_length") %>%
      dcast(age_sex ~ stratum, value.var = "mean_length") %>% select(-age_sex)
    
    x = mean_length_j[1,]
    out = apply(mean_length_j, 1, function(x) {
      discard_j = which(is.na(x))
      tmp_esc_j = esc_j
      tmp_esc_j[discard_j] = 0
      pi_j = tmp_esc_j/sum(tmp_esc_j)
      sum(x * pi_j, na.rm = T)
      })
    out = c(out, n_tot)
    out[out == 0] = NA
  } else {
    out = c(rep(NA, 8), 0)
  }
  
  out
}

interp = function(x) {
  
  # if first or last row is missing, have separate rule
  if (is.na(x[1,2])) {
    x[1,2] = mean(x[1:10,2], na.rm = T)
  }
  if (is.na(x[nrow(x),2])) {
    x[nrow(x),2] = mean(x[(nrow(x) - 5):nrow(x),2], na.rm = T)
  }
  
  obs_y = x[!is.na(x[,2]),"year"]
  nobs_y = x[is.na(x[,2]),"year"]
  all_y = x[,"year"]
  x = x[,-1]
  
  for (o in 1:length(nobs_y)) {
    fiy = max(obs_y[obs_y < nobs_y[o]])
    liy = min(obs_y[obs_y > nobs_y[o]])
    
    pred = approx(x = c(fiy, liy), y = c(x[all_y %in% c(fiy, liy)]), xout = nobs_y[o])$y
    
    x[all_y == nobs_y[o]] = pred
  }
  x
}

