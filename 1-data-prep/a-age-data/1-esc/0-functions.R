
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
    summarise(count = sum(count, na.rm = T)) %>%
    ungroup
}

##### PREPARE THE RAW ASL FILES
asl_data_prep = function(dat) {
  dat %>%
    # remove other species and keep only years in range
    filter(species == "Chinook" & year %in% all_years) %>%
    # create a day of the year variable
    group_by(year) %>%
    mutate(doy = date2doy(date)) %>%
    ungroup %>%
    # remove fish that weren't aged or sexed successfully
    filter(!is.na(fw_age) & !is.na(sw_age) & sex %in% c("male", "female")) %>%
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
    summarise(count = sum(count)) %>%
    dcast(year ~ stratum, value.var = "count")
  esc[is.na(esc)] = 0
  esc
}

##### PREPARE THE PREP-ED ASL DATA #####
asl_data_prep2 = function(asl) {
  
  # create an age/sex variable
  asl$age_sex = paste(substr(asl$sex, 1, 1), asl$age, sep = "")
  
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

##### PERFORM THE TEMPORAL WEIGHTED AVERAGE #####
get_wt_avg = function(yr, asl, esc) {
  if ((yr %in% esc$year & yr %in% asl$year)) {
    asl_strata = filter(asl, year == yr) %>%
      select(stratum) %>%
      unlist %>% unname %>%
      unique
    
    n_tot = asl %>% filter(year == yr) %>%
      select(n) %>% unlist %>% unname %>% sum(na.rm = T)
    
    esc_j = filter(esc, year == yr) %>%
      select(-year) %>% select(asl_strata) %>% unlist
    
    pi_j = esc_j/sum(esc_j)
    
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
