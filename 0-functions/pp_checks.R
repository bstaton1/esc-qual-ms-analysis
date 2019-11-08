
# for multinomial distribution
# q_samps is the result of post_subset(post, "q_esc", matrix = T) (q_esc is an example)
# x_obs is the observed matrix of counts by [year,age/sex]

mult_pp_check = function(q_samps, x_obs, progress = F) {
  # number of mcmc samples
  saved = nrow(q_samps)
  
  # multinomial sample size (observed)
  n = rowSums(x_obs)
  
  # containers
  fit_obs = fit_new = rep(NA, saved)
  
  # loop through mcmc iters
  for (i in 1:saved) {
    if (progress) StatonMisc::progress_updater(i, saved, rnd = 1, indent = 2)
    
    # multinomial probability vector for this mcmc sample
    q_fit = array_format(q_samps[i,])
    
    # expected counts
    expected = apply(q_fit, 2, function(x) x * n)
    
    # simulated data: counts at each age/sex by year
    x_new = t(sapply(1:nrow(x_obs), function(j) rmultinom(1, n[j], q_fit[j,])))
    
    # fit criterion for observed and simulated data: chi-squared statistic
    x_diff_new = (x_new - expected)^2/expected
    x_diff_obs = (x_obs - expected)^2/expected
    
    # sum up deviance measures across years and age/sexes
    fit_new[i] = sum(x_diff_new, na.rm = T)
    fit_obs[i] = sum(x_diff_obs, na.rm = T)
  }
  
  # output
  list(
    fit_new = fit_new,
    fit_obs = fit_obs
  )
}

# for lognormal
# x_samps: posterior samples of the predicted number each year (e.g., post_subset(post, "S_t[", matrix = T))
# x_sig: observation sd each year
# x_obs: observed number each year

lnorm_pp_check = function(x_samps, x_sig, x_obs, progress = F) {
  # number of mcmc samples
  saved = nrow(x_samps)
  
  # containers
  fit_new = fit_obs = rep(NA, saved)
  
  # take logs to make code below simpler
  log_x_obs = log(x_obs)
  log_x_samps = log(x_samps)
  
  # loop through mcmc iters
  for (i in 1:saved) {
    if (progress) StatonMisc::progress_updater(i, saved, rnd = 1, indent = 2)
    
    # simulated data: log number
    log_x_new = rnorm(ncol(x_samps), log_x_samps[i,], x_sig)

    # fit criterion for observed and simulated data: sum of squares on log-scale
    fit_new[i] = sum((log_x_new - log_x_samps[i,])^2)
    fit_obs[i] = sum((log_x_obs - log_x_samps[i,])^2)
  }
  
  # output
  list(
    fit_new = fit_new,
    fit_obs = fit_obs
  )
}

# create a nice standardized plot
# check_list: the output from mult_pp_check or lnorm_pp_check
# main: the title
# axis_q: what quantile of the posterior samples of fit criterion do you wish to limit the axis limits to?
# e.g., if for one posterior sample the fit was outrageously poor for either the new or observed data
# it may be difficult to see the pattern. In this case, set axis_q = c(0, 0.975) to exclude outliers from view
# include the bayesian p-value on the plot?

pp_check_plot = function(check_list, main = NULL, axis_q = NULL, p_val = F) {
  with(check_list, {
    if (!is.null(axis_q)) {
      lim = range(quantile(fit_new, axis_q), quantile(fit_obs, axis_q))
    } else {
      lim = range(fit_new, fit_obs)
    }
    
    mean_obs = mean(fit_obs)
    mean_new = mean(fit_new)

    plot(fit_new ~ fit_obs , xlim = lim, ylim = lim, 
         xlab = "Observed Fit Measure",
         ylab = "Simulated Fit Measure",
         main = main,
         pch = 16, col = scales::alpha("grey20", 0.05))
    abline(v = mean_obs, col = "blue", lty = 2)
    abline(h = mean_new, col = "blue", lty = 2)
    abline(0,1, col = "blue", lty = 2)
    
    if (p_val) {
      usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
      text(usr[1] - xdiff * 0.015, usr[4] - ydiff * 0.05, pos = 4, labels = round(mean(fit_obs > fit_new), 2), font = 2)
    }
  })
}