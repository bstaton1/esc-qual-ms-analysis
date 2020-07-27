extract_jags_metadata = function(post) {
  list(
    DIC = c(pD = post$pD, DIC = post$DIC),
    mcmc.info = unlist(post$mcmc.info[c("n.chains", "n.adapt", "n.iter", "n.burnin", "n.thin")]),
    starttime = post$run.date,
    stoptime = Sys.time(),
    elapsed = format(Sys.time() - post$run.date)
  )
}
