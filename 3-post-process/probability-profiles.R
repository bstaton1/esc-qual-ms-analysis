
post.samp = samps_all
spawn_units = "fish"
vuln = "unr"
silent = F
Rmax_p = 0.9
Rmax_prob = 0.9
msy_p = 0.9
msy_prob = 0.9
S_breaks = seq(0, 300000, 1000)
nF = 500


starttime = Sys.time()
EG_unr_early = get_EG(post.samp = samps_early, spawn_units = ifelse(eggs, "eggs", "fish"), vuln = "unr")
EG_unr_late = get_EG(post.samp = samps_late, spawn_units = ifelse(eggs, "eggs", "fish"), vuln = "unr")
EG_unr_all = get_EG(post.samp = samps_all, spawn_units = ifelse(eggs, "eggs", "fish"), vuln = "unr")
EG_res_early = get_EG(post.samp = samps_early, spawn_units = "eggs", vuln = "res")
EG_res_late= get_EG(post.samp = samps_late, spawn_units = "eggs", vuln = "res")
EG_res_all = get_EG(post.samp = samps_all, spawn_units = "eggs", vuln = "res")
Sys.time() - starttime

EG_early = abind(EG_unr_early$goals, EG_res_early$goals, along = 3)
EG_late = abind(EG_unr_late$goals, EG_res_late$goals, along = 3)
EG_all = abind(EG_unr_all$goals, EG_res_all$goals, along = 3)

abind(EG_early)
p = colMeans(out)
par(xaxs = "i", yaxs = "i")
plot(p, type = "l", lwd = 2, ylim = c(0,1))
abline(h = 0.9, col = "grey", lty = 2)

segments(range(which(p > 0.9)), 0, range(which(p > 0.9)), 0.9, col = "blue")
range(which(p > 0.9))
