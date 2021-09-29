
# WORKING DIRECTORY SHOULD BE SET TO PROJECT LOCATION
# JUST HAVE PROJECT OPEN IN RSTUDIO SESSION

rm(list = ls(all = T))

##### SESSION SETUP #####

# load data/functions
model = 1  # just needed to build the data
source("load-packages.R")
source("2-model-fit/1-compile-data.R")
source("load-functions.R")
rm(model) # clear out the model object 

out_dir = "model-output/permanent"
out_files = dir(out_dir, full.names = T)

# HOW DO YOU WANT TO SAVE THE OUTPUT
file_type = "pdf"
# file_type = "png"
fig_dir = "3-post-process/ms-figs"

# create directory to store output figures if it doesn't exist
if (!dir.exists(fig_dir)) dir.create(fig_dir)

# reduce the set of models to only main-text models
keep_mods = 1:12
keep_mods = paste(paste0("-", keep_mods, "\\."), collapse = "|")
out_files = out_files[str_detect(out_files, keep_mods)]

# the file names of the output files
postfiles = out_files[str_detect(out_files, "post")]
mods = str_extract(postfiles, "[0-9]+")
metafiles = out_files[str_detect(out_files, "meta")]
msyfiles = out_files[str_detect(out_files, "msy")]
Rmaxfiles = out_files[str_detect(out_files, "Rmax")]

# create empty objects to store the output from each model
meta = list()
post_list = list()
msy = NULL
Rmax = NULL

# read in the posterior samples and meta data
for (i in 1:length(mods)) {
  progress_updater(i, length(mods))
  post_list[[i]] = readRDS(postfiles[i])
  meta[[i]] = readRDS(metafiles[i])
}

# create the ids for each model
ids = unlist(lapply(meta, id_model))

# read in the msy equilibrium quantities
msy = readRDS(msyfiles[1])
Rmax = readRDS(Rmaxfiles[1])
for (i in 2:length(mods)) {
  msy = abind(msy, readRDS(msyfiles[i]), along = 5)
  Rmax = abind(Rmax, readRDS(Rmaxfiles[i]), along = 5)
}

# give the objects model identifiers
names(meta) = ids
names(post_list) = ids
dimnames(msy)[[5]] = ids

# these could be useful for model subsetting later
N_mods = str_detect(ids, "^N-")
E_mods = str_detect(ids, "^E-")
EM_mods = str_detect(ids, "^EM-")
a_mods = str_detect(ids, "A")
s_mods = str_detect(ids, "S")
l_mods = str_detect(ids, "L")

# JOURNAL MAX FIGURE DIMENTIONS
# max width (one column): 3.5inches
# max width (two column): 7.2inches
# max height: 9.4 inches

##### CREATE WAIC TABLE #####
waic = t(sapply(meta, function(m) m$WAIC))
waic_tab = data.frame(model = rownames(waic), pD = waic[,"pD"], WAIC = waic[,"WAIC"])
write.csv(waic_tab, file.path(fig_dir, "waic.csv"), row.names = F)

##### SELECTIVITY PLOT: WITH TIME STRUCTURE #####

early_t = 1:10
all_t = 1:nt
late_t = (nt - 9):nt

summ_v = function(keep_t, keep_sex, keep_vuln) {
  inds = apply(expand.grid(keep_t, 1:4, keep_sex, keep_vuln), 1, function(x) paste(x, collapse = ","))
  keep = paste0("v[", inds, "]")
  out = apply(array_format(post_summ(post_list[["E-ASL"]], keep)["50%",]), 2, summ, na.rm = T)
  
  cbind(mean = out["mean",], lwr = out["2.5%",], upr = out["97.5%",])
}

v_f_mesh8 = abind(early = summ_v(early_t, 1, 1), all =  summ_v(all_t, 1, 1), late = summ_v(late_t, 1, 1), along = 3)
v_m_mesh8 = abind(early = summ_v(early_t, 2, 1), all =  summ_v(all_t, 2, 1), late = summ_v(late_t, 2, 1), along = 3)
v_f_mesh6 = abind(early = summ_v(early_t, 1, 2), all =  summ_v(all_t, 1, 2), late = summ_v(late_t, 1, 2), along = 3)
v_m_mesh6 = abind(early = summ_v(early_t, 2, 2), all =  summ_v(all_t, 2, 2), late = summ_v(late_t, 2, 2), along = 3)

x_t = seq(0.15,0.05, -0.05)
xf = a_min:a_max - 0.01
xm = a_min:a_max + 0.01

file_device(file.path(fig_dir, paste0("v-age.", file_type)), h = 5, w = 3.45)
par(mfrow = c(2,1), mar = c(1,1.25,0.5,0.5),  oma = c(1,1,0,0), tcl = -0.15, mgp = c(2,0.2,0), cex.axis = 0.75)
plot(1,1, ylim = c(0,1), xlim = range(min(xf) - max(x_t), max(xm) + max(x_t)),
     col = "red", pch = 16, type = "o",xaxt = "n", las = 2, xlab = "", ylab = "")
col = c("black", "grey", "white")

for (t in (1:3)) {
  lines(xf - x_t[t], v_f_mesh8[,"mean",t], type = "o", pch = 21, bg = col[t], cex = 1.4)
  lines(xm + rev(x_t)[t], v_m_mesh8[,"mean",t], type = "o", pch = 23, bg = col[t], lty = 2, cex = 1.4)
}

legend("bottomright", c("Female", "Male", "First 10 Yrs", "All Yrs", "Last 10 Yrs"), bty = "n", x.intersp = 0.5, seg.len = 2.9,
       lty = c(1, 2, NA, NA, NA), pch = c(21, 23, 22, 22, 22), pt.bg = c("white", "white", col), pt.cex = 1.4, cex = 0.75)
box()
axis(side = 1, at = 4:7, labels = 4:7)
usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
text(x = usr[1] - xdiff * 0.015, usr[3] + ydiff * 0.05, labels = "(a)", pos = 4, font = 2, cex = 0.9)
text(x = usr[1] + xdiff * 0.06, usr[3] + ydiff * 0.04, labels = "8 in. Mesh", pos = 4, font = 1, cex = 0.8)

plot(1,1, ylim = c(0,1), xlim = range(min(xf) - max(x_t), max(xm) + max(x_t)),
     col = "red", pch = 16, type = "o",xaxt = "n", las = 2, xlab = "", ylab = "")

for (t in (1:3)) {
  lines(xf - x_t[t], v_f_mesh6[,"mean",t], type = "o", pch = 21, bg = col[t], cex = 1.4)
  lines(xm + rev(x_t)[t], v_m_mesh6[,"mean",t], type = "o", pch = 23, bg = col[t], lty = 2, cex = 1.4)
}

axis(side = 1, at = 4:7, labels = 4:7)
text(x = usr[1] - xdiff * 0.015, usr[3] + ydiff * 0.05, labels = "(b)", pos = 4, font = 2, cex = 0.9)
text(x = usr[1] + xdiff * 0.06, usr[3] + ydiff * 0.04, labels = "6 in. Mesh", pos = 4, font = 1, cex = 0.8)
usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])

mtext(side = 1, line = 0.0, outer = T, "Age", cex = 0.8)
mtext(side = 2, line = 0.25, outer = T, "Selectivity", cex = 0.8)
dev.off()

##### SELECTIVITY PLOT: FULL SHAPE #####

rlm_seq = seq(min(rlm), max(rlm), length = 1000)

pearson = function(rlm, lambda, theta, sigma, tau) {
  
  # separate calculation into 5 steps
  t1 = (1 + lambda^2/(4 * theta^2))^theta
  t2 = rlm - (sigma * lambda)/(2 * theta) - tau
  t3 = (1 + t2^2/sigma^2)^-theta
  t4 = exp(-lambda * (atan(t2/sigma) + atan(lambda/(2 * theta))))
  
  v = t1 * t3 * t4
  
  # standardize the output so only one age/sex is fully vuln for a gear
  v/max(v)
}

ssm_ests = post_subset(post_list[["E-ASL"]], "^V...$", T)

ssm_pearson = function(rlm, i) {
  pearson(
    rlm,
    tau = ssm_ests[i,1], sigma = ssm_ests[i,2], theta = ssm_ests[i,3], lambda = ssm_ests[i,4]
  )
}

out = t(sapply(1:post_dim(post_list[["E-ASL"]], "saved"), function(i) ssm_pearson(rlm_seq, i)))
v_ssm = apply(out, 2, summ)
v_kusko = pearson(rlm_seq, Vlam_kusko[1], Vtha_kusko[1], Vsig_kusko[1], Vtau_kusko[1])
v_yukon = pearson(rlm_seq, Vlam_yukon[1], Vtha_yukon[1], Vsig_yukon[1], Vtau_yukon[1])

file_device(file.path(fig_dir, paste0("v-length.", file_type)), h = 3.45, w = 3.45)
par(mar = c(2,2.25,2,0.5), mgp = c(1.25,0.2,0), tcl = -0.15, cex.axis = 0.75, lend = "square", cex.lab = 0.8)
plot(1,1, xlim = range(rlm_seq), ylim = c(min(v_ssm[4,]), 1), type = "n", ylab = "Selectivity", las = 2, xaxt = "n", xlab = "", lwd = 2, lty = 1)
polygon(x = c(rlm_seq, rev(rlm_seq)), y = c(v_ssm[4,], rev(v_ssm[5,])),
        border = NA, col = "grey90")

lines(v_ssm[3,] ~ rlm_seq, lwd = 4, lend = "round")

lines(v_yukon ~ rlm_seq, lty = 2)
lines(v_kusko ~ rlm_seq, lty = 3, lwd = 1.4)

# keep_t = (nt - 9):nt
keep_t = 1:nt

at_rlm1 = sapply(colMeans(rlm[keep_t,,1,1]), function(a) which_closest(rlm_seq, a))
at_rlm2 = sapply(colMeans(rlm[keep_t,,1,2]), function(a) which_closest(rlm_seq, a))

points(v_ssm[3,at_rlm1] ~ colMeans(rlm[keep_t,,1,1]),
       pch = 21, bg = "grey90", col = "grey90", cex = 1.5)
points(v_ssm[3,at_rlm2] ~ colMeans(rlm[keep_t,,1,2]),
       pch = 21, bg = "grey90", col = "grey90", cex = 1.5)
points(v_ssm[3,at_rlm1] ~ colMeans(rlm[keep_t,,1,1]),
       pch = c("4", "5", "6", "7"), col = "black", cex = 0.8)
points(v_ssm[3,at_rlm2] ~ colMeans(rlm[keep_t,,1,2]),
       pch = c("4", "5", "6", "7"), col = "grey60", cex = 0.8)

lines(v_ssm[4,] ~ rlm_seq, lwd = 1, lty = 1, col = "grey")
lines(v_ssm[5,] ~ rlm_seq, lwd = 1, lty = 1, col = "grey")
mesh8_perim = 8 * 2* 25.4  
mesh6_perim = 6 * 2 * 25.4

axis_i = seq(1, length(rlm_seq), 150)

axis(side = 1, at = rlm_seq[axis_i], labels = round(rlm_seq[axis_i] * mesh8_perim, -1))
axis(side = 3, at = rlm_seq[axis_i], labels = round(rlm_seq[axis_i] * mesh6_perim, -1), col.axis = "grey60", col.ticks = "grey60")
mtext(side = 3, "METF (mm) w/6 in. Mesh", line = 1, col = "grey60", font = 1, cex = 0.8)
segments(par("usr")[1], par("usr")[4], par("usr")[2], par("usr")[4],col = "grey60", xpd = T)
mtext(side = 1, "METF (mm) w/8 in. Mesh", line = 1, font = 1, cex = 0.8)
legend("topright", legend = c("SSM", "Yukon R. Sonar", "Kuskokwim R. Sonar"), lty = c(1,2,3), lwd = c(4,1,1.4), bty = "n", cex = 0.75)
dev.off()

##### AGE/SEX COMPOSITION COMPARISONS #####
q_esc = array_format(post_summ(post_list[["E-ASL"]], "q_esc")["50%",])
q_com = array_format(post_summ(post_list[["E-ASL"]], "q_com")["50%",])
q_sub = array_format(post_summ(post_list[["E-ASL"]], "q_sub")["50%",])

file_device(file.path(fig_dir, paste0("age-comp.", file_type)), h = 5, w = 3.45)
par(mfcol = c(4,2), mar = c(0.25,0.25,0.25,0.25), oma = c(2.5,4.6,1.5,2),
    tcl = -0.20, mgp = c(2,0.25,0), lend = "square", ljoin = "mitre")
yaxis_side = rep(c(2,4), each = 4)
xaxis_draw = rep(c(F,F,F,T), 2)
legend_draw = c(T, rep(F, 7))
upper_text_draw = rep(c(T, F, F, F), 2)
right_text_draw = rep(c(T, F), each = 4)
right_text = c(paste0("Age ", 4:7), rep("", 4))

for (as in 1:8) {
  plot(1,1, type = "n", xlim = range(years), las = 1,
       ylim = c(0, max(cbind(q_com[,as], q_sub[,as], q_esc[,as]), 0.05)),
       xaxt = "n", yaxt = "n")
  
  usr = par("usr")
  rect(usr[1], usr[3], 1984, usr[4], col = "grey90")
  rect(2014, usr[3], usr[2], usr[4], col = "grey90")
  abline(v = c(1984, 2014), col = "grey50")
  lines(q_esc[,as] ~ years, lty = 1, col = "black")
  lines(q_com[,as] ~ years, lty = 3, col = "black", lwd = 2)
  lines(q_sub[,as] ~ years, lty = 2, col = "black")
  
  at_y = axisTicks(par("usr")[3:4], log = F, nint = 3)
  axis(side = yaxis_side[as], at = at_y, labels = paste0(at_y * 100, "%"), las = 2)
  
  if (xaxis_draw[as]) {
    at_long = years[years %in% seq(1980, 2010, 10)]
    at_short = years[years %in% seq(1976, 2018, 2)]
    axis(side = 1, at = at_long, labels = substr(at_long, 3, 4), tcl = -0.35)
    axis(side = 1, at = at_short, labels = F)
  }
  if (legend_draw[as]) {
    legend("top", horiz = T, x.intersp = c(0.5, 0.2, 0.2), seg.len = c(1.5,2.5,2) ,legend = c("Esc", "Com", "Sub"), title = "Source", 
           lty = c(1,3,2), lwd = c(1,2,1), cex = 0.7, box.col = "black", bg = "white")
  }
  box(lwd = 1)
  if (upper_text_draw[as]) {
    mtext(side = 3, line = 0, text = ifelse(as == 1, "Female", "Male"), xpd = T, cex = 0.8)
  }
  if (right_text_draw[as]) {
    mtext(side = 2, line = 2.4, text = right_text[as], xpd = T, cex = 0.8)
  }
}

mtext(side = 1, outer = T, line = 1.5, "Year")
mtext(side = 2, outer = T, line = 3.4, "Percent Contribution")
dev.off()

##### RETURN AT AGE PLOT #####
pi = array_format(post_summ(post_list[["E-ASL"]], "pi")[3,])
pi_lwr = array_format(post_summ(post_list[["E-ASL"]], "pi")[4,])
pi_upr = array_format(post_summ(post_list[["E-ASL"]], "pi")[5,])

lab_x = floor(seq(1970, 2014, length = 4))

file_device(file.path(fig_dir, paste0("pi-trends.", file_type)), h = 5, w = 3.45)
par(mfrow = c(2,1), mar = c(1,1.25,0.5,0.5),  oma = c(1.1,1,0,0), tcl = -0.15, mgp = c(2,0.2,0), cex.axis = 0.75, lend = "square")

cols = c("black", "grey40", "grey60", "grey75")
by = 1969:2015
sex = c("Female", "Male")
letter = c("(a)", "(b)")

for (s in 1:2) {
  plot(1,1, type = "n", xlim = range(by), ylim = c(-0.025,1), las = 1, yaxt = "n", xaxt = "n", xlab = "", ylab = "")
  for (a in 1:na) {
    polygon(c(by, rev(by)), c(pi_lwr[,a,s], rev(pi_upr[,a,s])), col = scales::alpha("grey30", 0.15), border = NA)
  }
  for (a in 1:na) {
    lines(pi[,a,s] ~ by, col = cols[a], lwd = 2)
    text(x = lab_x[a], y = pi[which(by == lab_x[a]),a,s] + 0.035, labels = ages[a], xpd = T, col = cols[a])
  }
  
  usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
  text(x = usr[1] - xdiff * 0.015, usr[4] - ydiff * 0.05, labels = letter[s], pos = 4, font = 2, cex = 0.9)
  text(x = usr[1] + xdiff * 0.06, usr[4] - ydiff * 0.055, labels = sex[s], pos = 4, font = 1, cex = 0.8)
  
  axis(side = 2, at = seq(0, 1, 0.2), labels = format(seq(0, 1, 0.2), nsmall = 1), las = 2)
  axis(side = 1, at = seq(1970, 2010, 10), labels = substr(seq(1970, 2010, 10), 3, 4), tcl = -0.35)
  axis(side = 1, at = seq(1970, 2014, 2), labels = F, tcl = -0.2)
  
  box()
}
mtext(side = 2, line = 0.2, outer = TRUE, "Probability of Return-at-Age", cex = 0.9)

mtext(side = 1, outer = T, line = 0.25, "Brood Year", cex = 0.9)
dev.off()

##### EGG NUMBER AND MASS RELATIONSHIPS AND CHANGES #####
egg_fun = function(x) {0.0009345 * x^2.363}
mass_fun = function(x) {8.71e-12 * x^4.829}

ldat = rlm[,,1,1] * 8 * 2* 25.4 

f10 = 1:10
l10 = (nt - 9):nt
set.seed(1234)
out = replicate(100, {
  lkeep = sample(l10, replace = T)
  fkeep = sample(f10, replace = T)
  rbind(
    (colMeans(ldat[lkeep,]) - colMeans(ldat[fkeep,]))/(colMeans(ldat[fkeep,])),
    (egg_fun(colMeans(ldat[lkeep,])) - egg_fun(colMeans(ldat[fkeep,])))/egg_fun(colMeans(ldat[fkeep,])),
    (mass_fun(colMeans(ldat[lkeep,])) - mass_fun(colMeans(ldat[fkeep,])))/mass_fun(colMeans(ldat[fkeep,]))
  )
})

lwr = upr = matrix(NA, 3, 4)

for (i in 1:3) {
  for (j in 1:4) {
    lwr[i,j] = quantile(out[i,j,], 0.025)
    upr[i,j] = quantile(out[i,j,], 0.975)
  }
}

med = rbind(
  (colMeans(ldat[l10,]) - colMeans(ldat[f10,]))/(colMeans(ldat[f10,])),
  (egg_fun(colMeans(ldat[l10,])) - egg_fun(colMeans(ldat[f10,])))/egg_fun(colMeans(ldat[f10,])),
  (mass_fun(colMeans(ldat[l10,])) - mass_fun(colMeans(ldat[f10,])))/mass_fun(colMeans(ldat[f10,]))
)

file_device(file.path(fig_dir, paste0("z-figure.", file_type)), h = 6, w = 3.5)

par(mfrow = c(2,1), mar = c(2.5,3.75,0.5,0.5), mgp = c(1.5,0.35,0), tcl = -0.25, lend = "square")

pred_x = seq(500, mean(ldat[f10,4]), length = 100)
pred_egg = egg_fun(pred_x)/egg_fun(mean(ldat[f10,4]))
pred_mass = mass_fun(pred_x)/mass_fun(mean(ldat[f10,4]))

ff = egg_fun(colMeans(ldat[f10,]))
lf = egg_fun(colMeans(ldat[l10,]))
fm = mass_fun(colMeans(ldat[f10,]))
lm = mass_fun(colMeans(ldat[l10,]))

(colMeans(ldat[l10,]) - colMeans(ldat[f10,]))/colMeans(ldat[f10,])
(lf - ff)/ff
(lm - fm)/fm

(ff[1:3])/ff[4]
(fm[1:3])/fm[4]


plot(pred_egg ~ pred_x, type = "l", ylim = range(0, pred_egg, pred_mass),
     xlab = "METF (mm)", ylab = "", yaxt = "n", lwd = 2)
lines(pred_mass ~ pred_x, type = "l", lty = 2, lwd = 2)
axis(side = 2, at = seq(0, 1.2, 0.2), labels = paste0(seq(0, 1.2, 0.2) * 100, "%"), las = 2)
mtext(side = 2, "Relative Reproductive Output", line = 2.5)
points(rep(0, 4) ~ colMeans(ldat[f10,]), pch = c("4", "5", "6", "7"), col = "grey60")
points(rep(0, 4) ~ colMeans(ldat[l10,]), pch = c("4", "5", "6", "7"))
usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
text(usr[1] - 0.025 * xdiff, usr[4] - ydiff * 0.05, pos = 4, labels = "(a)", font = 2)

segments(colMeans(ldat[f10,]), rep(0.035, 4), colMeans(ldat[f10,]), egg_fun(colMeans(ldat[f10,]))/egg_fun(mean(ldat[f10,4])), col = "grey60")
segments(colMeans(ldat[l10,]), rep(0.035, 4), colMeans(ldat[l10,]), egg_fun(colMeans(ldat[l10,]))/egg_fun(mean(ldat[f10,4])))

points(colMeans(ldat[f10,]), egg_fun(colMeans(ldat[f10,]))/egg_fun(mean(ldat[f10,4])), col = "grey60", pch = 22, bg = "grey60")
points(colMeans(ldat[l10,]), egg_fun(colMeans(ldat[l10,]))/egg_fun(mean(ldat[f10,4])), col = "black", pch = 22, bg = "black")
points(colMeans(ldat[f10,]), mass_fun(colMeans(ldat[f10,]))/mass_fun(mean(ldat[f10,4])), col = "grey60", pch = 24, bg = "grey60")
points(colMeans(ldat[l10,]), mass_fun(colMeans(ldat[l10,]))/mass_fun(mean(ldat[f10,4])), col = "black", pch = 24, bg = "black")

legend(x = usr[1], y = usr[4] - ydiff * 0.075, seg.len = c(2.5), legend = c("Egg #", "Egg Mass", "First 10 Yrs", "Last 10 Yrs"), 
       lty = c(1,2,NA,NA), pch = c(22, 24, NA, NA), pt.cex = 1.1, pt.bg = c("white", "white", NA, NA), text.col = c("black", "black", "grey60", "black"), bty = "n", cex = 0.8)


mp = barplot(med, beside = T, ylim = c(-0.5,0.8), col = "white",
             border = "white", yaxt = "n", xlab = "Age")
abline(h = 0, lty = 1)
segments(mp, lwr, mp, upr)
points(med[1,] ~ mp[1,], bg = "grey60", pch = 21)
points(med[2,] ~ mp[2,], bg = "grey60", pch = 22)
points(med[3,] ~ mp[3,], bg = "grey60", pch = 24)
axis(side = 1, at = mp[2,], labels = 4:7)
axis(side = 2, at = seq(-0.4, 1, by = 0.2),
     paste0(labels = seq(-0.4, 1, 0.2) * 100, "%"), las = 2)
mtext(side = 2, "% Change", line = 2.5)
legend("topright", legend = c("METF", "Egg #", "Egg Mass"), pt.cex = 1.1,
       pch = c(21,22,24), pt.bg = "grey60", bty = "n", cex = 0.8)
usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
text(usr[1] - 0.025 * xdiff, usr[4] - ydiff * 0.05, pos = 4, labels = "(b)", font = 2)

box()

dev.off()

round(egg_fun(colMeans(ldat[f10,]))/egg_fun(mean(ldat[f10,4])), 2)
round(mass_fun(colMeans(ldat[f10,]))/mass_fun(mean(ldat[f10,4])), 2)

##### PER CAPITA REPRODUCTIVE OUTPUT #####

# create time blocks: try to break time series into thirds
early_t = 1:15
late_t = (nt - 14):nt
block = ifelse(years %in% years[early_t], 1, 
       ifelse(years %in% years[late_t], 3, 2))
table(block)
sapply(1:3, function(x) range(years[block == x]))


f = function(post) {
  Z = post_subset(post, "^Z_per_S_t[", T)
  
  out = t(sapply(1:post_dim(post, "saved"), function(i) {
    tapply(Z[i,]/mean(Z[i,]), block, mean)
  }))
  
  apply(out, 2, summ, p = c(0.025, 0.25, 0.5, 0.75, 0.975))
}

keep_mods = c("E-0", "EM-0",
              "E-L", "E-A", "E-S",
              "E-AL", "E-AS", "E-SL",
              "E-ASL", "EM-ASL")

out = lapply(post_list[keep_mods], f)


meds = sapply(out, function(x) x["50%",])
lwrs1 = sapply(out, function(x) x["2.5%",])
lwrs2 = sapply(out, function(x) x["25%",])
uprs1 = sapply(out, function(x) x["97.5%",])
uprs2 = sapply(out, function(x) x["75%",])

round((meds[3,] - meds[1,])/meds[1,], 2)


file_device(file.path(fig_dir, paste0("z-percapita.", file_type)), h = 3.75, w = 3.5)
par(xaxs = "i", yaxs = "i", mar = c(4,3,0.5,0.5), tcl = -0.25, mgp = c(2,0.35,0))
mp = barplot(meds, las = 2,
             # ylim = 1 + (max(abs(rbind(lwrs1, uprs1))) - 1) * c(-1,1),
             ylim = 1 + (max(abs(range(meds) - 1)) * c(-1.5,1.5)),
             beside = T, col = "white", border = "white",
             xlim = c(0.5,40.5),
             ylab = "Scaled Per capita Reproductive Output")
usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
abline(h = 1, lty = 2)
at_v = (mp[1,1:(length(keep_mods) - 1)] + mp[3,2:length(keep_mods)])/2
zero_break = 2; one_break = 5; two_break = 8
abline(v = at_v, col = "grey60")

# rect(xleft = usr[1], ybottom = usr[4] - ydiff * 0.1, xright = usr[2], ytop = usr[4], border = "white", col = "white")
rect(xleft = usr[1], ybottom = usr[3], xright = at_v[zero_break], ytop = usr[4], lwd = 2, xpd = T)
rect(xleft = at_v[zero_break], ybottom = usr[3], xright = at_v[one_break], ytop = usr[4], lwd = 2, xpd = T)
rect(xleft = at_v[one_break], ybottom = usr[3], xright = at_v[two_break], ytop = usr[4], lwd = 2, xpd = T)
rect(xleft = at_v[two_break], ybottom = usr[3], xright = usr[2], ytop = usr[4], lwd = 2, xpd = T)
axis(side = 1, at = mp[2,], labels = F)
segments(mp[1,], lwrs1[1,], mp[1,], uprs1[1,])
segments(mp[2,], lwrs1[2,], mp[2,], uprs1[2,])
segments(mp[3,], lwrs1[3,], mp[3,], uprs1[3,])
points(meds[1,] ~ mp[1,], pch = 21, bg = "grey60", cex = 0.8)
points(meds[2,] ~ mp[2,], pch = 22, bg = "grey60", cex = 0.8)
points(meds[3,] ~ mp[3,], pch = 24, bg = "grey60", cex = 0.8)
# segments(mp[1,], lwrs2[1,], mp[1,], uprs2[1,], lwd = 4)
# segments(mp[2,], lwrs2[2,], mp[2,], uprs2[2,], lwd = 4)
# segments(mp[3,], lwrs2[3,], mp[3,], uprs2[3,], lwd = 4)
lab1 = paste(paste0("'", substr(range(years[block == 1]), 3, 4)), collapse = "-")
lab2 = paste(paste0("'", substr(range(years[block == 2]), 3, 4)), collapse = "-")
lab3 = paste(paste0("'", substr(range(years[block == 3]), 3, 4)), collapse = "-")
legend("top", x.intersp = 0.5, horiz = T, legend = c(lab1, lab2, lab3), pch = c(21, 22, 24), pt.bg = "grey60", pt.cex = 1, cex = 0.75, bg = "white", box.col = "black")
box(lwd = 2)

dev.off()

## % declines in per capita output (for main-text results):
round((meds[3,"EM-ASL"] - meds[1,"EM-ASL"])/meds[1,"EM-ASL"], 2)
round((meds[3,"E-ASL"] - meds[1,"E-ASL"])/meds[1,"E-ASL"], 2)
round((meds[3,"E-S"] - meds[1,"E-S"])/meds[1,"E-S"], 2)
round((meds[3,"E-L"] - meds[1,"E-L"])/meds[1,"E-L"], 2)
round((meds[3,"E-A"] - meds[1,"E-A"])/meds[1,"E-A"], 2)

##### MSC FIGURE #####

# function to create the plot comparing time periods and models for a given eq. quantity and vuln type
msy_plot = function(keep_val = "S", keep_vuln = "mesh8", keep_mods, xticklabs = F, legend = F, letter) {
  # extract the equilibrium info requested
  keep = msy[,keep_val,keep_vuln,,keep_mods]
  ylwr = ifelse(keep_val == "S", 40000, 75000)
  yupr = ifelse(keep_val == "S", 180000, 325000)
  
  # create the basic plot: dimensions and spacing
  mp = barplot(keep["50%",,keep_mods], yaxt = "n", xaxt = "n", col = "white", border = "white", las = 2,
               beside = T, 
               ylim = range(msy[c("10%", "90%"),keep_val,,,keep_mods]) * c(0.95, 1.1),
               xlim = c(0.5, 20.5) 
               # , ylim = c(ylwr, yupr)
  )
  
  # extract user coordinates
  usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
  
  # locations to draw vertical lines separating models
  at_v = (mp[1,1:(length(keep_mods) - 1)] + mp[3,2:length(keep_mods)])/2
  abline(v = at_v, col = "grey60")
  
  # locations of breaks: zero trends vs. one vs. two trends
  zero_break = 3; one_break = 6; two_break = 9
  
  # determine locations and labels of tick marks for y-axes
  at_y1 = axisTicks(usr[3:4], log = F, nint = 4)
  at_y1_2 = seq(0, 500000, 10000)
  lab_y1 = at_y1/1000
  
  lab_y2 = seq(50, 250, 50)
  at_y2 = keep["50%","all","N-0"] * (lab_y2/100)
  
  # draw horizontal line at no change from N-0 model
  abline(h = at_y2[lab_y2 == 100], lty = 2)
  
  # draw y-axes
  axis(side = 2, at = at_y1, labels = lab_y1, las = 2)
  axis(side = 2, at = at_y1_2, labels = F, tcl = -0.125)
  axis(side = 4, at = at_y2, labels = paste0(lab_y2 - 100, "%"), las = 2)
  
  # draw thick line to separate zero vs. three trend models
  abline(v = at_v[zero_break], lwd = 4, xpd = F, col = "grey60")
  
  # draw credible intervals: central 80%
  segments(mp[1,], keep["10%","early",keep_mods], mp[1,], keep["90%","early",keep_mods])
  segments(mp[2,], keep["10%","all",keep_mods], mp[2,], keep["90%","all",keep_mods])
  segments(mp[3,], keep["10%","late",keep_mods], mp[3,], keep["90%","late",keep_mods])
  
  # draw credible intervals: central 50%
  segments(mp[1,], keep["25%","early",keep_mods], mp[1,], keep["75%","early",keep_mods], lwd = 4)
  segments(mp[2,], keep["25%","all",keep_mods], mp[2,], keep["75%","all",keep_mods], lwd = 4)
  segments(mp[3,], keep["25%","late",keep_mods], mp[3,], keep["75%","late",keep_mods], lwd = 4)
  
  # draw medians
  points(keep["50%","early",keep_mods] ~ mp[1,], pch = 21, bg = "grey60", cex = 1.2)
  points(keep["50%","all",keep_mods] ~ mp[2,], pch = 22, bg = "grey60", cex = 1.2)
  points(keep["50%","late",keep_mods] ~ mp[3,], pch = 24, bg = "grey60", cex = 1.2)
  
  # add a legend if requested
  if (legend) {
    legend("topright", y.intersp = 0.75, legend = c("First 10 years", "All years", "Last 10 years"), title = "Demography",
           pch = c(21,22,24), pt.bg = "grey60", pt.cex = 1, cex = 0.7, bg = "white", box.col = "white")
  }
  
  # draw x-axis
  if (xticklabs) {
    axis(side = 1, at = mp[2,], labels = keep_mods, las = 2)
  } else {
    axis(side = 1, at = mp[2,], labels = F, las = 2)
  }
  
  # draw the label identifying each plot
  rect(usr[1], usr[4] - ydiff * 0.11, usr[1] + xdiff * 0.35, usr[4], border = "white", col = "white")
  text(x = usr[1] + xdiff * -0.025, y = usr[4] - ydiff * 0.065,
       labels = ifelse(keep_vuln == "mesh8", paste0("(", letter, ") 8 in. mesh"),
                       ifelse(keep_vuln == "flat", paste0("(", letter, ") No selectivity"), paste0("(", letter, ") 6 in. mesh"))), pos = 4, cex = 0.7, font = 2)
  
  # add a main title over top panel
  mtext(side = 3, ifelse(keep_vuln == "mesh8", TeX(paste0(keep_val, "_{MSC}")), ""), line = 0, font = 2)
  
  # draw a border
  box()
}

# which models to keep for plot
keep_mods = c(
  "N-0",
  "E-0", "EM-0",
  # "E-L", "E-A", "E-S",
  # "E-AL", "E-AS", "E-SL",
  "E-ASL", "EM-ASL")

# make the plot itself
file_device(file.path(fig_dir, paste0("msc.", file_type)), h = 5.5, w = 7.2)
par(mfcol = c(3,2), xaxs = "i", yaxs = "i", cex = 1, lend = "square", mar = c(0,1.75,0.75,2.25),
    oma = c(3.5,0.75,0.5,2), tcl = -0.25, mgp = c(2,0.35,0), cex.axis = 0.9)
msy_plot("S", "mesh8", keep_mods, legend = F, letter = "a")
msy_plot("S", "flat", keep_mods, legend = F, letter = "c")
msy_plot("S", "mesh6", keep_mods, legend = T, xticklabs = T, letter = "e"); par(mar = c(0,2.25,0.75,1.75))
msy_plot("H", "mesh8", keep_mods, legend = F, letter = "b")
msy_plot("H", "flat", keep_mods, legend = F, letter = "d")
msy_plot("H", "mesh6", keep_mods, xticklabs = T, letter = "f")
mtext(side = 2, outer = T, "Escapement or Harvest (1000s)", line = -0.2)
mtext(side = 4, outer = T, "% Change from Model N-0", line = 0.75)
dev.off()

keep_val = "H"
keep_vuln = "mesh6"
keep_mods = c("N-0", "E-0", "EM-0", "E-A", "E-S", "E-L", "E-AS", "E-AL", "E-SL", "E-ASL", "EM-ASL")

x = msy["50%", keep_val, keep_vuln, "all", keep_mods]
round(x, -3)
p = round((x - x[1])/(x[1]), 2)
range(p[-1])

early = msy["50%", "H", "mesh6", "early", keep_mods]
late = msy["50%", "H", "mesh6", "late", keep_mods]

msy["50%","p_female","mesh6", c("early","all", "late"),keep_mods]
msy["50%","p_female","flat", c("early","all", "late"), keep_mods]

round(((late - early)/early) * 100, 2)
##### CONVERGENCE SUMMARIES #####

# diag_nodes = c("alpha", "beta_e10", "^R[", "delta_0",
#                "delta_1", "gamma_0", "gamma_1",
#                "phi", "sigma_R", "sigma_R0",
#                "Fcom", "Fsub", "^Vtau$", "^Vsig$", "^Vtha$", "^Vlam$", "log_mean_R0"
# )
# 
# match_params(post_list[["E-ASL"]], diag_nodes)
# 
# f = function(post) {
#   out = t(post_summ(post, diag_nodes, neff = T, Rhat = T)[c("Rhat", "neff"),])
#   out = out[order(out[,1], decreasing = T),]
#   out = data.frame(out)
#   out = cbind(param = rownames(out), out); rownames(out) = NULL
#   out
# }
# 
# out = lapply(post_list, f)
# min_bound = 700
# max_bound = 20000
# f = function(z) {
#   z$base_name = postpack:::drop_index(z$param)
#   z$ess[is.na(z$Rhat)] = NA
#   tapply(z$neff, z$base_name, function(x) sum(x < max_bound & x > min_bound, na.rm = T))
# }
# 
# z = sapply(out, f)
# z[z %in% c("-Inf", "Inf")] = NA
# z
# 
# lapply(out, function(x) x[str_detect(x$param, "V"),"neff"])
# 
# out
# above_min = z > 0
# below_max = z < 10
# 
# bad_neff = apply(above_min & below_max, 2, function(x) names(which(x)))
# bad_neff = unique(unlist(bad_neff)); bad_neff
# 
# apply(z[bad_neff,], 2, max, na.rm = T)
# 
# f = function(z) {
#   z$base_name = postpack:::drop_index(z$param)
#   # z$Rhat[is.na(z$Rhat)] = NA
#   tapply(z$Rhat, z$base_name, max, na.rm = T)
# }
# z = sapply(out, f)
# z[z %in% c("-Inf", "Inf")] = NA
# z > 1.05
# 
# bad_alpha = z["alpha",] > 1.05
# sort(colnames(z)[bad_alpha])
# mean(z["alpha",bad_alpha])
# 
# mean(apply(z[-1,], 2, max, na.rm = T))
# 
# z

##### POSTERIOR PREDICTIVE CHECKS #####

# THIS TAKES A COUPLE MINUTES TO RUN
# ESPECIALLY FOR THE COMPOSITION SECTION

## ABUNDANCE

# f = function(post) {
#   pp = lnorm_pp_check(post_subset(post, "^S_t[", T), S_obs_sig, S_obs)
#   with(pp, mean(fit_obs > fit_new))
# }
# ESC_out = sapply(post_list, f)
# 
# f = function(post) {
#   pp = lnorm_pp_check(post_subset(post, "^Hcom[", T), Hcom_obs_sig, Hcom_obs)
#   with(pp, mean(fit_obs > fit_new))
# }
# COM_out = sapply(post_list, f)
# 
# f = function(post) {
#   pp = lnorm_pp_check(post_subset(post, "^Hsub[", T), Hsub_obs_sig, Hsub_obs)
#   with(pp, mean(fit_obs > fit_new))
# }
# SUB_out = sapply(post_list, f)
# 
# range(round(ESC_out, 2))
# range(round(SUB_out, 2))
# range(round(COM_out, 2))
# 
# COMPOSIITON
# # discard half the samples before calculating predictive checks
# thin = 0.5
# 
# f = function(post) {
#   pp = mult_pp_check(post_subset(post_thin(post, thin), "^q_esc[", T), x_esc, progress = T)
#   with(pp, mean(fit_obs > fit_new))
# }
# esc_out = sapply(post_list, f)
# 
# f = function(post) {
#   pp = mult_pp_check(post_subset(post_thin(post, thin), "^q_com[", T), x_com, progress = T)
#   with(pp, mean(fit_obs > fit_new))
# }
# com_out = sapply(post_list, f)
# 
# f = function(post) {
#   pp = mult_pp_check(post_subset(post_thin(post, thin), "^q_sub[", T), x_sub, progress = T)
#   with(pp, mean(fit_obs > fit_new))
# }
# sub_out = sapply(post_list, f)
# 
# range(round(esc_out, 2))
# range(round(com_out, 2))
# range(round(sub_out, 2))
# range(round(esc_out[a_mods], 2))
# range(round(com_out[a_mods], 2))
# range(round(sub_out[a_mods], 2))

##### NUMBERS FOR IN-TEXT #####

# extract tau: RLM of fully selected fish
tau_est = sapply(post_list, function(post) post_summ(post, "^Vtau$", digits = 2)["50%",])


mesh8_perim = 8 * 2 * 25.4  
mesh6_perim = 6 * 2 * 25.4

kusko_mesh8 = mean(tau_est) * mesh8_perim
kusko_mesh6 = mean(tau_est) * mesh6_perim
yukon_mesh8 = Ytau_est * mesh8_perim
yukon_mesh6 = Ytau_est * mesh6_perim

kusko_mesh8 - yukon_mesh8
kusko_mesh6 - yukon_mesh6

keep_mod = "E-ASL"

# probability of returning as female in all years under no-sex trend models
sapply(post_list["E-0"], function(post) {
  post_samps = post_subset(post, "delta_0", T)
  summ(expit(post_samps), rnd = 2)
})

# probability of returning as female in first year under sex trend models
sapply(post_list[keep_mod], function(post) {
  post_samps = post_summ(post, "psi[1]", digits = 2)
})

# probability of returning as female in last year under sex trend models
sapply(post_list[keep_mod], function(post) {
  post_samps = post_summ(post, "psi[47]", digits = 2)
})

# median coefficient of delta_1
sapply(post_list[keep_mod], function(post) {
  post_samps = post_summ(post, "delta_1", digits = 3)
})

# odds ratio of female
sapply(post_list[keep_mod], function(post) {
  post_samps = post_subset(post, "delta_1", T)
  summ(exp(post_samps), rnd = 3)
})

# female return probability at age 6 in first and last brood years
post_summ(post_list[[keep_mod]], c("pi[1,3,1]", "pi[47,3,1]"), digits = 2)

# female return probability at age 5 in first and last brood years
post_summ(post_list[[keep_mod]], c("pi[1,2,1]", "pi[47,2,1]"), digits = 2)

# male return probability at age 4 in first and last brood years
post_summ(post_list[[keep_mod]], c("pi[1,1,2]", "pi[47,1,2]"), digits = 2)

# male return probability at age 6 in first and last brood years
post_summ(post_list[[keep_mod]], c("pi[1,3,2]", "pi[47,3,2]"), digits = 2)

gamma_1 = post_summ(post_list[[keep_mod]], "gamma_1", digits = 3)
array_format(gamma_1[4,])
array_format(gamma_1[5,])

med_msy_all = msy["50%", "S", "mesh8", "all",]
range(round((med_msy_all[E_mods | EM_mods] - med_msy_all["N-0"])/med_msy_all["N-0"], 2))

med_msy_all = msy["50%", "H", "mesh8", "all",]
range(round((med_msy_all[E_mods | EM_mods] - med_msy_all["N-0"])/med_msy_all["N-0"], 2))

med_msy = msy["50%", "H", "mesh8", "late",]
range(round((med_msy[E_mods | EM_mods] - med_msy["N-0"])/med_msy["N-0"], 2))

med_msy = msy["50%", "S", "mesh6", "all",]
range(round((med_msy[E_mods | EM_mods] - med_msy["N-0"])/med_msy["N-0"], 2))

med_msy = msy["50%", "H", "mesh6", "all",]
range(round((med_msy[E_mods | EM_mods] - med_msy["N-0"])/med_msy["N-0"], 2))


med_msy_early = msy["50%", "H", "mesh6", "early",]
med_msy_late = msy["50%", "H", "mesh6", "late",]

round(((med_msy_late - med_msy_early)/med_msy_early) * 100)

q = dimnames(msy)[[2]]
msy["50%",str_detect(q, "^U"),"mesh6","all",]
