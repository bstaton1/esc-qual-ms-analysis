
rm(list = ls(all = T))

library(StatonMisc)
library(postpack)
library(abind)
library(stringr)

##### SESSION SETUP #####

data_dir = "../2-model-fit/inputs/"
model = 1  # just needed to build the data

source("../2-model-fit/1-compile-data.R")
source("../new-func-source.R")
rm(model) # clear out the model object 

out_dir = "../../model-output-no-rand-age/"
out_files = dir(out_dir, full.names = T)

# HOW DO YOU WANT TO SAVE THE OUTPUT
# file_type = "pdf"
file_type = "jpeg"
fig_dir = "ms-figs"

# create directory to store output figures if it doesn't exist
if (!dir.exists(fig_dir)) dir.create(fig_dir)

# device-general function
my_device = function(name, file_type, h, w, ppi = 600) {
  file.type = tolower(file_type)
  
  # build the code to call the appropriate device
  file = paste(name, ifelse(file_type == "jpeg", "jpg", file_type), sep = ".")
  if (file_type != "pdf") {
    text = paste0(file_type, "('", file, "', h = ", ppi, " * ", h, ", w = ", ppi, " * ", w, ", res = ", ppi, ")")
  } else {
    text = paste0(file_type, "('", file, "', h = ", h, ", w = ", w, ")")
  }
  
  # call the function to generate the text
  eval(parse(text = text))
}

# assign a model identifier based the saved meta data
id_model = function(meta, include_rand_age = F) {
  with(meta, {
    
    unit = ifelse(z_unit == "fish_count", "N",
                  ifelse(z_unit == "egg_count", "E", "EM"))
    
    A = ifelse(age_trend, "A", "")
    S = ifelse(sex_trend, "S", "")
    L = ifelse(len_trend, "L", "")
    trends = paste0(A, S, L, collapse = "")
    trends = ifelse(trends == "", "0", trends)
    
    id = paste(unit, trends, sep = "-")
    
    if (include_rand_age) {
      d = ifelse(rand_age, "D", "M")
      id = paste(id, d, sep = "-")
    }
    id
  })
}

# create empty objects to store the output from each model
meta = list()
post_list = list()
msy = NULL

# the file names of the output files
postfiles = out_files[str_detect(out_files, "post")]
mods = str_extract(postfiles, "[0-9]+")
metafiles = out_files[str_detect(out_files, "meta")]
msyfiles = out_files[str_detect(out_files, "msy")]

# read in the posterior samples and meta data
for (i in 1:length(mods)) {
  progress_updater(i, length(mods))
  post_list[[i]] = readRDS(postfiles[i])
  meta[[i]] = readRDS(metafiles[i])
}

# create the ids for each model
ids = unlist(lapply(meta, id_model, include_rand_age = F))

# read in the msy equilibrium quantities
msy = readRDS(msyfiles[1])
for (i in 2:length(mods)) {
  msy = abind(msy, readRDS(msyfiles[i]), along = 5)
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

v_f_unr = abind(early = summ_v(early_t, 1, 1), all =  summ_v(all_t, 1, 1), late = summ_v(late_t, 1, 1), along = 3)
v_m_unr = abind(early = summ_v(early_t, 2, 1), all =  summ_v(all_t, 2, 1), late = summ_v(late_t, 2, 1), along = 3)
v_f_res = abind(early = summ_v(early_t, 1, 2), all =  summ_v(all_t, 1, 2), late = summ_v(late_t, 1, 2), along = 3)
v_m_res = abind(early = summ_v(early_t, 2, 2), all =  summ_v(all_t, 2, 2), late = summ_v(late_t, 2, 2), along = 3)

x_t = seq(0.3, 0.1, -0.1)
xf = a_min:a_max - 0.075
xm = a_min:a_max + 0.075

my_device(file.path(fig_dir, "v-age"), file_type, h = 6, w = 3.4)
par(mfrow = c(2,1), mar = c(1,2,0.5,0.5),  oma = c(1.5,1,0,0), tcl = -0.25, mgp = c(2,0.4,0))
plot(1,1, ylim = c(0,1), xlim = range(min(xf) - max(x_t), max(xm) + max(x_t)),
     col = "red", pch = 16, type = "o",xaxt = "n", las = 2)
col = c("black", "grey", "white")

for (t in (1:3)) {
  lines(xf - x_t[t], v_f_unr[,"mean",t], type = "o", pch = 21, bg = col[t], cex = 1.4)
  lines(xm + rev(x_t)[t], v_m_unr[,"mean",t], type = "o", pch = 23, bg = col[t], lty = 2, cex = 1.4)
}

legend("bottomright", c("Female", "Male", "First 10 Yrs", "All Yrs", "Last 10 Yrs"), box.col = "white", bg = "grey90", x.intersp = 0.5, seg.len = 3,
       lty = c(1, 2, NA, NA, NA), pch = c(21, 23, 22, 22, 22), pt.bg = c("white", "white", col), pt.cex = 1.4, cex = 0.7)
box()
axis(side = 1, at = 4:7, labels = 4:7)
usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
text(x = usr[1] - xdiff * 0.015, usr[3] + ydiff * 0.05, labels = "(a)", pos = 4, font = 2, cex = 1)
text(x = usr[1] + xdiff * 0.075, usr[3] + ydiff * 0.04, labels = "8 in. Mesh", pos = 4, font = 1, cex = 0.8)

plot(1,1, ylim = c(0,1), xlim = range(min(xf) - max(x_t), max(xm) + max(x_t)),
     col = "red", pch = 16, type = "o",xaxt = "n", las = 2)

for (t in (1:3)) {
  lines(xf - x_t[t], v_f_res[,"mean",t], type = "o", pch = 21, bg = col[t], cex = 1.4)
  lines(xm + rev(x_t)[t], v_m_res[,"mean",t], type = "o", pch = 23, bg = col[t], lty = 2, cex = 1.4)
}

axis(side = 1, at = 4:7, labels = 4:7)
text(x = usr[1] - xdiff * 0.015, usr[3] + ydiff * 0.05, labels = "(b)", pos = 4, font = 2, cex = 1)
text(x = usr[1] + xdiff * 0.075, usr[3] + ydiff * 0.04, labels = "6 in. Mesh", pos = 4, font = 1, cex = 0.8)
usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])

mtext(side = 1, line = 0.5, outer = T, "Age")
mtext(side = 2, line = 0.0, outer = T, "Selectivity")
dev.off()

##### SELECTIVITY PLOT: FULL SHAPE #####

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

kusko_ests = post_subset(post_list[["E-ASL"]], "^V...$", T)
yukon_ests = post_subset(post_list[["E-ASL"]], "^V..._yukon$", T)

kusko_pearson = function(rlm, i) {
  pearson(
    rlm, 
    tau = kusko_ests[i,1], sigma = kusko_ests[i,2], theta = kusko_ests[i,3], lambda = kusko_ests[i,4]
  )
}

yukon_pearson = function(rlm, i) {
  pearson(
    rlm, 
    tau = yukon_ests[i,1], sigma = yukon_ests[i,2], theta = yukon_ests[i,3], lambda = yukon_ests[i,4]
  )
}


rlm_seq = seq(min(rlm), max(rlm), length = 1000)

out = t(sapply(1:post_dim(post_list[["E-ASL"]], "saved"), function(i) kusko_pearson(rlm_seq, i)))
v_kusko = apply(out, 2, summ)
out = t(sapply(1:post_dim(post_list[["E-ASL"]], "saved"), function(i) yukon_pearson(rlm_seq, i)))
v_yukon = apply(out, 2, summ)

my_device(file.path(fig_dir, "v-length"), file_type, h = 3.4, w = 3.4)
par(mar = c(2,2.25,2,0.5), mgp = c(1.25,0.2,0), tcl = -0.15, cex.axis = 0.75, lend = "square", cex.lab = 0.8)
plot(1,1, xlim = range(rlm_seq), ylim = c(min(v_kusko[4,]), 1), type = "n", ylab = "Selectivity", las = 2, xaxt = "n", xlab = "", lwd = 2, lty = 1)
polygon(x = c(rlm_seq, rev(rlm_seq)), y = c(v_kusko[4,], rev(v_kusko[5,])),
        border = NA, col = "grey90")

lines(v_kusko[3,] ~ rlm_seq, lwd = 4, lend = "round")

lines(v_yukon[3,] ~ rlm_seq, lty = 2)

# keep_t = (nt - 9):nt
keep_t = 1:nt

at_rlm1 = sapply(colMeans(rlm[keep_t,,1,1]), function(a) which_closest(rlm_seq, a))
points(v_kusko[3,at_rlm1] ~ colMeans(rlm[keep_t,,1,1]),
       pch = 21, bg = "grey90", col = "grey90", cex = 1.5)
points(v_kusko[3,at_rlm1] ~ colMeans(rlm[keep_t,,1,1]),
       pch = c("4", "5", "6", "7"), col = "black", cex = 0.8)
at_rlm2 = sapply(colMeans(rlm[keep_t,,1,2]), function(a) which_closest(rlm_seq, a))
points(v_kusko[3,at_rlm2] ~ colMeans(rlm[keep_t,,1,2]),
       pch = 21, bg = "grey90", col = "grey90", cex = 1.5)
points(v_kusko[3,at_rlm2] ~ colMeans(rlm[keep_t,,1,2]),
       pch = c("4", "5", "6", "7"), col = "grey60", cex = 0.8)

lines(v_kusko[4,] ~ rlm_seq, lwd = 1, lty = 1, col = "grey")
lines(v_kusko[5,] ~ rlm_seq, lwd = 1, lty = 1, col = "grey")
unr_perim = 8 * 2* 25.4  
res_perim = 6 * 2 * 25.4

axis_i = seq(1, length(rlm_seq), 150)

axis(side = 1, at = rlm_seq[axis_i], labels = round(rlm_seq[axis_i] * unr_perim, -1))
axis(side = 3, at = rlm_seq[axis_i], labels = round(rlm_seq[axis_i] * res_perim, -1), col.axis = "grey60", col.ticks = "grey60")
mtext(side = 3, "MEFL (mm) w/6 in. Mesh", line = 1, col = "grey60", font = 2, cex = 0.8)
segments(par("usr")[1], par("usr")[4], par("usr")[2], par("usr")[4],col = "grey60", xpd = T)
mtext(side = 1, "MEFL (mm) w/8 in. Mesh", line = 1, font = 2, cex = 0.8)
dev.off()


##### AGE/SEX COMPOSITION COMPARISONS #####
q_esc = array_format(post_summ(post_list[["E-ASL"]], "q_esc")["50%",])
q_com = array_format(post_summ(post_list[["E-ASL"]], "q_com")["50%",])
q_sub = array_format(post_summ(post_list[["E-ASL"]], "q_sub")["50%",])

my_device(file.path(fig_dir, "age-comp"), file_type, h = 5.75, w = 3.4)
par(mfcol = c(4,2), mar = c(0.25,0.25,0.25,0.25), oma = c(3,4.5,1.5,2),
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
  box(lwd = 1.25)
  if (upper_text_draw[as]) {
    mtext(side = 3, line = 0, text = ifelse(as == 1, "Female", "Male"), xpd = T, cex = 0.8)
  }
  if (right_text_draw[as]) {
    mtext(side = 2, line = 2, text = right_text[as], xpd = T, cex = 0.8)
  }
}

mtext(side = 1, outer = T, line = 1.5, "Year")
mtext(side = 2, outer = T, line = 3, "Proportional Contribution")
dev.off()

##### RETURN AT AGE PLOT #####
pi = array_format(post_summ(post_list[["E-ASL"]], "mu_pi_mat")[3,])

ppi = 600
my_device(file.path(fig_dir, "pi-trends"), file_type, h = 4, w = 7.2)
par(mfrow = c(1,2), mar = c(1, 0.25, 0.25, 0.25), oma = c(1.5,3,1.5,2), xaxs = "i", yaxs = "i",
    cex.axis = 1, cex.main = 1, tcl = -0.25, mgp = c(2,0.4,0), lend = "square")
fill_col = c("grey35", "grey55", "grey80", "white")
by = 1969:2013
sex = c("Female", "Male")
for (s in 1:2) {
  plot(1,1, type = "n", xlim = range(by), ylim = c(0,1), las = 1, yaxt = "n", xaxt = "n")
  for (a in (na-1):1) {
    if (a == 1) {
      lines(pi[,a,s] ~ by)
      polygon(x = c(by, rev(by)), y = c(rep(0, ny), rev(pi[,a,s])), col = fill_col[a])
    } else {
      lines(rowSums(pi[,1:a,s]) ~ by)
      polygon(x = c(by, rev(by)), y = c(rep(0, ny), rev(rowSums(pi[,1:a,s]))), col = fill_col[a])
    }
  }
  
  if (s == 2) {
    legend("bottomright", title = "Age", legend = rev(c(4:7)), pch = 22, pt.bg = rev(fill_col), box.col = "white", bg = "white", cex = 0.95, pt.cex = 3)
    axis(side = 4, at = seq(0, 1, 0.2), labels = seq(0, 1, 0.2), las = 2)
    mtext(line = 0.25, side = 3, "Male")
    
  } else {
    axis(side = 2, at = seq(0, 1, 0.2), labels = seq(0, 1, 0.2), las = 2)
    mtext(line = 2, side = 2, "Probability of Return-at-Age")
    mtext(line = 0.25, side = 3, "Female")
  }
  axis(side = 1, at = seq(1970, 2010, 10), labels = substr(seq(1970, 2010, 10), 3, 4), tcl = -0.4)
  axis(side = 1, at = seq(1970, 2012, 2), labels = F, tcl = -0.2)
  
  box()
}
mtext(side = 1, outer = T, line = 0.5, "Brood Year")
dev.off()

##### EGG NUMBER AND MASS RELATIONSHIPS AND CHANGES #####
egg_fun = function(x) {2.8e-4 * x^2.54}
mass_fun = function(x) {8.7e-12 * x^4.83}

ldat = rlm[,,1,1] * 8 * 2* 25.4 

f10 = 1:10
l10 = (nt - 9):nt

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

my_device(file.path(fig_dir, "z-figure"), file_type, h = 6, w = 3.4)

par(mfrow = c(2,1), mar = c(2.5,3.75,0.5,0.5), mgp = c(1.5,0.35,0), tcl = -0.25, lend = "square")

pred_x = seq(500, mean(ldat[f10,4]), length = 100)
pred_egg = egg_fun(pred_x)/egg_fun(mean(ldat[f10,4]))
pred_mass = mass_fun(pred_x)/mass_fun(mean(ldat[f10,4]))

plot(pred_egg ~ pred_x, type = "l", ylim = range(0, pred_egg, pred_mass),
     xlab = "MEFL (mm)", ylab = "", yaxt = "n", lwd = 2)
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


mp = barplot(med, beside = T, ylim = c(-0.5,1), col = "white",
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
legend("topright", legend = c("MEFL", "Egg #", "Egg Mass"), pt.cex = 1.1,
       pch = c(21,22,24), pt.bg = "grey60", bty = "n", cex = 0.8)
usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
text(usr[1] - 0.025 * xdiff, usr[4] - ydiff * 0.05, pos = 4, labels = "(b)", font = 2)

box()

dev.off()

round(egg_fun(colMeans(ldat[f10,]))/egg_fun(mean(ldat[f10,4])), 2)
round(mass_fun(colMeans(ldat[f10,]))/mass_fun(mean(ldat[f10,4])), 2)

##### PER CAPITA REPRODUCTIVE OUTPUT #####

f = function(post) {
  block = rep(1:3, each = nt/3)
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

my_device(file.path(fig_dir, "z-percapita"), file_type, h = 3.75, w = 3.4)
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
block = rep(1:3, each = 14)
lab1 = paste(paste0("'", substr(range(years[block == 1]), 3, 4)), collapse = "-")
lab2 = paste(paste0("'", substr(range(years[block == 2]), 3, 4)), collapse = "-")
lab3 = paste(paste0("'", substr(range(years[block == 3]), 3, 4)), collapse = "-")
legend("top", x.intersp = 0.5, horiz = T, legend = c(lab1, lab2, lab3), pch = c(21, 22, 24), pt.bg = "grey60", pt.cex = 1, cex = 0.75, bg = "white", box.col = "black")
box(lwd = 2)

dev.off()


##### MSY FIGURE #####

msy_plot = function(keep_val = "S", keep_vuln = "unr", keep_mods, xticklabs = F, legend = F, letter) {
  keep = msy[,keep_val,keep_vuln,,keep_mods]
  
  mp = barplot(keep["50%",,keep_mods], yaxt = "n", xaxt = "n", col = "white", border = "white", las = 2,
               beside = T, ylim = range(msy[c("10%", "90%"),keep_val,,,keep_mods]) * c(0.95, 1.15),
               xlim = c(0.5, 32.5))
  usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
  at_v = (mp[1,1:(length(keep_mods) - 1)] + mp[3,2:length(keep_mods)])/2
  zero_break = 3; one_break = 6; two_break = 9
  abline(v = at_v, col = "grey60")
  at_y1 = axisTicks(usr[3:4], log = F, nint = 6)
  lab_y1 = at_y1/1000
  
  lab_y2 = seq(40, 250, 20)
  at_y2 = keep["50%","all","N-0"] * (lab_y2/100)
  abline(h = at_y2[lab_y2 == 100], lty = 2)
  # segments(mp[2,2], keep["50%","all","E-0"], usr[2], keep["50%","all","E-0"], lty = 2)
  axis(side = 2, at = at_y1, labels = lab_y1, las = 2)
  axis(side = 4, at = at_y2, labels = paste0(lab_y2 - 100, "%"), las = 2)
  # rect(xleft = usr[1], ybottom = usr[4] - ydiff * 0.1, xright = usr[2], ytop = usr[4], border = "white", col = "white")
  rect(xleft = usr[1], ybottom = usr[3], xright = at_v[zero_break], ytop = usr[4], lwd = 3, xpd = T, border = "grey60")
  rect(xleft = at_v[zero_break], ybottom = usr[3], xright = at_v[one_break], ytop = usr[4], lwd = 3, xpd = T, border = "grey60")
  rect(xleft = at_v[one_break], ybottom = usr[3], xright = at_v[two_break], ytop = usr[4], lwd = 3, xpd = T, border = "grey60")
  rect(xleft = at_v[two_break], ybottom = usr[3], xright = usr[2], ytop = usr[4], lwd = 3, xpd = T, border = "grey60")
  axis(side = 1, at = mp[2,], labels = F)
  segments(mp[1,], keep["10%","early",keep_mods], mp[1,], keep["90%","early",keep_mods])
  segments(mp[2,], keep["10%","all",keep_mods], mp[2,], keep["90%","all",keep_mods])
  segments(mp[3,], keep["10%","late",keep_mods], mp[3,], keep["90%","late",keep_mods])
  segments(mp[1,], keep["25%","early",keep_mods], mp[1,], keep["75%","early",keep_mods], lwd = 4)
  segments(mp[2,], keep["25%","all",keep_mods], mp[2,], keep["75%","all",keep_mods], lwd = 4)
  segments(mp[3,], keep["25%","late",keep_mods], mp[3,], keep["75%","late",keep_mods], lwd = 4)
  
  points(keep["50%","early",keep_mods] ~ mp[1,], pch = 21, bg = "grey60", cex = 1)
  points(keep["50%","all",keep_mods] ~ mp[2,], pch = 22, bg = "grey60", cex = 1)
  points(keep["50%","late",keep_mods] ~ mp[3,], pch = 24, bg = "grey60", cex = 1)
  
  if (legend) {
    legend("topleft", y.intersp = 0.75, legend = c("First 10", "All", "Last 10"), title = "Years", pch = c(21,22,24), pt.bg = "grey60", pt.cex = 1, cex = 0.75, bg = "white", box.col = "black")
  }
  
  if (xticklabs) {
    axis(side = 1, at = mp[2,], labels = keep_mods, las = 2)
  } else {
    axis(side = 1, at = mp[2,], labels = F, las = 2)
  }
  
  # if (letter) {
  rect(usr[2] - xdiff * 0.3, usr[4] - ydiff * 0.15, usr[2], usr[4], border = "white", col = "white")
  text(x = usr[2] + xdiff * 0.025, y = usr[4] - ydiff * 0.1,
       labels = ifelse(keep_vuln == "unr", paste0("(", letter, ")\n8 in. mesh"), paste0("(", letter, ")\n6 in. mesh")), pos = 2, cex = 0.8, font = 2)
  # }
  
  
  mtext(side = 3, ifelse(keep_vuln == "unr", latex2exp::TeX(paste0(keep_val, "_{MSC}")), ""), line = 0.25, font = 2)
  
  box(lwd = 2)
}

keep_mods = c(
  "N-0",
  "E-0", "EM-0",
  "E-L", "E-A", "E-S",
  # "E-AL", "E-AS", "E-SL",
  "E-ASL", "EM-ASL")


my_device(file.path(fig_dir, "msc"), file_type, h = 5.5, w = 7.2)
par(mfcol = c(2,2), xaxs = "i", yaxs = "i", cex = 1, lend = "square", mar = c(0.5,1.75,1,2.25),
    oma = c(3,0.75,0.75,2), tcl = -0.25, mgp = c(2,0.35,0), cex.axis = 0.9)
msy_plot("S", "unr", keep_mods, legend = T, letter = "a")
msy_plot("S", "res", keep_mods, xticklabs = T, letter = "c"); par(mar = c(0.5,2.25,1,1.75))
msy_plot("H", "unr", keep_mods, legend = F, letter = "b")
msy_plot("H", "res", keep_mods, xticklabs = T, letter = "d")
mtext(side = 2, outer = T, "Escapement or Harvest (1000s)", line = -0.2)
mtext(side = 4, outer = T, "% Change from Model N-0", line = 0.75)
dev.off()

keep_val = "H"
keep_vuln = "unr"
keep_mods = c("N-0", "E-0", "EM-0", "E-A", "E-S", "E-L", "E-AS", "E-AL", "E-SL", "E-ASL", "EM-ASL")

x = msy["50%", keep_val, keep_vuln, "all", keep_mods]
round(x, -3)
p = round((x - x[1])/(x[1]), 2)
range(p[-1])


##### CONVERGENCE SUMMARIES #####

# diag_nodes = c("alpha", "beta_e10", "^R[", "b0_sex", 
#                "b1_sex", "b0_mat", "b1_mat",
#                "phi", "sigma_R_white", "sigma_R0", 
#                "Fcom", "Fsub", "^Vtau$", "^Vsig$", "^Vtha$", "^Vlam$", "log_mean_R0"
# )
# 
# match_p(post, diag_nodes)
# 
# f = function(post) {
#   out = t(post_summ(post, diag_nodes, ess = T, Rhat = T)[c("Rhat", "ess"),])
#   out = out[order(out[,1], decreasing = T),]
#   out = data.frame(out)
#   out = cbind(param = rownames(out), out); rownames(out) = NULL
#   out
# }
# 
# out = lapply(post_list, f)
# f = function(z) {
#   z$base_name = stringr::str_remove(z$param, "\\[.+\\]")
#   z$ess[is.na(z$Rhat)] = NA
#   tapply(z$ess, z$base_name, min, na.rm = T)
# }
# z = sapply(out, f)
# z[z == "Inf"] = NA
# z > 4000
# z = lapply(out, function(x) x[x$Rhat > 1.1 & !is.na(x$Rhat),])
# 
# mean(unlist(sapply(z, function(i) i$Rhat)))
# 
# out1 = sapply(post_list, function(post) post_summ(post, "^v[", ess = T)["ess",])
# out2 = sapply(post_list, function(post) post_summ(post, "pi_mat", ess = T)["ess",])
# 
# diag_plots(post_list[["EM-ASL"]], c("alpha"), T, show_diags = "always")
# class(post_list[["EM-ASL"]])
# 
# post_summ(post_list[["EM-ASL"]], "alpha", Rhat = T)
# 
# x = post_subset(post_list[["EM-ASL"]], "alpha")
# 
# sapply(post_list, function(x) post_summ(x, "alpha")[3,])
# 


