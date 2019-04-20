
rm(list = ls(all = T))

library(StatonMisc)
library(codaTools)
library(abind)

source("../2-model-fit/1-compile-data.R")
source("../new-func-source.R")

out_dir = "../../model-output"
out_files = dir(out_dir)

msyfiles = out_files[stringr::str_detect(out_files, "msy-1")]
goalsfiles = out_files[stringr::str_detect(out_files, "goals-1")]
postfiles = out_files[stringr::str_detect(out_files, "post-1")]
mods = stringr::str_extract(msyfiles, "[0-9]+")

msy = readRDS(file.path(out_dir, msyfiles[1]))
goals = readRDS(file.path(out_dir, goalsfiles[1]))$EG
post = list(); post[[1]] = readRDS(file.path(out_dir, postfiles[1]))

for (i in 2:length(mods)) msy = abind::abind(msy, readRDS(file.path(out_dir, msyfiles[i])), along = 5) 
for (i in 2:length(mods)) goals = abind::abind(goals, readRDS(file.path(out_dir, goalsfiles[i]))$EG, along = 5) 
for (i in 2:length(mods)) {cat("\r", i); post[[i]] = readRDS(file.path(out_dir, postfiles[i]))}

dimnames(msy)[[5]] = mods
dimnames(goals)[[5]] = mods
names(post)[[5]] = mods

keep_mods = as.character(1:10)
mod_names = c(
  "Fish/None",
  "Fish/All",
  "Eggs/None",
  "Eggs/Eggs",
  "Eggs/Sex",
  "Eggs/Mat",
  "Eggs/Eggs-Mat", 
  "Eggs/Eggs-Sex",
  "Eggs/Sex-Mat",
  "Eggs/Eggs-Sex-Mat")

msy = msy[,,,,keep_mods]


value = "esc"
vuln = "res"
q = c("2.5%", "25%", "50%", "75%", "97.5%")

early = msy[,value,vuln,"early",]
all = msy[,value,vuln,"all",]
late = msy[,value,vuln,"late",]

# windows()

q_lwr = "10%"; q_upr = "90%"
xall = 1:length(keep_mods)
xearly = xall - 0.2
xlate = xall + 0.2
# ppi = 600
# png(paste(vuln, ".png", sep = ""), h = 5 * ppi, w = 5 * ppi, res = ppi)
par(mar = c(8,3.5,2,2), tcl = -0.25, mgp = c(2,0.5,0))
plot(all["50%",] ~ xall, yaxt = "n", xlim = range(xall) + c(-0.15,0.15), xaxt = "n", ylim = range(msy[c(q_lwr, q_upr),value,,,]), pch = 0,
     xlab = "", ylab = "", main = ifelse(vuln == "unr", "Unrestricted", "Restricted"), cex = 1.2)
points(early["50%",] ~ xearly, pch = 0, col = "grey", cex = 1.2)
points(late["50%",] ~ xlate, pch = 15, cex = 1.2)
segments(xall, all[q_lwr,], xall, all[q_upr,], lty = 2)
segments(xearly, early[q_lwr,], xearly, early[q_upr,], col = "grey")
segments(xlate, late[q_lwr,], xlate, late[q_upr,])
mtext(side = 1, "Model (Spawning Units/Trends)", line = 6.5, font = 2)
mtext(side = 2, "Smsc", line = 2.5, font = 2)
axis(side = 1, at = xall, las = 2,labels = mod_names)

plot(late["50%",] - early["50%",], xaxt = "n", yaxt = "n", xlab = "", ylab = "")
axis(side = 1, at = xall, las = 2,labels = mod_names)
axis(side = 2, at = seq(0, 200000, ifelse(vuln == "unr", 20000, 10000)), labels = seq(0, 200, ifelse(vuln == "unr", 20, 10)), las = 2)

legend(ifelse(vuln == "unr", "bottom", "top"), horiz = T,
       legend = c("Early 10Y", "All", "Late 10Y"),
       pch = c(0, 0, 15), lty = c(1,2,1), col = c("grey", "black", "black"),
       pt.cex = 1.2, cex = 0.8, bg = "grey95"
)

# dev.off()
a_min = 4; a_max = 7; na = length(a_min:a_max)
early_nt = prep_samples(post = post[[1]], keep_t = 1:10, keep_y = 1:10, eggs_trend = F, silent = F)
early_t = prep_samples(post = post[[1]], keep_t = 1:10, keep_y = 1:10, eggs_trend = T, silent = F)


Fcom = matrix(unlist(lapply(post, function(m) {
  summ_post(m, "Fcom")[3,]
})), nrow = 42, ncol = length(mods))
Fsub = matrix(unlist(lapply(post, function(m) {
  summ_post(m, "Fsub")[3,]
})), nrow = 42, ncol = length(mods))

alpha = matrix(unlist(lapply(post, function(m) {
  summ_post(m, "alpha")[3,]
})), nrow = 1, ncol = length(mods))


par(mfrow = c(2,1), mar = c(2,2,2,2))
matplot(Fcom, main = "Fcom", type = "l", ylim = range(cbind(Fcom, Fsub)))
matplot(Fsub, main = "Fsub", type = "l", ylim = range(cbind(Fcom, Fsub)))

write.csv(data.frame(
  year = 1976:2017,
  Fcom = rowMeans(Fcom),
  Fsub = rowMeans(Fsub)
), "F_ests.csv", row.names = F)

vuln = "unr"
type = "Rmax"
early = goals[,type,vuln,"early",keep_mods]
all = goals[,type,vuln,"all",keep_mods]
late = goals[,type,vuln,"late",keep_mods]

par(mar = c(2,8,1,1))
plot(xall ~ all[1,],type = "n", xaxt = "n", yaxt = "n",
     ylim = rev(range(c(xearly, xlate))),
     xlim = c(0, 150000), xlab = "", ylab = "")
usr = par("usr")
rect(xleft = 65000, ybottom = usr[3], xright = 120000, ytop = usr[4], col = "grey90", border = "grey")

segments(all[1,], xall, all[2,], xall, lty = 2)
points(xall ~ all[1,])
points(xall ~ all[2,])
segments(early[1,], xearly, early[2,], xearly, col = "grey")
points(xearly ~ early[1,], pch = 16, col = "grey")
points(xearly ~ early[2,], pch = 16, col = "grey")
segments(late[1,], xlate, late[2,], xlate)
points(xlate ~ late[1,], pch = 16)
points(xlate ~ late[2,], pch = 16)
axis(side = 2, at = xall, las = 2,labels = mod_names)
axis(side = 1, at = seq(0, 160000, 20000), labels = seq(0, 160, 20))
abline(h = xall - 0.5, col = "grey", lty = 3)
abline(h = xall + 0.5, col = "grey", lty = 3)
box()

get_nodes(post[[1]])
matrix(unlist(lapply(post, function(m) summ_post(m, "R[")[3,])), ny, 2)

