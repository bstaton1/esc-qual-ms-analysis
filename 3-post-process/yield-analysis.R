
rm(list = ls(all = T))

library(StatonMisc)
library(codaTools)
library(abind)

source("../new-func-source.R")

out_dir = "../../model-output"
out_files = dir(out_dir)

msyfiles = out_files[stringr::str_detect(out_files, "msy")]
goalsfiles = out_files[stringr::str_detect(out_files, "goals")]
mods = stringr::str_extract(msyfiles, "[0-9]+")

msy = readRDS(file.path(out_dir, msyfiles[1]))
goals = readRDS(file.path(out_dir, goalsfiles[1]))$EG

for (i in 2:length(msyfiles)) msy = abind::abind(msy, readRDS(file.path(out_dir, msyfiles[i])), along = 5) 
for (i in 2:length(goalsfiles)) goals = abind::abind(goals, readRDS(file.path(out_dir, goalsfiles[i]))$EG, along = 5) 

dimnames(msy)[[5]] = mods
dimnames(goals)[[5]] = mods

keep_mods = c("2", "7", "10", "12", "13", "15", "16", "17", "18")


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
ppi = 600
png(paste(vuln, ".png", sep = ""), h = 5 * ppi, w = 5 * ppi, res = ppi)
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
axis(side = 1, at = xall, las = 2,
     labels = c("Fish/All",
                "Eggs/None",
                "Eggs/Eggs",
                "Eggs/Sex", 
                "Eggs/Mat",
                "Eggs/Eggs-Mat",
                "Eggs/Eggs-Sex",
                "Eggs/Sex-Mat",
                "Eggs/All") 
)

axis(side = 2, at = seq(0, 200000, ifelse(vuln == "unr", 20000, 10000)), labels = seq(0, 200, ifelse(vuln == "unr", 20, 10)), las = 2)

legend(ifelse(vuln == "unr", "bottom", "top"), horiz = T,
       legend = c("Early 10Y", "All", "Late 10Y"),
       pch = c(0, 0, 15), lty = c(1,2,1), col = c("grey", "black", "black"),
       pt.cex = 1.2, cex = 0.8, bg = "grey95"
)

dev.off()
