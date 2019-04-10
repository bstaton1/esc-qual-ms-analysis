
rm(list = ls(all = T))

library(StatonMisc)
library(codaTools)
library(abind)

source("../new-func-source.R")

out_dir = "../../model-output"
out_files = dir(out_dir)

msy_files = file.path(out_dir, out_files[stringr::str_detect(out_files, "msy")])

i = 1

msys = readRDS(msy_files[1])
for (i in 2:length(msy_files)) {msys = abind(msys, readRDS(msy_files[i]), along = 5)}

value = "esc"
vuln = "res"
q = c("2.5%", "25%", "50%", "75%", "97.5%")

early = msys[,value,vuln,"early",]
all = msys[,value,vuln,"all",]
late = msys[,value,vuln,"late",]

# windows()

q_lwr = "10%"; q_upr = "90%"
xall = 1:6
xearly = xall - 0.15
xlate = xall + 0.15
ppi = 600
png(paste(vuln, ".png", sep = ""), h = 5 * ppi, w = 5 * ppi, res = ppi)
par(mar = c(6,3.5,2,2), tcl = -0.25, mgp = c(2,0.5,0))
plot(all["50%",] ~ xall, yaxt = "n", xlim = range(xall) + c(-0.15,0.15), xaxt = "n", ylim = range(msys[c(q_lwr, q_upr),value,,,]), pch = 0,
     xlab = "", ylab = "", main = ifelse(vuln == "unr", "Unrestricted", "Restricted"), cex = 1.2)
points(early["50%",] ~ xearly, pch = 0, col = "grey", cex = 1.2)
points(late["50%",] ~ xlate, pch = 15, cex = 1.2)
segments(xall, all[q_lwr,], xall, all[q_upr,], lty = 2)
segments(xearly, early[q_lwr,], xearly, early[q_upr,], col = "grey")
segments(xlate, late[q_lwr,], xlate, late[q_upr,])
mtext(side = 1, "Model (Spawning Units/Trends)", line = 4.5, font = 2)
mtext(side = 2, "Smsc", line = 2.5, font = 2)
axis(side = 1, at = xall, las = 2,
     labels = c("Body/All", "Eggs/Egg",
                "Eggs/Sex", "Eggs/Mat", 
                "Eggs/Vuln", "Eggs/All") 
)

axis(side = 2, at = seq(0, 200000, ifelse(vuln == "unr", 20000, 10000)), labels = seq(0, 200, ifelse(vuln == "unr", 20, 10)), las = 2)

legend("top", horiz = T,
       legend = c("Early 10Y", "All", "Late 10Y"),
       pch = c(0, 0, 15), lty = c(1,2,1), col = c("grey", "black", "black"),
       pt.cex = 1.2, cex = 0.8, bg = "grey95"
)

dev.off()
