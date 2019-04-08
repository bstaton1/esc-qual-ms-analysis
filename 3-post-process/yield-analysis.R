
rm(list = ls(all = T))

library(StatonMisc)
library(codaTools)
library(abind)

source("../2-model-fit/1-compile-data.R")
source("../new-func-source.R")

early_keep_t = 1:10; early_keep_y = 1:10
late_keep_t = nt - 9:0; late_keep_y = ny - 9:0
all_keep_t = 1:nt; all_keep_y = 1:ny

post = readRDS("../../model-output/post-1.rds")
post = thin_post(post, 0.8)
post.samp_early = prep_samples(post, keep_t = early_keep_t, keep_y = early_keep_y)
post.samp_late = prep_samples(post, keep_t = late_keep_t, keep_y = late_keep_y)
post.samp_all = prep_samples(post, keep_t = all_keep_t, keep_y = all_keep_t)

msy_early1 = msy_search(post.samp_early, "fish")
msy_late1 = msy_search(post.samp_late, "fish")
msy_all1 = msy_search(post.samp_all, "fish")
msy1 = abind(msy_early1, msy_all1, msy_late1, along = 4)

post = readRDS("../../model-output/post-2.rds")
post = thin_post(post, 0.8)
post.samp_early = prep_samples(post, keep_t = early_keep_t, keep_y = early_keep_y)
post.samp_late = prep_samples(post, keep_t = late_keep_t, keep_y = late_keep_y)
post.samp_all = prep_samples(post, keep_t = all_keep_t, keep_y = all_keep_t)

msy_early2 = msy_search(post.samp_early, "fish")
msy_late2 = msy_search(post.samp_late, "fish")
msy_all2 = msy_search(post.samp_all, "fish")
msy2 = abind(msy_early2, msy_all2, msy_late2, along = 4)

post = readRDS("../../model-output/post-3.rds")
post = thin_post(post, 0.8)
post.samp_early = prep_samples(post, keep_t = early_keep_t, keep_y = early_keep_y)
post.samp_late = prep_samples(post, keep_t = late_keep_t, keep_y = late_keep_y)
post.samp_all = prep_samples(post, keep_t = all_keep_t, keep_y = all_keep_t)

msy_early3 = msy_search(post.samp_early, "fish")
msy_late3 = msy_search(post.samp_late, "fish")
msy_all3 = msy_search(post.samp_all, "fish")
msy3 = abind(msy_early3, msy_all3, msy_late3, along = 4)

post = readRDS("../../model-output/post-4.rds")
post = thin_post(post, 0.8)
post.samp_early = prep_samples(post, keep_t = early_keep_t, keep_y = early_keep_y)
post.samp_late = prep_samples(post, keep_t = late_keep_t, keep_y = late_keep_y)
post.samp_all = prep_samples(post, keep_t = all_keep_t, keep_y = all_keep_t)

msy_early4 = msy_search(post.samp_early, "fish")
msy_late4 = msy_search(post.samp_late, "fish")
msy_all4 = msy_search(post.samp_all, "fish")
msy4 = abind(msy_early4, msy_all4, msy_late4, along = 4)

post = readRDS("../../model-output/post-5.rds")
post = thin_post(post, 0.8)
post.samp_early = prep_samples(post, keep_t = early_keep_t, keep_y = early_keep_y)
post.samp_late = prep_samples(post, keep_t = late_keep_t, keep_y = late_keep_y)
post.samp_all = prep_samples(post, keep_t = all_keep_t, keep_y = all_keep_t)

msy_early5 = msy_search(post.samp_early, "eggs")
msy_late5 = msy_search(post.samp_late, "eggs")
msy_all5 = msy_search(post.samp_all, "eggs")
msy5 = abind(msy_early5, msy_all5, msy_late5, along = 4)

post = readRDS("../../model-output/post-6.rds")
post = thin_post(post, 0.8)
post.samp_early = prep_samples(post, keep_t = early_keep_t, keep_y = early_keep_y)
post.samp_late = prep_samples(post, keep_t = late_keep_t, keep_y = late_keep_y)
post.samp_all = prep_samples(post, keep_t = all_keep_t, keep_y = all_keep_t)

msy_early6 = msy_search(post.samp_early, "eggs")
msy_late6 = msy_search(post.samp_late, "eggs")
msy_all6 = msy_search(post.samp_all, "eggs")
msy6 = abind(msy_early6, msy_all6, msy_late6, along = 4)

post = readRDS("../../model-output/post-7.rds")
post = thin_post(post, 0.8)
post.samp_early = prep_samples(post, keep_t = early_keep_t, keep_y = early_keep_y)
post.samp_late = prep_samples(post, keep_t = late_keep_t, keep_y = late_keep_y)
post.samp_all = prep_samples(post, keep_t = all_keep_t, keep_y = all_keep_t)

msy_early7 = msy_search(post.samp_early, "eggs")
msy_late7 = msy_search(post.samp_late, "eggs")
msy_all7 = msy_search(post.samp_all, "eggs")
msy7 = abind(msy_early7, msy_all7, msy_late7, along = 4)

post = readRDS("../../model-output/post-8.rds")
post = thin_post(post, 0.8)
post.samp_early = prep_samples(post, keep_t = early_keep_t, keep_y = early_keep_y)
post.samp_late = prep_samples(post, keep_t = late_keep_t, keep_y = late_keep_y)
post.samp_all = prep_samples(post, keep_t = all_keep_t, keep_y = all_keep_t)

msy_early8 = msy_search(post.samp_early, "eggs")
msy_late8 = msy_search(post.samp_late, "eggs")
msy_all8 = msy_search(post.samp_all, "eggs")
msy8 = abind(msy_early8, msy_all8, msy_late8, along = 4)


msys = abind(msy1, msy2, msy3, msy4, msy5, msy6, msy7, msy8, along = 5)

dimnames(msys)[[4]] = c("early", "all", "late")

value = "esc"
vuln = "unr"
period = "early"
q = c("2.5%", "25%", "50%", "75%", "97.5%")

early = msys[q,value,vuln,"early",]
all = msys[q,value,vuln,"all",]
late = msys[q,value,vuln,"late",]

# windows()

xall = 1:8
xearly = xall - 0.15
xlate = xall + 0.15
ppi = 600
# png("res.png", h = 5 * ppi, w = 6 * ppi, res = ppi)
par(mar = c(3,3,2,2))
plot(all["50%",] ~ xall, ylim = range(msys[c("25%", "75%"),value,vuln,,]), pch = 0,
     xlab = "Model", ylab = "Smsc", main = "Restricted", cex = 1.2)
points(early["50%",] ~ xearly, pch = 0, col = "grey", cex = 1.2)
points(late["50%",] ~ xlate, pch = 15, cex = 1.2)
segments(xall, all["25%",], xall, all["75%",], lty = 2)
segments(xearly, early["25%",], xearly, early["75%",], col = "grey")
segments(xlate, late["25%",], xlate, late["75%",])
mtext(side = 1, "Model", line = 2)
mtext(side = 2, "Smsc", line = 2)

legend("topleft",
       legend = c("First Half", "All", "Last Half"),
       pch = c(0, 0, 15), lty = c(1,2,1), col = c("grey", "black", "black"),
       pt.cex = 1.2
)

# dev.off()
