
setwd("C:/Users/bas0041/Dropbox/PhD Project/Manuscripts/Escapement Quality/esc-qual-ms-analysis/1-data-prep/c-fecundity-data")

dat = read.csv("yukon-fecundity-2006.csv")
dat$weight = dat$weight/2.20462  # convert weight to kilograms

# fit length-weight relationship
plot(weight ~ length, data = dat)
fit_lw = lm(log(weight) ~ log(length), data = dat)
lw_a = unname(exp(coef(fit_lw)[1]))
lw_b = unname(coef(fit_lw)[2])
curve(lw_a * x ^ lw_b, from = 600, to = 1000, add = T)

# fit weight-fecundity relationship
plot(fecundity ~ weight, data = dat, col = ifelse(dat$district == 5, "blue", "red"))
fit_wf = lm(fecundity ~ weight, data = dat)
summary(fit_wf)
wf_b0 = unname(coef(fit_wf)[1])
wf_b1 = unname(coef(fit_wf)[2])
curve(wf_b0 + wf_b1 * x, from = 4, to = 14, add = T)

# function to obtain fecundity from length
predict_fecundity = function(length, lw_a = 1.282001e-09, lw_b = 3.342558,
                          wf_b0 = 2847.348, wf_b1 = 417.9477) {
  pred_weight = lw_a * length ^ lw_b
  round(wf_b0 + pred_weight * wf_b1)
}

# length-fecundity relationship
l = 500:950
p = predict_fecundity(l)
plot(p ~ l, type = "l")

# apply to kusko mean length data
mean_lengths = read.csv("../b-length-data/outputs/esc-mean-length.csv")
f_lengths = mean_lengths[,stringr::str_detect(colnames(mean_lengths), "f")]
out = apply(f_lengths, 2, predict_fecundity)

out = data.frame(cbind(year = mean_lengths$year, out))
write.csv(out, "predicted-fecundity.csv")
