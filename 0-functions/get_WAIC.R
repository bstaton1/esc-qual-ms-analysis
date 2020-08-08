
## calculate WAIC
# based on the total ppd from the model, get pD (effective parameters) and WAIC

# adapted from Hooten and Hobbs (2015) online supplement
# https://esajournals.onlinelibrary.wiley.com/doi/10.1890/14-0661.1

get_WAIC = function(ppd) {
  tmp_sum = -2 * sum(log(apply(ppd, 2, mean)))
  pD = sum(apply(log(ppd), 2, var))
  c(pD = pD, WAIC = tmp_sum + 2 * pD)
}
