
# x: the multinomial age/sex matrix
# q: the fitted probability matrix: posterior summaries or each individual sample
# per_n: return the RMSE as opposed to SSE
# by: grouped errors: year, age/sex, age, sex, or total

calc_mult_gof = function(x, q, per_n = F, by = "total") {
  n = rowSums(x)
  x_hat = apply(q, 2, function(x) x * n)
  x_diff = (x - x_hat)^2/x_hat
  
  if (by == "year") {
    out = rowSums(x_diff, na.rm = T)
    if (per_n) out = sqrt(out/n)
  } else {
    if (by == "age/sex") {
      out = colSums(x_diff, na.rm = T)
      if (per_n) out = sqrt(out/colSums(x))
    } else {
      if (by == "age") {
        out = colSums(
          cbind(
            x_diff[,1] + x_diff[,5],
            x_diff[,2] + x_diff[,6],
            x_diff[,3] + x_diff[,7],
            x_diff[,4] + x_diff[,8]
          ), 
          na.rm = T)
        if (per_n) {
          out = sqrt(out/colSums(cbind(
            x[,1] + x[,5],
            x[,2] + x[,6],
            x[,3] + x[,7],
            x[,4] + x[,8]
          ), na.rm = T))
        }
        
      } else {
        if (by == "sex") {
          out = colSums(
            cbind(
              rowSums(x_diff[,1:4]),
              rowSums(x_diff[,5:8])
            ),
            na.rm = T
          )
          if (per_n) {
            out = sqrt(out/colSums(cbind(
              rowSums(x[,1:4]),
              rowSums(x[,5:8])
            ), na.rm = T))
          }
        } else {
          if (by == "total") {
            out = sum(x_diff, na.rm = T)
            if (per_n) out = sqrt(out/sum(n))
          } else {
            stop ("by must be one of 'age', 'sex', 'age/sex', 'year', or 'total'")
          }
        }
      }
    }
  }
  out
}