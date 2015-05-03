library(ineq)

getGini <- function(data)
{
  print(data)
  Gini(data)
  plot(Lc(data), col = "darkred", lwd = 2)
}

getGini(rep(1, 10))
getGini(c(rep(1, 10), rep(2, 10)))
getGini(c(1, 2))
getGini(0:1)
getGini(1:10)
getGini(exp(1:10))
getGini(c(1, 4))
