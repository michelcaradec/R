#' ---
#' title: "Gini Index and Lorenz Curve"
#' author: "Michel Caradec"
#' output: html_document
#' date: ""
#' ---

library(ineq)

getGini <- function(data)
{
  print(format(data, digits = 2))
  plot(
    Lc(data),
    col = "darkred",
    lwd = 2,
    main = paste("Lorenz Curve - ", "Gini index = ", format(Gini(data), digits = 2))
  )
}

# ----

getGini(rep(1, 10))
getGini(c(rep(1, 10), rep(2, 10)))
getGini(c(1, 2))
getGini(0:1)
getGini(1:10)
getGini(exp(1:10))
getGini(c(1, 4))
