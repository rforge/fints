autocorTest <- function(x, lag=ceiling(log(length(x)))){
  Box.test(x, lag, "Ljung-Box")
}
