library(data.table)

F_hat <- function(t) {
  # assume NA is right-censored
  c <- sum(is.na(t))
  n <- length(t)
  den <- (n-c)/n
  function(...) ecdf(na.omit(t))(...)*(den)
}

# test <- F_hat(t = c(runif(1000, 0, 10), rep(NA, 10)))

se_F_hat <- function(F_hat, n) {
  function(t) sqrt(F_hat(t)*(1-F_hat(t))/n)
}

# se_F_hat(F_hat(c(c(1,2,3,4), rep(NA, 10))), n=10)(1:3)

endpoint <- function(F_hat, n, type=c('lower', 'upper'), confidence = c('0.8', '0.9', '0.95', '0.99')) {
  ep_const <- c(2.86, 3.12, 3.36, 3.85)[which(match.arg(confidence)==eval(formals()$confidence))]
  arg_type <- match.arg(type)
  function(t) {
    w <- exp(ep_const*se_F_hat(F_hat, n)(t)/(F_hat(t)*(1-F_hat(t))))
    if(arg_type=='upper') w <- 1/w
    F_hat(t)/(F_hat(t) + (1-F_hat(t))*w)
  }
}

# data(mtcars)
# setDT(mtcars)
# setorder(mtcars, mpg)
# mtcars[,cdf:=F_hat(mpg)(mpg)]
# mtcars[,lb:=endpoint(F_hat(mpg), n=.N, type='l')(mpg)]
# mtcars[,ub:=endpoint(F_hat(mpg), n=.N, type='u')(mpg)]
# 
# mtcars[,plot(mpg, cdf)]
# mtcars[,lines(mpg, lb)]
# mtcars[,lines(mpg, ub)]
# 
# 
