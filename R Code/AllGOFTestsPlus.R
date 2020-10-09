

#####################################################################
# NAME: Nikola Surjanovic and Peter Tea                             #
# DATE: March 24, 2020                                              #
# PURPOSE: C_n, and Information Matrix (IM) test statistics         #
#                                                                   #
# NOTES: Works for binary response regression models,               #
#   with NO replications. Adapted from the "AllGOFTests.R" code     #
#   by Tom Loughin.                                                 #
#####################################################################

Cn.test <- function(obj) {
  # First, check to see if we fed in the right kind of object
  stopifnot(family(obj)$family == "binomial" && family(obj)$link == "logit")
  dat <- model.matrix(obj)
  y <- model.frame(obj)[ , 1]
  p <- ncol(dat)
  n <- nrow(dat)
  mu.hat <- obj$fitted.values
  nu.hat <- obj$linear.predictors
  
  C_n <- sum((y-mu.hat)/sqrt(mu.hat*(1-mu.hat)))
  Omega_n <- 0
  for (i in 1:n) {
    Omega_n <- Omega_n + exp(2*nu.hat[i])/((1+exp(nu.hat[i]))^4*mu.hat[i]*(1-mu.hat[i]))*(dat[i, ] %*% t(dat[i, ]))
  }
  Omega_n <- 1/n * Omega_n
  
  v_n <- 0
  for (i in 1:n) {
    v_n <- v_n + exp(nu.hat[i])/(1+exp(nu.hat[i]))^2/sqrt(mu.hat[i]*(1-mu.hat[i])) * dat[i, ]
  }
  v_n <- 1/n * v_n
  
  sigma2_n <- 1 - t(v_n) %*% Omega_n %*% v_n
  z <- C_n/sqrt(n*sigma2_n)
  P <- 2*pnorm(abs(z), lower.tail=FALSE)
  
  return(structure(list(
    method = c("C_n Statistic Goodness-of-Fit Test"),
    data.name = deparse(substitute(obj)),
    statistic = c(Z = z),
    p.value = P
  ), class = 'htest'))
}


IM.test <- function(obj) {
  # First, check to see if we fed in the right kind of object
  stopifnot(family(obj)$family == "binomial" && family(obj)$link == "logit")
  dat <- model.matrix(obj)
  y <- model.frame(obj)[ , 1]
  p <- ncol(dat)
  mu.hat <- obj$fitted.values
  
  chisq <- sum((y - mu.hat)*(1-2*mu.hat)*rowSums(dat^2))
  P <- pchisq(chisq, df=p, lower.tail=FALSE)
  
  return(structure(list(
    method = c("Information-Matrix (White 1982) Goodness-of-Fit Test"),
    data.name = deparse(substitute(obj)),
    statistic = c(X2 = chisq),
    parameter = c(df = p),
    p.value = P
  ), class = 'htest'))
}
