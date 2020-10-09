# Recreate some plots from Stukel's paper

h <- function(x, alpha1, alpha2) {
  # x is a vector (potentially)!
  n <- length(x)
  ans <- numeric(n)
  for (i in 1:n) {
    eta <- x[i]
    if (eta>=0) {
      if (alpha1 > 0) {
        ansi <- 1/alpha1 * (exp(alpha1*abs(eta)) - 1)
      } else if (alpha1 == 0) {
        ansi <- eta
      } else {
        ansi <- -1/alpha1 * log(1-alpha1*abs(eta))
      }
    } else {
      if (alpha2 > 0) {
        ansi <- -1/alpha2 * (exp(alpha2*abs(eta)) - 1)
      } else if (alpha2 == 0) {
        ansi <- eta
      } else {
        ansi <- 1/alpha2 * log(1- alpha2*abs(eta))
      }
    }
    ans[i] <- ansi
  }
  return(ans)
}


# For slides
curve(1*x, from=-3, to=3, xlab=expression(eta), ylab=expression(h(eta)), lwd=3)
curve(h(x, 0.25, 0.25), add=TRUE, lty=2, col='blue', lwd=3)
curve(h(x, -1, -1), add=TRUE, lty=3, col='red', lwd=5)


curve(exp(x)/(1+exp(x)), from=-6, to=6, xlab=expression(eta), ylab=expression(mu(eta)), lwd=3)
curve(1/(1+exp(-h(x, 0.25, 0.25))), add=TRUE, lty=2, col='blue', lwd=3)
curve(1/(1+exp(-h(x, -1, -1))), add=TRUE, lty=3, col='red', lwd=5)



# For report
par(mar=c(5.1, 5.1, 4.1, 2.1)) # Default: 5.1, 4.1, 4.1, 2.1
curve(1*x, from=-3, to=3, xlab=expression(eta), ylab=expression(h(eta)), lwd=5, cex.axis=2, cex.lab=2)
curve(h(x, 0.25, 0.25), add=TRUE, lty=2, col='blue', lwd=5)
curve(h(x, -1, -1), add=TRUE, lty=3, col='red', lwd=5)


curve(exp(x)/(1+exp(x)), from=-6, to=6, xlab=expression(eta), ylab=expression(mu(eta)), lwd=5, cex.axis=2, cex.lab=2)
curve(1/(1+exp(-h(x, 0.25, 0.25))), add=TRUE, lty=2, col='blue', lwd=5)
curve(1/(1+exp(-h(x, -1, -1))), add=TRUE, lty=3, col='red', lwd=5)

par(mar=c(5.1, 4.1, 4.1, 2.1))
