set.seed(62361)
x <- rnorm(500, 0, 1)
betas <- c(1, 0.7, 0.5)
mu <- 1/(1+exp(-(betas[1] + betas[2]*x + betas[3]*x^2)))
y <- rbinom(500, size=1, prob=mu)

obj <- glm(y~poly(x,2), family=binomial(link='logit'))
obj

obj2 <- glm(y~x, family=binomial(link='logit'))
obj2

HLTest(obj, g=10)
o.r.test(obj)
stukel.test(obj)
# IM.test(obj)
Cn.test(obj)

HLTest(obj2, g=10)
o.r.test(obj2)
stukel.test(obj2)
# IM.test(obj2)
Cn.test(obj2)


####################################################

set.seed(320920)
n <- 40
x <- runif(n, -6, 6)
betas <- c(0, 0.7)
mu <- 1/(1+exp(-(betas[1] + betas[2]*x)))
y <- rbinom(n, size=1, prob=mu)

obj <- glm(y~x, family=binomial(link='logit'))


# For slides
plot(x=x,y=y, type='p', col='black', lwd=3, xlim=c(-6,6), ylab=expression(pi))
curve(1/(1+exp(-(betas[1] + betas[2]*x))), col='red', lwd=3, add=TRUE)

# For report
plot(x=x,y=y, type='p', col='black', lwd=5, xlim=c(-6,6), ylab=expression(pi), cex.axis=2, cex.lab=2)
curve(1/(1+exp(-(betas[1] + betas[2]*x))), col='red', lwd=5, add=TRUE)
