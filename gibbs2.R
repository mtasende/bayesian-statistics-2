# y = c(1.2, 1.4, -0.5, 0.3, 0.9, 2.3, 1.0, 0.1, 1.3, 1.9)
y = c(-0.2, -1.5, -5.3, 0.3, -0.8, -2.2)
ybar = mean(y)
n = length(y)

## prior
prior = list()

prior$mu_0 = 0.0
prior$sig2_0 = 1.0

prior$n_0 = 2.0 # Estimated prior sample size
prior$s2_0 = 1.0 # Prior guess
prior$nu_0 = prior$n_0 /2.0
prior$beta_0 = prior$n_0 * prior$s2_0 / 2.0

hist(y, freq=FALSE, xlim=c(-1.0, 3.0))
curve(dnorm(x=x, mean=prior$mu_0, sd=sqrt(prior$sig2_0)), lty=2, add=TRUE)
points(y, rep(0,n), pch=1)
points(ybar, 0, pch=19)


set.seed(53)

init = list()
init$mu = 0.0

post = gibbs(y=y, n_iter=5e3, init=init, prior=prior)
head(post)
tail(post)

library("coda")
plot(as.mcmc(post))


mean(post[,1])
mean(post[,2])
summary(post[,2])
quantile(post[,2], 0.975)
quantile(post[,2], 0.025)
