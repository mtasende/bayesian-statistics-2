m = 1e5

a = 5
b = 3
theta = rbeta(m, a, b)

odds = theta / (1 - theta)
mean(odds)

proba_greater = mean(odds > 1.0)
proba_greater

nsamples = rnorm(m, 0, 1)
quantile(nsamples, 0.3)
qnorm(0.3, 0, 1)

sqrt(5.2 / 5000)
