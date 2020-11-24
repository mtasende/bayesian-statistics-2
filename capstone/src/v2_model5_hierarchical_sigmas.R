rm(list=ls())
# options(device="X11")
options(device="RStudioGD")
setwd("/home/miguel/coursera/bayesian_statistics_2/capstone/data/preprocessed_data")
library(coda)
library(rjags)

dat = read.csv(file="data.csv", header=TRUE)
head(dat)
dim(dat)
unique(dat$symbol)

mod_string = " model {
for (i in 1:length(y)) {
y[i] ~ dnorm(mu[symbol[i]], prec[symbol[i]])
}

for (j in 1:max(symbol)) {
mu[j] ~ dnorm(mu0, prec_mu)
prec[j] ~ dgamma(a0, b0)
sig[j] = sqrt(1.0 / prec[j])
}

mu0 ~ dnorm(0.0, 1.0/1.0e6)
prec_mu ~ dgamma(1/2.0, 1*10.0/2.0)
tau = sqrt(1.0/prec_mu)

a0 ~ dexp(100.0)
b0 ~ dexp(100.0)
} "

set.seed(2017)
str(dat)

x = dat$cpi / dat$cpi_usa
y = dat$rate_std * x
data_jags = list(y=y, symbol=as.numeric(dat$symbol))

params = c("mu", "sig", "mu0", "tau", "a0", "b0")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod, variable.names=params, n.iter=7e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

### Convergence diagnostics
plot(mod_sim, ask = TRUE)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim, ask=TRUE)
effectiveSize(mod_sim)
raftery.diag(mod_sim)
gelman.diag(mod_sim, multivariate=FALSE)
summary(mod_sim)

dic.samples(mod, n.iter = 1e3)

# Residuals Analysis
pm_params = colMeans(mod_csim)
yhat = rep(pm_params[3:53], each=120)
resid = y - yhat
plot(resid)
plot(yhat, resid)

# Let's first calculate the UYU rate (in UYU/USD):
y_uy = y[dat$symbol=='UYU']
x_uy = x[dat$symbol=='UYU']
dat_uy = dat[dat$symbol=='UYU',]
dat_uy_base = dat_uy[dat_uy$date=='2010-08-15',]

mu_rate = (as.numeric(x_uy[120] / dat_uy_base['exchange_rate'])) * (1 / mod_csim[,'mu[49]'])
plot(as.mcmc(mu_rate))

# What is the expectation of UYU? Is it under or over valued?
# (The numeric value for UYU is 49)
mean(mu_rate)
rate_current = (as.numeric(x_uy[120] / dat_uy_base['exchange_rate'])) * (1 / y_uy[120]) # Current rate of UYU/USD
rate_current
# Probability that it is overvalued
# Let's simulate y:
y_uy_csim = rnorm(nrow(mod_csim), mod_csim[,'mu[49]'], mod_csim[,'sig[49]'])
rate_uy_csim = (as.numeric(x_uy[120] / dat_uy_base['exchange_rate'])) * (1 / y_uy_csim)
mean(y_uy_csim < y_uy[120])
mean(rate_uy_csim > rate_current)
plot(as.mcmc(rate_uy_csim))
# Just to check, what would the naive probability calculation yield
mean(y < y_uy[120])

# Equally tailed interval for UYU mean given the cpi and cpi_usa values
summary(mod_sim) # Result: [1.002746, 1.054839]
dat_uy = dat[dat$symbol=='UYU',]
dat_uy_now = dat_uy[nrow(dat_uy),]
dat_uy_base = dat_uy[dat_uy$date=='2010-08-15',]
ex_min = dat_uy_base$exchange_rate * 1.002746 * dat_uy_now$cpi_usa / dat_uy_now$cpi
ex_max = dat_uy_base$exchange_rate * 1.054839 * dat_uy_now$cpi_usa / dat_uy_now$cpi
1/ex_min
1/ex_max

# What about for the actual value?
confidence_int = quantile(y_uy_csim, c(0.025, 0.975))
ex_min_y = dat_uy_base$exchange_rate * confidence_int[1] * dat_uy_now$cpi_usa / dat_uy_now$cpi
ex_max_y = dat_uy_base$exchange_rate * confidence_int[2] * dat_uy_now$cpi_usa / dat_uy_now$cpi
1/ex_min_y
1/ex_max_y

# Expected value
1/(dat_uy_base$exchange_rate * mean(mod_csim[,'mu[49]']) * dat_uy_now$cpi_usa / dat_uy_now$cpi)


# Convenience of buying USD or UI
y0 = y_uy[120]
u0 = dat$cpi_usa[dat$symbol=='UYU'][120]
u1 = 1.02 * u0 # This is to be variable

# 0.05 quantile (Very confident UI is better if larger than this)
lambda = quantile(y_uy, 0.05)
u1 = u0 * y0 / lambda
inflation_usa_1 = u1 / u0 - 1.0
inflation_usa_1

# Border case (Not sure)
theta = quantile(y_uy, 0.5)
u2 = u0 * y0 / theta
inflation_usa_2 = u2 / u0 - 1.0
inflation_usa_2

# 0.95 quantile (Very confident USD is better if smaller than this)
nu = quantile(y_uy, 0.95)
u3 = u0 * y0 / nu
inflation_usa_3 = u3 / u0 - 1.0
inflation_usa_3

# Historical inflation
u = dat$cpi_usa[dat$symbol=='UYU']
inf_usa = u[2:120] / u[1:119] - 1.0
plot(dat$date[dat$symbol == 'UYU'][2:120], inf_usa)
plot(as.mcmc(inf_usa))
quantile(inf_usa, c(0.025, 0.975))
summary(inf_usa)

# Full P graph when varying u_i
min_ui = (min(inf_usa) + 0.9)*u0
max_ui = (max(inf_usa) + 1.5)*u0
delta = max_ui - min_ui
u_i = seq(from=min_ui, to=max_ui, by=0.01*delta)

p <- 0
for (i in 1:length(u_i)) {
  p[i] = mean(y_uy < u0*y0/u_i[i])
}
plot(u_i/u0 - 1.0, p)
abline(0.5, 0.0)
abline(0.05, 0.0)
abline(0.95, 0.0)
points(c(inflation_usa_1, inflation_usa_2, inflation_usa_3), c(0.05, 0.5, 0.95), col=34, pch='O')
text(c(inflation_usa_1, inflation_usa_2, inflation_usa_3) + 0.05, c(0.05, 0.5, 0.95), 
     labels= paste('x=',c(inflation_usa_1, inflation_usa_2, inflation_usa_3), ',\ny=', c(0.05, 0.5, 0.95)))
