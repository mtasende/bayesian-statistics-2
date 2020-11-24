rm(list=ls())
options(device="X11")
# options(device="RStudioGD")
setwd("/home/miguel/coursera/bayesian_statistics_2/capstone/data/preprocessed_data")
library(coda)
library(rjags)

dat = read.csv(file="data.csv", header=TRUE)
head(dat)
dim(dat)
unique(dat$region)

mod_string = " model {
for (i in 1:length(y)) {
y[i] ~ dnorm(mu[region[i]], prec[region[i]])
}

for (j in 1:max(region)) {
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

y = dat$rate_std * dat$cpi / dat$cpi_usa
data_jags = list(y=y, region=as.numeric(dat$region))

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
yhat = rep(pm_params[3:10], each=120)
resid = y - yhat
plot(resid)
plot(yhat, resid)

# What is the expectation of UYU? Is it under or over valued?
# (The numeric value for UYU is 49)
mean(mod_csim[,'mu[3]'])
y_uy = y[dat$symbol=='UYU']
y_uy[120] # Current value of UYU
y_lat = y[data_jags$region==3]
# Probability that it is overvalued
# Let's simulate y:
y_uy_csim = rnorm(nrow(mod_csim), mod_csim[,'mu[3]'], mod_csim[,'sig[3]'])
mean(y_uy_csim < y_uy[120])
plot(as.mcmc(y_uy_csim))
# Just to check, what would the naive probability calculation yield
mean(y < y_uy[120])

# Equally tailed interval for UYU mean given the cpi and cpi_usa values
summary(mod_sim) # Result: [0.9611060, 0.976896]
dat_uy = dat[dat$symbol=='UYU',]
dat_uy_now = dat_uy[nrow(dat_uy),]
dat_uy_base = dat_uy[dat_uy$date=='2010-08-15',]
ex_min = dat_uy_base$exchange_rate * 0.9611060 * dat_uy_now$cpi_usa / dat_uy_now$cpi
ex_max = dat_uy_base$exchange_rate * 0.976896 * dat_uy_now$cpi_usa / dat_uy_now$cpi
1/ex_min
1/ex_max

# What about for the actual value?
confidence_int = quantile(y_uy_csim, c(0.025, 0.975))
ex_min_y = dat_uy_base$exchange_rate * confidence_int[1] * dat_uy_now$cpi_usa / dat_uy_now$cpi
ex_max_y = dat_uy_base$exchange_rate * confidence_int[2] * dat_uy_now$cpi_usa / dat_uy_now$cpi
1/ex_min_y
1/ex_max_y

# Expected value
1/(dat_uy_base$exchange_rate * mean(mod_csim[,'mu[3]']) * dat_uy_now$cpi_usa / dat_uy_now$cpi)
