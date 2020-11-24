rm(list=ls())
options(device="X11")
# options(device="RStudioGD")
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

for (j in 1:51) {
mu[j] ~ dnorm(0.0, 1.0/1.0e6)
prec[j] ~ dgamma(5/2.0, 5*1.0/2.0)
sig[j] = sqrt(1.0/prec[j])
}
} "

set.seed(2017)
str(dat)

y = dat$rate_std * dat$cpi / dat$cpi_usa
data_jags = list(y=y, symbol=as.numeric(dat$symbol))

params = c("mu", "sig")

inits = function() {
  inits = list("mu"=rnorm(51,0.0,100.0), "prec"=rgamma(51,1.0,1.0))
}

mod = jags.model(textConnection(mod_string), data=data_jags, inits=inits, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod, variable.names=params, n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

### Convergence diagnostics
plot(mod_sim, ask = TRUE)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)
raftery.diag(mod_sim)
gelman.diag(mod_sim, multivariate=FALSE)
summary(mod_sim)

dic.samples(mod, n.iter = 1e3)

# Residuals Analysis
pm_params = colMeans(mod_csim)
yhat = rep(pm_params[1:51], each=120)
resid = y - yhat
plot(resid)
plot(yhat, resid)

# What is the expectation of UYU? Is it under or over valued?
# (The numeric value for UYU is 49)
mean(mod_csim[,'mu[49]'])
y_uy = y[dat$symbol=='UYU']
y_uy[120] # Current value of UYU
# Probability that it is overvalued
# Let's simulate y:
y_uy_csim = rnorm(nrow(mod_csim), mod_csim[,'mu[49]'], mod_csim[,'sig[49]'])
mean(y_uy_csim < y_uy[120])
plot(as.mcmc(y_uy_csim))
# Just to check, what would the naive probability calculation yield
mean(y < y_uy[120])

# Equally tailed interval for UYU mean given the cpi and cpi_usa values
summary(mod_sim) # Result: [0.9843, 1.0734]
dat_uy = dat[dat$symbol=='UYU',]
dat_uy_now = dat_uy[nrow(dat_uy),]
dat_uy_base = dat_uy[dat_uy$date=='2010-08-15',]
ex_min = dat_uy_base$exchange_rate * 0.9843 * dat_uy_now$cpi_usa / dat_uy_now$cpi
ex_max = dat_uy_base$exchange_rate * 1.0734 * dat_uy_now$cpi_usa / dat_uy_now$cpi
1/ex_min
1/ex_max

# What about for the actual value?
confidence_int = quantile(y_uy_csim, c(0.025, 0.975))
ex_min_y = dat_uy_base$exchange_rate * confidence_int[1] * dat_uy_now$cpi_usa / dat_uy_now$cpi
ex_max_y = dat_uy_base$exchange_rate * confidence_int[2] * dat_uy_now$cpi_usa / dat_uy_now$cpi
1/ex_min_y
1/ex_max_y

1/(dat_uy_base$exchange_rate * mean(mod_csim[,'mu[49]']) * dat_uy_now$cpi_usa / dat_uy_now$cpi)
