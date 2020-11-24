rm(list=ls())
# options(device="X11")
options(device="RStudioGD")
setwd("/home/miguel/coursera/bayesian_statistics_2/capstone/data/preprocessed_data")
library(coda)
library(rjags)

dat = read.csv(file="data.csv", header=TRUE)
head(dat)

sdat <- dat[sample(nrow(dat)),]
plot(as.mcmc(sdat['exchange_rate']))

sdat['quotient'] = (sdat['cpi_usa'] / sdat['cpi'])
plot(as.mcmc(sdat['quotient']))

plot(as.mcmc(sdat['rate_std']))

plot(as.mcmc(sdat['rate_std'] - sdat['quotient']))

sdat['rate_div_cpi_quotient'] = (sdat['rate_std'] / sdat['quotient'])
plot(as.mcmc(sdat['rate_div_cpi_quotient']))

boxplot(rate_std - quotient ~ symbol, data=sdat, ylab="rate_std - cpi quotient", xlab="Currency code")
boxplot(rate_std / quotient ~ symbol, data=sdat, ylab="rate_std / cpi quotient", xlab="Currency code")
plot(sdat$quotient, sdat$rate_std)

# First let's test a simple linear model, just to have an idea of the value of b
mod_lm = lm(rate_std ~ quotient, data=sdat)
summary(mod_lm)

plot(residuals(mod_lm))
plot(fitted(mod_lm), residuals(mod_lm))

plot(as.mcmc(sdat['rate_std'] - fitted(mod_lm)))
# (That looks like a gaussian mixture of 2 gaussians)

# Let's try with a gaussian mixture
mod_string = " model {
  for (i in 1:length(y)) {
y[i] ~ dnorm(mu[z[i]], prec)
z[i] ~ dcat(omega)
}

mu[1] ~ dnorm(-1.0, 1.0/100.0)
mu[2] ~ dnorm(1.0, 1.0/100.0) T(mu[1],)

prec ~ dgamma(1.0/2.0, 1.0*1.0/2.0)
sig = sqrt(1.0/prec)

omega ~ ddirich(c(1.0, 1.0))
} "

y = sdat$rate_std - fitted(mod_lm)
data_jags = list(y=y)

params = c("mu", "sig", "omega")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod, variable.names=params, n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

### Convergence diagnostics
plot(mod_sim, ask = TRUE)
raftery.diag(mod_sim)
autocorr.diag(mod_sim)
effectiveSize(mod_sim)
gelman.diag(mod_sim, multivariate=FALSE)
summary(mod_sim)

dic.samples(mod, n.iter = 1e3)

