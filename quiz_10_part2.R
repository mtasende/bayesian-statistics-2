options(device="X11")
setwd("/home/miguel/coursera/bayesian_statistics_2")

dat = read.csv(file="callers.csv", header=TRUE)
head(dat)
pairs(dat)

boxplot(calls/days_active~isgroup2, data=dat, ylab="calls / days_active", xlab="Is group 2?")

library(rjags)

mod_string = " model {
  for (i in 1:length(calls)) {
    calls[i] ~ dpois(lam[i] * days_active[i])
    log(lam[i]) = b0 + b[1]*age[i] + b[2]*isgroup2[i]
  }

  b0 ~ dnorm(0.0, 1.0/1e2)
  for (i in 1:2) {
    b[i] ~ dnorm(0.0, 1.0/1e2)
  }
} "

# dat_rate = cbind(dat[,1]/dat[,2], dat[,3:4])
# colnames(dat_rate)[1] = "rate"
# dat_rate

jags_data = as.list(dat)
mod = jags.model(textConnection(mod_string), data=jags_data, n.chains=3)

params = c("b0", "b")
mod_sim = coda.samples(model=mod, variable.names=params, n.iter=1e5)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

# Burn out
update(mod, 1e3)

### Convergence analysis
plot(mod_sim)

gelman.diag(mod_sim)

autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)
raftery.diag(mod_csim)

head(mod_csim)
mean(mod_csim[,"b[2]"] > 0.0)

mean_coefs = colMeans(mod_csim)
lambdas = exp(mean_coefs["b0"] + mean_coefs["b[1]"]*dat[,"age"] + mean_coefs["b[2]"]*dat[,"isgroup2"])

# 29 year old customer, group2, 30 days active => proba calls >= 3
lambda = exp(mean_coefs["b0"] + mean_coefs["b[1]"]*29 + mean_coefs["b[2]"]*1)
proba = 1 - ppois(2, lambda * 30)
proba
