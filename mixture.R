options(device="X11")
rm(list=ls())

setwd('/home/miguel/coursera/bayesian_statistics_2')
dat = read.csv("mixture.csv")
head(dat)

y = dat$X.2.26617489046013

hist(y)
plot(density(y))

library("rjags")

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

set.seed(11)

data_jags = list(y=y)

params = c("mu", "sig", "omega", "z[1]", "z[31]", "z[49]", "z[6]")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod, variable.names=params, n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

# Convergence
plot(mod_sim, ask=TRUE)
summary(mod_sim)

par(mfrow=c(2,2))
densplot(mod_csim[,c("z[1]", "z[31]", "z[49]", "z[6]")])

