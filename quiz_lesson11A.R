options(device="X11")
setwd('/home/miguel/coursera/bayesian_statistics_2')
getwd()

dat = read.csv("pctgrowth.csv", header=TRUE)
head(dat)


## JAGS model
library("rjags")

mod_string = " model {
for (i in 1:length(y)) {
y[i] ~ dnorm(theta[grp[i]], prec)
}

for (j in 1:max(grp)) {
theta[j] ~ dnorm(mu, tau_prec)
}

mu ~ dnorm(0.0, 1e6)
tau_prec ~ dgamma(1.0/2.0, 3.0/2.0)
prec ~ dgamma(2.0/2.0, 2.0/2.0)
tau = sqrt(1/tau_prec)
sig = sqrt(1/prec)
} "

set.seed(2017)

data_jags = as.list(dat)
params = c("theta", "sig", "mu", "tau")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod, variable.names=params, n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

# Convergence
plot(mod_sim, ask=TRUE)
gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)
raftery.diag(mod_csim)

dic = dic.samples(mod, n.iter=1e3)
dic

summary(mod_sim)
means_theta = colMeans(mod_csim)[4:8]

# Now let's fit an ANOVA model
means_anova = tapply(dat$y, INDEX=dat$grp, FUN=mean)
plot(means_anova)
points(means_theta, col="red")
