options(device="X11")
setwd('/home/miguel/coursera/bayesian_statistics_2')
getwd()

dat = read.table(file="cookies.dat", header=TRUE)
head(dat)
table(dat$location)
boxplot(chips ~ location, data=dat)

set.seed(112)
n_sim = 500

alpha_pri = rexp(n_sim, rate=1.0/2.0)
beta_pri = rexp(n_sim, rate=5.0)

mu_pri = alpha_pri / beta_pri
sig_pri = sqrt(alpha_pri / beta_pri^2)

summary(mu_pri)
summary(sig_pri)

lam_pri = rgamma(n_sim, shape=alpha_pri, rate=beta_pri)
summary(lam_pri)

y_pri = rpois(n_sim, lam_pri)
summary(y_pri)

lam_pri = lam_pri[1:5]
y_pri = rpois(150, lambda=rep(lam_pri, each=30))
y_pri

## JAGS model
library("rjags")

mod_string = " model {
  for (i in 1:length(chips)) {
    chips[i] ~ dpois(lam[location[i]])
  }
  
  for (j in 1:max(location)) {
    lam[j] ~ dgamma(alpha, beta)
  }
  
  mu ~ dgamma(2.0, 1.0/5.0)
  sig ~ dexp(1.0)
  alpha = mu^2 / sig^2
  beta = mu / sig^2
} "

set.seed(113)

data_jags = as.list(dat)
params = c("lam", "mu", "sig")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod, variable.names=params, n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

# Convergence (TODO)
plot(mod_sim, ask=TRUE)
gelman.diag(mod_sim)
autocorr.diag(mod_sim)
effectiveSize(mod_sim)
raftery.diag(mod_csim)

dic = dic.samples(mod, n.iter=1e3)
dic

## Model checking
pm_params = colMeans(mod_csim)
yhat = rep(pm_params[1:5], each=30)
resid = dat$chips - yhat

plot(resid)
plot(jitter(yhat), resid)

lam_resid = pm_params[1:5] - pm_params["mu"]

plot(lam_resid)
abline(h=0, lty=2)


summary(mod_sim)

# Predict things
n_sim = nrow(mod_csim)

post_alpha = mod_csim[,"mu"]^2 / mod_csim[,"sig"]^2
post_beta = mod_csim[,"mu"] / mod_csim[,"sig"]^2
lam_pred = rgamma(n_sim, shape=post_alpha, rate=post_beta)

hist(lam_pred)
mean(lam_pred > 15)

y_pred = rpois(n_sim, lambda=lam_pred)
hist(y_pred)
mean(y_pred>15)

hist(dat$chips)

y_pred1 = rpois(n_sim, lambda=mod_csim[,"lam[1]"])
hist(y_pred1)
mean(y_pred1 <7)
