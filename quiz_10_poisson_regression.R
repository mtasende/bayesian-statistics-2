rm(list=ls())
options(device="X11")

#install.packages("COUNT")
library("COUNT")
data("badhealth")
?badhealth

head(badhealth)
any(is.na(badhealth))

hist(badhealth$numvisit, breaks=20)
min(badhealth$numvisit)
sum(badhealth$numvisit == 0) # Zero values can't be included because of the "log"

plot(jitter(log(badhealth$numvisit)) ~ jitter(age), 
     data=badhealth, 
     subset = badh==0&numvisit>0,
     xlab="age",
     ylab="log(visits)")
points(jitter(log(badhealth$numvisit)) ~ jitter(age), 
     data=badhealth, 
     subset = badh==1&numvisit>0,
     xlab="age",
     ylab="log(visits)", col="red")

library(rjags)

mod_string = " model {
  for (i in 1:length(numvisit)) {
    numvisit[i] ~ dpois(lam[i])
    log(lam[i]) = int + b_badh*badh[i] + b_age*age[i] + b_intx*age[i]*badh[i]
  }
  
  int ~ dnorm(0.0, 1.0/1e6)
  b_badh ~ dnorm(0.0, 1.0/1e4)
  b_age ~ dnorm(0.0, 1.0/1e4)
  b_intx ~ dnorm(0.0, 1.0/1e4)
} "

set.seed(102)

data_jags = as.list(badhealth)

params = c("int", "b_badh", "b_age", "b_intx")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod, variable.names=params, n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

## convergence diagnostics
plot(mod_sim, ask=TRUE)

gelman.diag(mod_sim)

autocorr.diag(mod_sim)
autocorr.plot(mod_sim, ask=TRUE)
effectiveSize(mod_sim)

raftery.diag(mod_sim)

## compute DIC
dic = dic.samples(mod, n.iter=1e3)
dic


### Residuals
X = as.matrix(badhealth[,-1])
X = cbind(X, with(badhealth, badh*age))
X
head(X)
tail(X)

pmed_coef = apply(mod_csim, 2, median) # Posterior median
pmed_coef

llam_hat = pmed_coef["int"] + X %*% pmed_coef[c("b_badh", "b_age", "b_intx")]
lam_hat = exp(llam_hat)
resid = badhealth$numvisit - lam_hat

plot(resid) # The data was previously ordered

plot(lam_hat[which(badhealth$badh==0)], resid[which(badhealth$badh==0)], xlim=c(0,8), ylim=range(resid))
points(lam_hat[which(badhealth$badh==1)], resid[which(badhealth$badh==1)], xlim=c(0,8), col="red")

var(resid[which(badhealth$badh==0)])
var(resid[which(badhealth$badh==1)])


summary(mod_sim)

### Posterior probability that x2 has more visits than x1
# good health, 35 years old, 
x1 = c(0,35,0)
# bad health, 35 years old, 
x2 = c(1,35,35)

head(mod_csim)
loglam1 = mod_csim[,"int"] + mod_csim[,c(2,1,3)] %*% x1
loglam2 = mod_csim[,"int"] + mod_csim[,c(2,1,3)] %*% x2

lam1 = exp(loglam1)
lam2 = exp(loglam2)

plot(density(lam1))
n_sim = length(lam1)
n_sim

y1 = rpois(n_sim, lam1)
y2 = rpois(n_sim, lam2)

plot(table(factor(y1, levels=0:18))/n_sim)
points(table(y2 + 0.1)/n_sim, col="red")

mean(y2 > y1)


