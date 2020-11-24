options(device="X11")
rm(list=ls())

library("MASS")
data("OME")
?OME # background on the data
head(OME)

any(is.na(OME)) # check for missing values
dat = subset(OME, OME != "N/A") # manually remove OME missing values identified with "N/A"
dat$OME = factor(dat$OME)
dat$ID = as.numeric(factor(dat$ID))
str(dat)

#Original reference model
mod_glm = glm(Correct/Trials ~ Age + OME + Loud + Noise, data=dat,
              weights=Trials, family="binomial")
X = model.matrix(mod_glm)[,-1]


# Jags mdel
library("rjags")
mod_string = " model {
for (i in 1:length(y)) {
y[i] ~ dbin(phi[i], n[i])
logit(phi[i]) = a[id[i]] + b[1]*Age[i] + b[2]*OMElow[i] + b[3]*Loud[i] + b[4]*Noiseincoherent[i]
}

for (j in 1:max(id)) {
  a[j] ~ dnorm(mu, prec_a)
}
  
mu ~ dnorm(0.0, 1.0/1.0e2)
prec_a ~ dgamma(1/2.0, 1.0/2.0)
tau = sqrt(1.0/prec_a)

b0 ~ dnorm(0.0, 1.0/5.0^2)
for (j in 1:4) {
b[j] ~ dnorm(0.0, 1.0/4.0^2)
}

} "

data_jags = as.list(as.data.frame(X))
data_jags$y = dat$Correct # this will not work if there are missing values in dat (because they would be ignored by model.matrix). Always make sure that the data are accurately pre-processed for JAGS.
data_jags$n = dat$Trials
data_jags$id = dat$ID
str(data_jags) # make sure that all variables have the same number of observations (712).
params = c("a","b","mu","tau")

library("coda")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
mod_sim = coda.samples(model=mod, variable.names=params, n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

# Convergence diagnostics
plot(mod_sim, ask=TRUE)

autocorr.diag(mod_sim)
autocorr.plot(mod_sim, ask = TRUE)
raftery.diag(mod_csim)
effectiveSize(mod_sim)
gelman.diag(mod_sim)

dic.samples(mod, n.iter=1e3)





