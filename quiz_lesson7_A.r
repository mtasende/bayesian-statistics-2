rm(list=ls()) # Clear all

# options(device = "RStudioGD")
options(device = "X11")

library("car")  # load the 'car' package
data("Anscombe")  # load the data set
?Anscombe  # read a description of the data
head(Anscombe)  # look at the first few lines of the data
pairs(Anscombe)  # scatter plots for each pair of variables

mod1 = lm(education ~ income + young + urban, data=Anscombe)
summary(mod1)

# Fitting with JAGS
library("rjags")

mod_string = " model {
for (i in 1:length(education)) {
education[i] ~ dnorm(mu[i], prec)
mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*urban[i]
}

b0 ~ dnorm(0.0, 1.0/1.0e6)
for (i in 1:3) {
b[i] ~ dnorm(0.0, 1.0/1.0e6)
}

prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
## Initial guess of variance based on overall
## variance of education variable. Uses low prior
## effective sample size. Technically, this is not
## a true 'prior', but it is not very informative.
sig2 = 1.0 / prec
sig = sqrt(sig2)
} "

data_jags = as.list(Anscombe)

set.seed(2017)

params = c("b", "sig")

inits = function() {
  inits = list("b"=rnorm(3, 0.0, 100.0), "prec"=rgamma(1, 1.0, 1.0))
}

mod = jags.model(textConnection(mod_string), data=data_jags, inits=inits, n.chains=3)

# Burn-in period
#update(mod1, 1000)

mod_sim = coda.samples(model=mod, variable.names=params, n.iter=2e5)

mod_csim = do.call(rbind, mod_sim)

### Convergence diagnostics
plot(mod_sim)
raftery.diag(mod_sim)
autocorr.diag(mod_sim)
effectiveSize(mod_sim)
gelman.diag(mod_sim)
summary(mod_sim)
summary(mod1)

plot(mod1)
