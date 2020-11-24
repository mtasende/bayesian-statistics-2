rm(list=ls())

options(device = "X11")

library("car")  # load the 'car' package
data("Anscombe")  # load the data set
?Anscombe  # read a description of the data
head(Anscombe)  # look at the first few lines of the data
pairs(Anscombe)  # scatter plots for each pair of variables

Xc = scale(Anscombe, center=TRUE, scale=TRUE)
str(Xc)

data_jags = as.list(data.frame(Xc))

# Fitting with JAGS
library("rjags")

mod_string = " model {
for (i in 1:length(education)) {
education[i] ~ dnorm(mu[i], prec)
mu[i] = b[1]*income[i] + b[2]*young[i] + b[3]*urban[i]
}

for (i in 1:3) {
b[i] ~ ddexp(0.0, 1.0) # Has variance = 2
}

eff_sample_size = 1.0
initial_guess = 1.0
alpha = eff_sample_size / 2
beta = alpha * initial_guess
prec ~ dgamma(alpha, beta)
sig2 = 1.0 / prec
sig = sqrt(sig2)
} "

set.seed(2017)

params = c("b", "sig")

inits = function() {
  inits = list("b"=rnorm(3, 0.0, 100.0), "prec"=rgamma(1, 1.0, 1.0))
}

mod = jags.model(textConnection(mod_string), data=data_jags, inits=inits, n.chains=3)

# Burn-in period
update(mod, 1000)

mod_sim = coda.samples(model=mod, variable.names=params, n.iter=5e3)

mod_csim = do.call(rbind, mod_sim)

### Convergence diagnostics
plot(mod_sim)
raftery.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim, ask=TRUE)
effectiveSize(mod_sim)
gelman.diag(mod_sim)
summary(mod_sim)




