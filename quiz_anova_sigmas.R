options(device="X11")
data("PlantGrowth")

library("rjags")
library("coda")
mod2_string = " model {
for (i in 1:length(y)) {
y[i] ~ dnorm(mu[grp[i]], prec[grp[i]])
}

for (j in 1:3) {
mu[j] ~ dnorm(0.0, 1.0/1.0e6)
prec[j] ~ dgamma(5/2.0, 5*1.0/2.0)
sig[j] = sqrt(1.0/prec[j])
}
} "

set.seed(82)
str(PlantGrowth)
data_jags = list(y=PlantGrowth$weight, 
                 grp=as.numeric(PlantGrowth$group))

params2 = c("mu", "sig")

inits2 = function() {
  inits = list("mu"=rnorm(3, 0.0, 100.0), "prec"=rgamma(3,1.0,1.0))
}

mod2 = jags.model(textConnection(mod2_string),
                 data=data_jags,
                 inits=inits2,
                 n.chains=3)
update(mod2, 1e3)

mod2_sim = coda.samples(model=mod2,
                       variable.names=params2,
                       n.iter=5e3)
mod2_csim = as.mcmc(do.call(rbind, mod2_sim))

summary(mod2_sim)
dic2 = dic.samples(mod2, n.iter = 1e5)
