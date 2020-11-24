options(device="X11")
data("PlantGrowth")

library("rjags")
library("coda")
mod_string = " model {
  for (i in 1:length(y)) {
    y[i] ~ dnorm(mu[grp[i]], prec)
  }
  
  for (j in 1:3) {
    mu[j] ~ dnorm(0.0, 1.0/1.0e6)
  }
  
  prec ~ dgamma(5/2.0, 5*1.0/2.0)
  sig = sqrt(1.0/prec)
} "

set.seed(82)
str(PlantGrowth)
data_jags = list(y=PlantGrowth$weight, 
                 grp=as.numeric(PlantGrowth$group))

params = c("mu", "sig")

inits = function() {
  inits = list("mu"=rnorm(3, 0.0, 100.0), "prec"=rgamma(1,1.0,1.0))
}

mod = jags.model(textConnection(mod_string),
                 data=data_jags,
                 inits=inits,
                 n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

summary(mod_sim)
dic1 = dic.samples(mod, n.iter = 1e5)

HPDinterval(mod_csim[,"mu[3]"] - mod_csim[,"mu[1]"] , 0.95)

mod_cm = lm(weight ~ -1 + group, data=PlantGrowth)
summary(mod_cm)
