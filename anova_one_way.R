options(device="X11")
data("PlantGrowth")
?PlantGrowth
head(PlantGrowth)
boxplot(weight ~ group, data=PlantGrowth)

lmod = lm(weight ~ group, data=PlantGrowth)
summary(lmod)
anova(lmod)

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

plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)

pm_params = colMeans(mod_csim)
pm_params
coefficients(lmod)

yhat = pm_params[1:3][data_jags$grp] # Sets the prediction for each sample as the group mean for the sample's group
resid = data_jags$y -yhat

plot(resid)
plot(yhat, resid)

summary(mod_sim)
HPDinterval(mod_csim)
HPDinterval(mod_csim, 0.9)

mean(mod_csim[,"mu[3]"] > mod_csim[,"mu[1]"] ) # Proba of any increase
mean(mod_csim[,"mu[3]"] > mod_csim[,"mu[1]"]*(1.1) ) # Proba of an increase of at least 10%
