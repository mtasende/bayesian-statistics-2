options(device="X11")
# install.packages("coda")
# install.packages("rjags")
library("boot")
data("urine")
?urine
head(urine)

dat = na.omit(urine)
dim(dat)
pairs(dat)

# Problem: "Variable selection"  => deal with colinearity

X = scale(dat[,-1], center=TRUE, scale=TRUE)
head(X)
colMeans(X)
apply(X, 2, sd) # Matrix, axis, function

# Show the double exponential distribution, and compare with a normal distribution
ddexp = function(x, mu, tau) {
  0.5 * tau * exp(-tau*abs(x-mu))
}
curve(ddexp(x, mu=0.0, tau=1.0), from=-5.0, to=5.0, ylab="density", main="Double exponential\ndistribution")
curve(dnorm(x, mean=0.0, sd=1.0), from=-5.0, to=5.0, lty=2, add=TRUE)
legend("topright", legend=c("double exponential", "normal"), lty=c(1,2), bty="n")

# JAGS model
library("rjags")
library("coda")

mod1_string = " model {
  for (i in 1:length(y)) {
    
    y[i] ~ dbern(p[i])
    
    logit(p[i]) = int + b[1]*gravity[i] + b[2]*ph[i] + b[3]*osmo[i] + b[4]*cond[i] + b[5]*urea[i] + b[6]*calc[i]
    
  }
  
  int ~ dnorm(0.0, 1.0/25.0)
  for (j in 1:6) {
    b[j] ~ ddexp(0.0, sqrt(2.0)) # has variance 1.0
  }
} "

set.seed(92)

data_jags = list(y=dat$r, gravity=X[,"gravity"], ph=X[,"ph"],
                 osmo=X[,"osmo"], cond=X[,"cond"], urea=X[,"urea"], calc=X[,"calc"])

params = c("int", "b")

mod1 = jags.model(textConnection(mod1_string), data=data_jags, n.chains=3)
update(mod1, 1e3)

mod1_sim = coda.samples(model=mod1, variable.names=params, n.iter=5e3)
mod1_csim = as.mcmc(do.call(rbind, mod1_sim))

# Convergence diagnostics
plot(mod1_sim, ask=TRUE)

gelman.diag(mod1_sim)
autocorr.diag(mod1_sim)
dev.off()
autocorr.plot(mod1_sim, ask = TRUE)
effectiveSize(mod1_sim)

## calculate DIC
dic1 = dic.samples(mod1, n.iter = 1e3)
dic1


par(mfrow=c(3,2))
densplot(mod1_csim[,1:6], xlim=c(-3.0,3.0))
colnames(X)

# Will keep gravity, cond and calc
# Also changing prior for b[i]


mod2_string = " model {
  for (i in 1:length(y)) {
    
    y[i] ~ dbern(p[i])
    
    logit(p[i]) = int + b[1]*gravity[i] + b[2]*cond[i] + b[3]*calc[i]
    
  }
  
  int ~ dnorm(0.0, 1.0/25.0)
  for (j in 1:3) {
    b[j] ~ dnorm(0.0, sqrt(2.0)) # has variance 1.0
  }
} "

mod2 = jags.model(textConnection(mod2_string), data=data_jags, n.chains=3)

update(mod2, 5e3)
mod2_sim = coda.samples(model=mod2, variable.names=params, n.iter=5e3)
mod2_csim = as.mcmc(do.call(rbind, mod2_sim))

# Convergence diagnostics
plot(mod2_sim, ask=TRUE)

gelman.diag(mod2_sim)
autocorr.diag(mod2_sim)
dev.off()
autocorr.plot(mod2_sim, ask = TRUE)
effectiveSize(mod2_sim)

## calculate DIC
dic2 = dic.samples(mod2, n.iter = 1e3)
dic2

summary(mod2_sim)

# Old estimations
colMeans(mod1_csim)
colnames(X)

# Predictions
pm_coef = colMeans(mod2_csim)
pm_coef

1.0 / (1.0 + exp(0.15))

# xi's are coded in "standard deviation" units from the mean
1.0 / (1.0 + exp(0.15 - 0.989*0.0 - -0.82*(-1.0) - 1.88*1.0))

pm_Xb = pm_coef["int"] + X[,c(1,4,6)] %*% pm_coef[1:3]
phat = 1.0 / (1.0 + exp(-pm_Xb))
head(phat)

plot(phat, jitter(dat$r))

tab0.5 = table(phat > 0.5, dat$r)
tab0.5
sum(diag(tab0.5)) / sum(tab0.5)

tab0.3 = table(phat > 0.3, dat$r)
tab0.3
sum(diag(tab0.3)) / sum(tab0.3)