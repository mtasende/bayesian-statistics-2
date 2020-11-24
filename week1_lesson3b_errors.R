# Calculating the errors in the Monte Carlo estimation
set.seed(32)

m = 10000
a = 2.0
b = 1.0 / 3.0

theta = rgamma(n=m, shape=a, rate=b)

std_error = sd(theta) / sqrt(m)

# Confidence interval (mean of theta with normal distribution)
mean(theta) - 2.0*std_error
mean(theta) + 2.0*std_error

ind = theta < 5
mean(ind)
pgamma(0.5, shape=a, rate=b)

se = sd(ind) / sqrt(m)
2*se
