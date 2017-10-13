# Normal distribution

library(smoothmest)
library(moments)


par(mfrow = c(4,2))
set.seed(123)
histogram(rnorm(1000,3, .25),  main="Normal distribution")
skewness(rnorm(1000,3, .25)) #0
kurtosis(rnorm(1000,3, .25)) #3

set.seed(123)
histogram(rdoublex(1:1000,lambda=2), main = "Double exponential distribution")
skewness(rdoublex(1:1000,lambda=2)) #2
kurtosis(rdoublex(1:1000,lambda=2)) #10

set.seed(123)
histogram(rcauchy(1000, location = 0, scale = 1), main = "Cauchy distribution")
skewness(rcauchy(1000, location = 0, scale = 1)) #29
kurtosis(rcauchy(1000, location = 0, scale = 1)) #503

set.seed(123)
histogram(rpois(1000, lambda = 3), main = "Poisson distribution")
skewness(rpois(1000, lambda = 3)) #0.6
kurtosis(rpois(1000, lambda = 3)) #3

set.seed(123)
histogram(rbinom(1000, 20, 0.2), main = "Binomial distribution")
skewness(rbinom(1000, 20, 0.2)) #0.4
kurtosis(rbinom(1000, 20, 0.2)) #3

set.seed(123)
histogram(rchisq(1000,1, ncp = 1))
skewness(rchisq(1000,1000, ncp = 1)) #0
kurtosis(rchisq(1000,1000, ncp = 1)) #3

library(LambertW)
set.seed(123)
histogram(rLambertW(n=1000, distname = "normal", beta = c(0,1), delta = 0.5))
skewness(rLambertW(n=1000, distname = "normal", beta = c(0,1), delta = 0.5)) #6
kurtosis(rLambertW(n=1000, distname = "normal", beta = c(0,1), delta = 0.5)) #143
