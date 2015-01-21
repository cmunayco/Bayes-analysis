install.packages("devtools", quiet = TRUE)
install.packages("dplyr", quiet = TRUE)
install.packages("ggplot2", quiet = TRUE)
install.packages("scales", quiet = TRUE)

library(devtools) ##run this first
install_github("poissonconsulting/tulip@v0.0.11")
install_github("poissonconsulting/datalist@v0.4")
install_github("poissonconsulting/juggler@v0.1.3")
install_github("poissonconsulting/jaggernaut@v2.2.5")

##R Script Headers
library(dplyr)
library(ggplot2)
library(scales)
library(jaggernaut)
options(digits = 4)

graphics.off()
rm(list = ls())

library(jaggernaut)
model <- jags_model("model {
 bLambda ~ dlnorm(0,10^-2)
 for (i in 1:length(x)) {
   x[i]~dpois(bLambda)
 }
}")

summary(jags_analysis (model, data = data.frame(x = rpois(100,1))))

##Coin Flips
##Consider the case where n=10 flips of a coin produce y=3 tails. We can model this using a binomial distribution.
## y∼dbin(θ,n)

##where θ is the probability of throwing a head.

##Maximum Likelihood
##The likelihood for the binomial model is given by the following equation

## p(y|θ)=(n y)θy(1−θ)n−y

#The likelihood values for different values of θ are therefore as follows


likelihood <- function(theta, n = 10, y = 3) {
    choose(n, y) * theta^y * (1 - theta)^(n - y)
}
theta <- seq(from = 0, to = 1, length.out = 100)

quartz(width=10, height=6, pointsize=10)
qplot(theta, likelihood(theta), geom = "line", ylab = "Likelihood", xlab = "theta")

model1 <- jags_model("model { 
  theta ~ dunif(0, 1)
  y ~ dbin(theta, n)
}") 

data <- data.frame(n = 10, y = 3)
analysis1 <- jags_analysis(model1, data = data)

quartz(width=10, height=6, pointsize=10)
plot(analysis1)

coef(analysis1)


## Black Cherry Trees
## The trees data set in the dataset package provides information on the girth and volume of 31 black cherry trees.

quartz(width=10, height=6, pointsize=10)
qplot(x = Girth, y = Volume, data = trees)

model1 <- jags_model("model {
  alpha ~ dnorm(0, 50^-2) 
  beta ~ dnorm(0, 10^-2)
  sigma ~ dunif(0, 10)

  for(i in 1:length(Volume)) { 
    eMu[i] <- alpha + beta * Girth[i]
    Volume[i] ~ dnorm(eMu[i], sigma^-2)
  } 
}")

## Parallel Chains
if (getDoParWorkers() == 1) {
    registerDoParallel(4)
    opts_jagr(parallel = TRUE)
}

data(trees)
analysis1 <- jags_analysis(model1, data = trees)

quartz(width=10, height=6, pointsize=10)
plot(analysis1, parm = "alpha")

quartz(width=10, height=6, pointsize=10)
plot(analysis1, parm = "beta")

quartz(width=10, height=6, pointsize=10)
plot(analysis1, parm = "sigma")

coef(analysis1)

##Convergence

##Iterations
##Convergence can often be improved by simply increasing the number of iterations.


analysis1 <- jags_analysis(model1, data = trees, niters = 10^4)

quartz(width=10, height=6, pointsize=10)
plot(analysis1, parm = "alpha")


quartz(width=10, height=6, pointsize=10)
plot(analysis1, parm = "beta")

quartz(width=10, height=6, pointsize=10)
plot(analysis1, parm = "sigma")

## Chain Mixing

modify_data(model1) <- function (data) { print(data); data }

## Derived Parameters

derived_code <- "data {
  for(i in 1:length(Volume)) { 
    prediction[i] <- alpha + beta * Girth[i]

    simulated[i] ~ dnorm(prediction[i], sigma^-2)

    D_observed[i] <- log(dnorm(Volume[i], prediction[i], sigma^-2))
    D_simulated[i] <- log(dnorm(simulated[i], prediction[i], sigma^-2))
  }
  residual <- (Volume - prediction) / sigma
  discrepancy <- sum(D_observed) - sum(D_simulated)
}"

## Predictions

prediction <- predict(analysis1, newdata = "Girth", derived_code = derived_code)
simulated <- predict(analysis1, parm = "simulated", newdata = "Girth", 
                     derived_code = derived_code)

gp <- ggplot(data = prediction, aes(x = Girth, y = estimate))
gp <- gp + geom_point(data = dataset(analysis1), aes(y = Volume))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
gp <- gp + geom_line(data = simulated, aes(y = lower), linetype = "dotted")
gp <- gp + geom_line(data = simulated, aes(y = upper), linetype = "dotted")
gp <- gp + scale_y_continuous(name = "Volume")

quartz(width=10, height=6, pointsize=10)
print(gp)


## Residuals
fitted <- fitted(analysis1, derived_code = derived_code)
fitted$residual <- residuals(analysis1, derived_code = derived_code)$estimate

quartz(width=10, height=6, pointsize=10)
qplot(estimate, residual, data = fitted) + geom_hline(yintercept = 0) + 
    geom_smooth(se = FALSE)


## Posterior Predictive Checks
predictive_check(analysis1, derived_code = derived_code)
