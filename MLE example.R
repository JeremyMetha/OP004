# Maximum-Likelihood Estimation (MLE) is a statistical technique for estimating model parameters. It basically sets out to answer the question: what model parameters are most likely to characterise a given set of data? First you need to select a model for the data. And the model must have one or more (unknown) parameters. As the name implies, MLE proceeds to maximise a likelihood function, which in turn maximises the agreement between the model and the data.
#
# Most illustrative examples of MLE aim to derive the parameters for a probability density function (PDF) of a particular distribution. In this case the likelihood function is obtained by considering the PDF not as a function of the sample variable, but as a function of distribution’s parameters. For each data point one then has a function of the distribution’s parameters. The joint likelihood of the full data set is the product of these functions. This product is generally very small indeed, so the likelihood function is  normally replaced by a log-likelihood function. Maximising either the likelihood or log-likelihood function yields the same results, but the latter is just a little more tractable!
#
#   Fitting a Normal Distribution
# Let’s illustrate with a simple example: fitting a normal distribution. First we generate some data.

set.seed(1001)
N <- 100
x <- rnorm(N, mean = 3, sd = 2)
mean(x)
sd(x)

# Then we formulate the log-likelihood function.

LL <- function(mu, sigma) {
  R = dnorm(x, mu, sigma)
  -sum(log(R))
  }
# And apply MLE to estimate the two parameters (mean and standard deviation) for which the normal distribution best describes the data.

library(stats4)]
mle(LL, start = list(mu = 1, sigma=1))

#Those warnings are a little disconcerting! They are produced when negative values are attempted for the standard deviation.

dnorm(x, 1, -1)

# There are two ways to sort this out. The first is to apply constraints on the parameters. The mean does not require a constraint but we insist that the standard deviation is positive.

mle(LL, start = list(mu = 1, sigma=1), method = "L-BFGS-B", lower = c(-Inf, 0), upper = c(Inf, Inf))

# This works because mle() calls optim(), which has a number of optimisation methods. The default method is BFGS. An alternative, the L-BFGS-B method, allows box constraints.

# The other solution is to simply ignore the warnings. It’s neater and produces the same results.

LL <- function(mu, sigma) {
  R = suppressWarnings(dnorm(x, mu, sigma))
  -sum(log(R))
  }
mle(LL, start = list(mu = 1, sigma=1))

# The maximum-likelihood values for the mean and standard deviation are damn close to the corresponding sample statistics for the data. Of course, they do not agree perfectly with the values used when we generated the data: the results can only be as good as the data. If there were more samples then the results would be closer to these ideal values.

# A note of caution: if your initial guess for the parameters is too far off then things can go seriously wrong!

mle(LL, start = list(mu = 0, sigma=1))

# Fitting a Linear Model
# Now let’s try something a little more sophisticated: fitting a linear model. As before, we generate some data.

x <- runif(N)
y <- 5 * x + 3 + rnorm(N)
# We can immediately fit this model using least squares regression.

fit <- lm(y ~ x)
summary(fit)

# The values for the slope and intercept are very satisfactory. No arguments there. We can superimpose the fitted line onto a scatter plot.

plot(x, y)
abline(fit, col = "red")
# ml-estimation-scatter-fit

# Pushing on to the MLE for the linear model parameters. First we need a likelihood function. The model is not a PDF, so we can’t proceed in precisely the same way that we did with the normal distribution. However, if you fit a linear model then you want the residuals to be normally distributed. So the likelihood function fits a normal distribution to the residuals.

LL <- function(beta0, beta1, mu, sigma) {
  # Find residuals
  R = y - x * beta1 - beta0
  # Calculate the likelihood for the residuals (with mu and sigma as parameters)
  R = suppressWarnings(dnorm(R, mu, sigma))
  # Sum the log likelihoods for all of the data points
  -sum(log(R))
}
# One small refinement that one might make is to move the logarithm into the call to dnorm().

LL <- function(beta0, beta1, mu, sigma) {
  R = y - x * beta1 - beta0
  R = suppressWarnings(dnorm(R, mu, sigma, log = TRUE))
  -sum(R)
}
# It turns out that the initial guess is again rather important and a poor choice can result in errors. We will return to this issue a little later.

fit <- mle(LL, start = list(beta0 = 3, beta1 = 1, mu = 0, sigma=1))

fit <- mle(LL, start = list(beta0 = 5, beta1 = 3, mu = 0, sigma=1))

# But if we choose values that are reasonably close then we get a decent outcome.

fit <- mle(LL, start = list(beta0 = 4, beta1 = 2, mu = 0, sigma=1))
fit

# The maximum-likelihood estimates for the slope (beta1) and intercept (beta0) are not too bad. But there is a troubling warning about NANs being produced in the summary output below.

summary(fit)
# It stands to reason that we actually want to have the zero mean for the residuals. We can apply this constraint by specifying mu as a fixed parameter. Another option would be to simply replace mu with 0 in the call to dnorm(), but the alternative is just a little more flexible.

fit <- mle(LL, start = list(beta0 = 2, beta1 = 1.5, sigma=1), fixed = list(mu = 0), nobs = length(y))
summary(fit)
# The resulting estimates for the slope and intercept are rather good. And we have standard errors for these parameters as well.

# How about assessing the overall quality of the model? We can look at the Akaike Information Criterion (AIC) and Bayesian Information Criterion (BIC). These can be used to compare the performance of different models for a given set of data.

AIC(fit)

BIC(fit)

logLik(fit)

# Returning now to the errors mentioned above. Both of the cases where the call to mle() failed resulted from problems with inverting the Hessian Matrix. With the implementation of mle() in the stats4 package there is really no way to get around this problem apart from having a good initial guess. In some situations though, this is just not feasible. There are, however, alternative implementations of MLE which circumvent this problem. The bbmle package has mle2() which offers essentially the same functionality but includes the option of not inverting the Hessian Matrix.

library(bbmle)
fit <- mle2(LL, start = list(beta0 = 3, beta1 = 1, mu = 0, sigma = 1))
summary(fit)

# Here mle2() is called with the same initial guess that broke mle(), but it works fine. The summary information for the optimal set of parameters is also more extensive.

# Fitting a linear model is just a toy example. However, Maximum-Likelihood Estimation can be applied to models of arbitrary complexity. If the model residuals are expected to be normally distributed then a log-likelihood function based on the one above can be used. If the residuals conform to a different distribution then the appropriate density function should be used instead of dnorm().

# generate data
data.x <- rnorm(n = 100, mean = 10, sd = 2)

# create true model

a.true <- 3
b.true <- 8

true.y <- data.x * a.true + b.true


# create noise

err.sd.true <- 1  # Set noise sd
noise <- rnorm(n = 100, mean = 0, sd = 2)  # Generate noise
data.y <- true.y + noise  # Add noise to true (latent) responses

# plot

plot(data.x, data.y)


# ML loss function - taking a decision theoretic approach

lm.loss <- function(par) {

  a.par <- par[1]  # The current slope
  b.par <- par[2]  # The current intercept
  err.sigma <- par[3]  # The current error standard deviation

  # If the error standard deviation is invalid (i.e.; negative), then we need to return a very high deviance
  # This will tell the optimization procedure to stay away from invalid (either statistically or psychologically)
  #   parameter values.

  if(err.sigma < 0) {deviance <- 10000000}

  # If the error standard deviation is valid (i.e.; > 0), then calculate the deviance...

  if(err.sigma > 0) {

    # Calculate the likelihood of each data point.
    # Here is where you specify the model and how you calculate likelihoods.

    likelihoods <- dnorm(data.y, mean = data.x * a.par + b.par, sd = err.sigma)

    # Now let's convert the vector of likelihoods to a summary deviance score (-2 times sub log-lik)

    # Calculate log-likelihoods of each data point
    log.likelihoods <- log(likelihoods)

    # Calculate the deviance (-2 times sum of log-likelihoods)
    deviance <- -2 * sum(log.likelihoods)

  }

  return(deviance)

}

# sanity check 1, far off values

dev.temp <- lm.loss(c(1, 5, 20))
dev.temp # print value


# sanity check 2, closer values

dev.temp <- lm.loss(c(3.2, 7.5, 2))
dev.temp


# minimizinng the loss with optim()

parameter.fits <- optim(par = c(0, 0, 10),
                        fn = lm.loss, hessian = T
)

parameter.fits

# ci's based on hessian - no idea how this works...

hessian <- parameter.fits$hessian
hessian.inv <- solve(hessian)
parameter.se <- sqrt(diag(hessian.inv))
parameter.se


# This tells us  the standard error of the slope (a) the standard error for the intercept (b), and the standard error for the error (σ) parameter. We can now calculate 95% confidence intervals for each parameter by adding and subracting 1.96 times the standard error from the ML estimates:

CI.matrix <- as.data.frame(matrix(NA, nrow = 3, ncol = 3))

CI.matrix[1,] <- parameter.fits$par
CI.matrix[2,] <- parameter.fits$par - 1.96 * parameter.se
CI.matrix[3,] <- parameter.fits$par + 1.96 * parameter.se
names(CI.matrix) <- c("a", "b", "sigma")
rownames(CI.matrix) <- c("ML", "95% Lower bound", "95% Upper bound")

CI.matrix

# compare to the built-in lm in R

built.in <- lm(data.y ~ data.x)
built.in


# a new model - the propertional difference model

# visualisation

pd.lik <- function(x) {pnorm(x, mean = 0, sd = 1)}
plot(1, xlim = c(-2, 2), ylim = c(0, 1),
     type = "n", xlab = expression(d - delta), ylab = "p(Choose x)")

rect(-100, -100, 100, 100, col = gray(.95, alpha = .8))
abline(h = seq(0, 1, .2), col = gray(1, alpha = .8), lwd = 3)
abline(h = seq(.1, 1.1, .2), col = gray(1, alpha = .8), lwd = 1)
abline(v = seq(-2, 2, .5), col = gray(1, alpha = .8), lwd = 3)
abline(v = seq(-2.25, 2.25, .5), col = gray(1, alpha = .8), lwd = 1)

curve(pd.lik, from = -2, to = 2, add = T, lwd = 2)


# step zero, build stimuli

n.stim <- 200

option1 <- data.frame(a = sample(1:10, size = n.stim, replace = T),
                      p = sample(50:55, size = n.stim, replace = T))

option2 <- data.frame(b = sample(1:10, size = n.stim, replace = T),
                      q = sample(50:55, size = n.stim, replace = T))


library(matrixStats)

den.ab <- rowMaxs(cbind(option1$a, option2$b))
den.pq <- rowMaxs(cbind(option1$p, option2$q))

d <- ((option1$a - option2$b) / den.ab - (option1$p - option2$q) / den.pq)

pd.stimuli <- cbind(option1, option2, d)
head(pd.stimuli)

# step 1, simulate data

true.delta <- .2
err.sd <- 1

p.choose.x <- pnorm(pd.stimuli$d - true.delta, mean = 0, sd = 1)


choices <- sapply(1:length(p.choose.x), function(x) {

  prob <- p.choose.x[x]
  outcome <- sample(c("x", "y"), size = 1, replace = 1, prob = c(prob, 1 - prob))

})


# step 2, define loss function

pd.loss <- function(par) {

  delta <- par[1]
  err.sigma <- par[2]


  # If the error standard deviation is invalid (i.e.; negative), then we need to return a very high deviance
  # This will tell the optimization procedure to stay away from invalid (either statistically or psychologically)
  #   parameter values. I will restrict delta to be between -5 and + 5, and err.sigma to be greater than 0:

  if(err.sigma < 0 | delta > 5 | delta < -5) {

    deviance <- 1000000

  }


  # If the parameters are valid, then calculate the deviance...

  if(err.sigma > 0) {

    # Now, calculate the likelihood of choosing option x for each stimuli

    p.choose.x <- pnorm(pd.stimuli$d - delta,
                        mean = 0,
                        sd = err.sigma
    )

    # Likelihood of choosing y is just 1 - p(choose x)

    p.choose.y <- 1 - p.choose.x

    # Now calculate the likelihood of each observed choice

    likelihoods <- rep(NA, length(choices))  # Set up the vector
    likelihoods[choices == "x"] <- p.choose.x[choices == "x"]  # Likelihoods for x choices
    likelihoods[choices == "y"] <- p.choose.y[choices == "y"]  # Likelihoods for y choices


    # Because we don't like likelihoods of 0 (which should rarely happen), convert 0s to a very small number
    likelihoods[likelihoods == 0] <- .0001

    # Now let's convert the vector of likelihoods to a summary deviance score (-2 times sub log-lik)

    # Calculate log-likelihoods of each data point
    log.likelihoods <- log(likelihoods)

    # Calculate the deviance (-2 times sum of log-likelihoods)
    deviance <- -2 * sum(log.likelihoods)

  }


  return(deviance)

}

# step 3, optimise

parameter.fits <- optim(par = c(0, 1),
                        fn = pd.loss, hessian = T
)

parameter.fits


# confints

hessian <- parameter.fits$hessian
hessian.inv <- solve(hessian)
parameter.se <- sqrt(diag(hessian.inv))

CI.matrix <- as.data.frame(matrix(NA, nrow = 3, ncol = 2))

CI.matrix[1,] <- parameter.fits$par
CI.matrix[2,] <- parameter.fits$par - 1.96 * parameter.se
CI.matrix[3,] <- parameter.fits$par + 1.96 * parameter.se
names(CI.matrix) <- c("delta", "sigma")
rownames(CI.matrix) <- c("ML", "95% Lower bound", "95% Upper bound")

CI.matrix

# checking parameter recovery

n.stim.seq <- c(100)   # Number of stimuli for each participant
true.delta.seq <- seq(-1, 1, .25) # Participant delta
err.sd <- 1 # Error sd
n.sim <- 100 # Number of simulations

library(matrixStats)

result.df <- expand.grid(true.delta = true.delta.seq,
                         n.stim = n.stim.seq,
                         sim = 1:n.sim,
                         delta.fit = NA,
                         sd.fit = NA
)



for(i in 1:nrow(result.df)) {

  n.stim <- result.df$n.stim[i]
  true.delta <- result.df$true.delta[i]

  option1 <- data.frame(a = sample(1:10, size = n.stim, replace = T),
                        p = sample(50:55, size = n.stim, replace = T))

  option2 <- data.frame(b = sample(1:10, size = n.stim, replace = T),
                        q = sample(50:55, size = n.stim, replace = T))


  den.ab <- rowMaxs(cbind(option1$a, option2$b))
  den.pq <- rowMaxs(cbind(option1$p, option2$q))

  d <- ((option1$a - option2$b) / den.ab - (option1$p - option2$q) / den.pq)

  pd.stimuli <- cbind(option1, option2, d)


  p.choose.x <- pnorm(pd.stimuli$d - true.delta, mean = 0, sd = 1)

  choices <- sapply(1:length(p.choose.x), function(x) {

    prob <- p.choose.x[x]
    outcome <- sample(c("x", "y"), size = 1, replace = 1, prob = c(prob, 1 - prob))

  })

  parameter.fits <- optim(par = c(0, 1),
                          fn = pd.loss
  )


  result.df[i, 4:5] <- parameter.fits$par

}

library(beanplot)
library(RColorBrewer)


plot(1, xlim = c(0, length(true.delta.seq) + 1),
     type = "n", ylim = c(-2, 2), xaxt = "n",
     ylab = "Fitted Delta Value", xlab = "True Delta Value",
     main = "Simulated ML fits of proportional difference model"
)

abline(h = seq(-2, 2, 1), col = gray(.8))
abline(h = seq(-2, 2, .25), col = gray(.8), lwd = .5)

abline(a = -1.25, b = .25, lty = 2, col = "red", lwd = 2)

with(result.df[result.df$n.stim == 100,],
     beanplot(delta.fit ~ true.delta, col = gray(.8),
              ylim = c(-2, 2), add = T
     ))

