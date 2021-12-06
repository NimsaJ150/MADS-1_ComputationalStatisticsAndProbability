library(rethinking)

# Load data frame
data(Howell1)
d <- Howell1
d2 <- d[d$age >= 18,]

# Define priors
flist <- alist(
  height ~ dnorm(mu, sigma),
  mu ~ dnorm(178, 20),
  sigma ~ dunif(0, 50)
)

# Fit model
m4.1 <- quap(flist, data = d2)

precis(m4.1)


# Narrow priors
m4.2 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu ~ dnorm(178, 0.1),
    sigma ~ dunif(0, 50)
  ), data = d2)
precis(m4.2)

vcov( m4.1)  # matrix of variances and covariances
diag( vcov(m4.1))  # vector of variances
cov2cor( vcov(m4.1))  # a correlation matrix

# Sampling
library(rethinking)
post <-extract.samples(m4.1,n=1e4)

head(post)
precis(post)

plot(post)