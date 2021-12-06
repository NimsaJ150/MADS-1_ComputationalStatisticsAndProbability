library(rethinking)
data(milk)
d <- milk
str(d)

# standardize
d$K <- standardize(d$kcal.per.g)
d$N <- standardize(d$neocortex.perc)
d$M <- standardize(log(d$mass))

# draft bivariate regression
m5.5_draft <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a + bN * N,
    a ~ dnorm(0, 1),
    bN ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = d)
# -> error message

# inspect data
d$neocortex.perc

# data frame with only complete cases
dcc <- d[complete.cases(d$K, d$N, d$M),]

# new try
m5.5_draft <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a + bN * N,
    a ~ dnorm(0, 1),
    bN ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = dcc)

# plot priors
prior <- extract.prior(m5.5_draft)
xseq <- c(-2, 2)
mu <- link(m5.5_draft, post = prior, data = list(N = xseq))
plot(NULL, xlim = xseq, ylim = xseq)
for (i in 1:50)lines(xseq, mu[i,], col = col.alpha("black", 0.3))

# new model
m5.5 <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a + bN * N,
    a ~ dnorm(0, 0.2),
    bN ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = dcc)

# plot priors
prior <- extract.prior(m5.5)
xseq <- c(-2, 2)
mu <- link(m5.5, post = prior, data = list(N = xseq))
plot(NULL, xlim = xseq, ylim = xseq)
for (i in 1:50)lines(xseq, mu[i,], col = col.alpha("black", 0.3))

# look at posterior
precis(m5.5)

# plot posterior
xseq <- seq(from = min(dcc$N) - 0.15, to = max(dcc$N) + 0.15, length.out = 30)
mu <- link(m5.5, data = list(N = xseq))
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)
plot(K ~ N, data = dcc)
lines(xseq, mu_mean, lwd = 2)
shade(mu_PI, xseq)


# draft bivariate regression -> mass
m5.6 <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a + bM * M,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = dcc)

precis(m5.6)

# plot posterior
xseq <- seq(from = min(dcc$M) - 0.15, to = max(dcc$M) + 0.15, length.out = 30)
mu <- link(m5.6, data = list(M = xseq))
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)
plot(K ~ N, data = dcc)
lines(xseq, mu_mean, lwd = 2)
shade(mu_PI, xseq)


# model with both priors
m5.7 <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a + bN * N + bM * M,
    a ~ dnorm(0, 0.2),
    bN ~ dnorm(0, 0.5),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = dcc)
precis(m5.7)

# coefficient distribution
plot(coeftab(m5.5, m5.6, m5.7), pars = c("bM", "bN"))
# correlations
pairs(~K + M + N, dcc)


# counterfactual plots
xseq <- seq(from = min(dcc$M) - 0.15, to = max(dcc$M) + 0.15, length.out = 30)
mu <- link(m5.7, data = data.frame(M = xseq, N = 0))
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)
plot(NULL, xlim = range(dcc$M), ylim = range(dcc$K))
lines(xseq, mu_mean, lwd = 2)
shade(mu_PI, xseq)

xseq <- seq(from = min(dcc$M) - 0.15, to = max(dcc$M) + 0.15, length.out = 30)
mu <- link(m5.7, data = data.frame(M = 0, N = xseq))
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)
plot(NULL, xlim = range(dcc$M), ylim = range(dcc$K))
lines(xseq, mu_mean, lwd = 2)
shade(mu_PI, xseq)