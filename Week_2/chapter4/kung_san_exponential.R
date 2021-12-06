library(rethinking)

# Load data frame
data(Howell1)
d <- Howell1
plot(height ~ weight, d)

# Quadratic model
d$weight_s <- (d$weight - mean(d$weight)) / sd(d$weight) # standardize weights
d$weight_s2 <- d$weight_s^2 # square of weight_s
m4.5 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1 * weight_s + b2 * weight_s2,
    a ~ dnorm(178, 20),
    b1 ~ dlnorm(0, 1),
    b2 ~ dnorm(0, 1),
    sigma ~ dunif(0, 50)
  ), data = d)

precis(m4.5)

# Cubic model
d$weight_s3 <- d$weight_s^3
m4.6 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a +
      b1 * weight_s +
      b2 * weight_s2 +
      b3 * weight_s3,
    a ~ dnorm(178, 20),
    b1 ~ dlnorm(0, 1),
    b2 ~ dnorm(0, 10),
    b3 ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data = d)

# Plot
weight.seq <- seq(from = -2.2, to = 2, length.out = 30)

# quadratic
pred_dat <- list(weight_s = weight.seq, weight_s2 = weight.seq^2)
mu <- link(m4.5, data = pred_dat)
sim.height <- sim(m4.5, data = pred_dat)

#cubic
pred_dat <- list(weight_s = weight.seq, weight_s2 = weight.seq^2, weight_s3 = weight.seq^3)
mu <- link(m4.6, data = pred_dat)
sim.height <- sim(m4.6, data = pred_dat)

mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.89)
height.PI <- apply(sim.height, 2, PI, prob = 0.89)

plot(height ~ weight_s, d, col = col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
shade(height.PI, weight.seq)



