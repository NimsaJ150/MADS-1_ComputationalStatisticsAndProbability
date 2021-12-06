library(rethinking)

# Load data frame
data(Howell1)
d <- Howell1
d2 <- d[d$age >= 18,]

plot(d2$height ~ d2$weight)

# Priors 
set.seed(2971)
N <- 100  # 100lines
a <- rnorm(N, 178, 20)
b <- rnorm(N, 0, 10)

# Logarithmic Priors
set.seed(2971)
N <- 100 # 100lines
a <- rnorm(N, 178, 20)
b <- rlnorm(N, 0, 1)

test <- rlnorm(1e4, 0, 1)
dens(test, xlim = c(0, 5), adj = 0.1)

# simulate heights
plot(NULL, xlim = range(d2$weight), ylim = c(-100, 400),
     xlab = "weight", ylab = "height")
abline(h = 0, lty = 2)
abline(h = 272, lty = 1, lwd = 0.5)
mtext("b~dnorm(0,10)")
xbar <- mean(d2$weight)
for (i in 1:N) curve(a[i] + b[i] * (x - xbar),
                     from = min(d2$weight), to = max(d2$weight), add = TRUE,
                     col = col.alpha("black", 0.2))

# Model fitting
# define the average weight,x-bar
xbar <- mean(d2$weight)
# fit model
m4.3 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * (weight - xbar),
    a ~ dnorm(178, 20),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  ), data = d2)

# Interpreting the model
# tables
precis(m4.3)  # b -> a person 1kg heavier is expected to be 0.90cm taller

round(vcov(m4.3), 3)
pairs(m4.3)

# plotting
plot(height ~ weight, data = d2, col = rangi2)  # plots the raw data
post <- extract.samples(m4.3)
a_map <- mean(post$a)  # computes the posterior mean values for a and b
b_map <- mean(post$b)
curve(a_map + b_map * (x - xbar), add = TRUE)  # draws the implied line

post[1:5,]  # view sample combinations

# Adding uncertainty -> more lines in plot
N <- 10
dN <- d2[1:N,]
mN <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * (weight - mean(weight)),
    a ~ dnorm(178, 20),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  ), data = dN)

# extract 20 samples from the posterior
post <- extract.samples(mN, n = 20)
# display raw data and sample size
plot(dN$weight, dN$height,
     xlim = range(d2$weight), ylim = range(d2$height),
     col = rangi2, xlab = "weight", ylab = "height")
mtext(concat("N =", N))
# plot the lines, with transparency
for (i in 1:20)
  curve(post$a[i] + post$b[i] * (x - mean(dN$weight)),
        col = col.alpha("black", 0.3), add = TRUE)

# Plotting regression intervals and contours
# weight = 50
post <- extract.samples(m4.3)
mu_at_50 <- post$a + post$b * (50 - xbar)
dens(mu_at_50, col = rangi2, lwd = 2, xlab = "mu|weight=50")

# percentile interval
PI(mu_at_50, prob = 0.89)

# for all weights
mu <- link(m4.3)
str(mu)

# for all weights with only unique values
# define sequence of weights to compute predictions for
# these values will be on the horizontal axis
weight.seq <- seq(from = 25, to = 70, by = 1)
# use link to compute mu
# for each sample from posterior
# and for each weight in weight.seq
mu <- link(m4.3, data = data.frame(weight = weight.seq))
str(mu)

# visualize
# use type="n" to hide raw data
plot(height ~ weight, d2, type = "n")
# loop over samples and plot each mu value
for (i in 1:100)
  points(weight.seq, mu[i,], pch = 16, col = col.alpha(rangi2, 0.1))

# summarize the distribution of mu
mu.mean <- apply(mu, 2, mean)  # compute the mean of each column (dimension“2”) of
# the matrix mu.
mu.PI <- apply(mu, 2, PI, prob = 0.89)

# plot raw data
# fading out points to make line and interval more visible
plot(height ~ weight, data = d2, col = col.alpha(rangi2, 0.5))
# plot the MAP line, aka the mean mu for each weight
lines(weight.seq, mu.mean)
# plot a shaded region for 89% PI
shade(mu.PI, weight.seq)

# Prediction intervals
sim.height <- sim(m4.3, data = list(weight = weight.seq)) # simulate height
str(sim.height)
height.PI <- apply(sim.height, 2, PI, prob = 0.89)  # percentile interval

# plot raw data
plot(height ~ weight, d2, col = col.alpha(rangi2, 0.5))
# draw MAP line
lines(weight.seq, mu.mean)
# draw HPDI region for line
mu.HPDI <- apply(mu, 2, HPDI)
shade(mu.HPDI, weight.seq)
# draw PI region for simulated heights
shade(height.PI, weight.seq)
