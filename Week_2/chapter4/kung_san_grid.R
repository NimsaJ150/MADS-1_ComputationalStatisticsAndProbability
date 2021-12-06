library(rethinking)

# Load data frame
data(Howell1)
d <- Howell1

# Show overview
str(d)
precis(d)

# Vectors
d$height
d2 <- d[d$age >= 18,]

# Density Height
dens(d2$height)  # Gaussian shape

# Define priors
curve(dnorm(x, 178, 20), from = 100, to = 250)  # for mean (mu)
curve(dunif(x, 0, 50), from = -10, to = 60)  # for standard deviation (sigma)

# Sample
sample_mu <- rnorm(1e4, 178, 20)
sample_sigma <- runif(1e4, 0, 50)
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h)

sample_mu <- rnorm(1e4, 178, 100) # high standard deviation
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h)

# Posterior distribution by grid approximation
mu.list <- seq(from = 150, to = 160, length.out = 100)
sigma.list <- seq(from = 7, to = 9, length.out = 100)
post <- expand.grid(mu = mu.list, sigma = sigma.list)
post$LL <- sapply(1:nrow(post), function(i)sum(
  dnorm(d2$height, post$mu[i], post$sigma[i], log = TRUE)))
post$prod <- post$LL +
  dnorm(post$mu, 178, 20, TRUE) +
  dunif(post$sigma, 0, 50, TRUE)
post$prob <- exp(post$prod - max(post$prod))

contour_xyz(post$mu, post$sigma, post$prob)  # contour plot
image_xyz(post$mu, post$sigma, post$prob)  # simple heat map

# Sampling from posterior
sample.rows <- sample(1:nrow(post), size = 1e5, replace = TRUE,
                      prob = post$prob)
sample.mu <- post$mu[sample.rows]
sample.sigma <- post$sigma[sample.rows]

plot(sample.mu, sample.sigma, cex = 0.5, pch = 16, col = col.alpha(rangi2, 0.1))
dens(sample.mu)
dens(sample.sigma)


PI(sample.mu)
PI(sample.sigma)

# 20 random height samples
d3 <- sample(d2$height, size = 20)

mu.list <- seq(from = 150, to = 170, length.out = 200)
sigma.list <- seq(from = 4, to = 20, length.out = 200)
post2 <- expand.grid(mu = mu.list, sigma = sigma.list)
post2$LL <- sapply(1:nrow(post2), function(i)
  sum(dnorm(d3, mean = post2$mu[i], sd = post2$sigma[i],
            log = TRUE)))
post2$prod <- post2$LL +
  dnorm(post2$mu, 178, 20, TRUE) +
  dunif(post2$sigma, 0, 50, TRUE)
post2$prob <- exp(post2$prod - max(post2$prod))
sample2.rows <- sample(1:nrow(post2), size = 1e4, replace = TRUE,
                       prob = post2$prob)
sample2.mu <- post2$mu[sample2.rows]
sample2.sigma <- post2$sigma[sample2.rows]
plot(sample2.mu, sample2.sigma, cex = 0.5,
     col = col.alpha(rangi2, 0.1),
     xlab = "mu", ylab = "sigma", pch = 16)

dens(sample2.sigma, norm.comp = TRUE)
