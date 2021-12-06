# Likelihood for combinations of W and L -> W is given 6 times out of 9 with prob 0.5
likelihood <- dbinom(6, size = 9, prob = 0.5)
likelihood

"Using Grid approximation to estimate the part of earth covered by water"

# define grid
p_grid <- seq(from = 0, to = 1, length.out = 20)

# define prior
prior <- rep(1, 20)  # uniform prior
prior <- ifelse(p_grid < 0.5, 0, 1) # step function
prior <- exp(-5 * abs(p_grid - 0.5)) # sharp curve

# compute likelihood at each value in grid
likelihood <- dbinom(6, size = 9, prob = p_grid)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior,so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

# plot
plot(p_grid, posterior, type = "b",
     xlab = "probability of water", ylab = "posterior probability")
mtext("20points")


"Using Grid approximation to estimate the part of earth covered by water step by step"
"WWLWLWWLW"

# define grid
p_grid <- seq(from = 0, to = 1, length.out = 20)

# define prior
prior <- rep(1, 20)  # uniform prior

# compute likelihood at each value in grid
likelihood_land <- dbinom(0, size = 1, prob = p_grid)
likelihood_water <- dbinom(1, size = 1, prob = p_grid)

# compute product of likelihood and prior
new_prior <- likelihood_water * prior / sum(likelihood_water * prior)
new_prior <- likelihood_water * new_prior / sum(likelihood_water * new_prior)
new_prior <- likelihood_land * new_prior / sum(likelihood_land * new_prior)
new_prior <- likelihood_water * new_prior / sum(likelihood_water * new_prior)
new_prior <- likelihood_land * new_prior / sum(likelihood_land * new_prior)
new_prior <- likelihood_water * new_prior / sum(likelihood_water * new_prior)
new_prior <- likelihood_water * new_prior / sum(likelihood_water * new_prior)
new_prior <- likelihood_land * new_prior / sum(likelihood_land * new_prior)
posterior <- likelihood_water * new_prior / sum(likelihood_water * new_prior)

# plot
plot(p_grid, posterior, type = "b",
     xlab = "probability of water", ylab = "posterior probability")
mtext("20points")


"Quadratic approximation"

library(rethinking)
globe.qa <- quap(
  alist(
    W ~ dbinom(W + L, p), # binomial likelihood
    p ~ dunif(0, 1) # uniform prior
  ),
  data = list(W = 6, L = 3))
# display summary of quadratic approximation
precis(globe.qa)


"Compare analytical to quadratic approach"

# analytical calculation
W <- 6
L <- 3
curve(dbeta(x, W + 1, L + 1), from = 0, to = 1)
# quadratic approximation
curve(dnorm(x, 0.67, 0.16), lty = 2, add = TRUE)