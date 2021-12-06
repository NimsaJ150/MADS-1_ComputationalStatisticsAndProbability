"General case from chapter 2"

# calculate posterior by grid_search
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prob_p <- rep(1, 1000)
prob_data <- dbinom(6, size = 9, prob = p_grid)
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)

# get 10000 sample p values
samples_specialsamples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)

# plot samples
plot(samples)

# density plot
library(rethinking)
dens(samples)


"Summarizing values"

# add up posterior probability where p < 0.5
sum(posterior[p_grid < 0.5])
sum(samples < 0.5) / 1e4  # better
sum(samples > 0.5 & samples < 0.75) / 1e4

quantile(samples, 0.8)
quantile(samples, c(0.1, 0.9))


"Special case: 3W in N=3"

likelihood_special <- dbinom(3, size = 3, prob = p_grid)
posterior_special <- likelihood_special * prob_p
posterior_special <- posterior_special / sum(posterior_special)

samples_special <- sample(p_grid, size = 1e4, replace = TRUE, prob = posterior_special)

# confidence intervals
PI(samples_special, prob = 0.5)
HPDI(samples_special, prob = 0.5)

# maximum a posteriori (MAP)
p_grid[which.max(posterior_special)]
chainmode(samples_special, adj = 0.01)

# Mean, Median
mean(samples_special)
median(samples_special)


"Loss function"

sum(posterior_special * abs(0.5 - p_grid))  # Compute weighted average loss at 0.5
loss <- sapply(p_grid, function(d) sum(posterior_special * abs(d - p_grid))) # Compute for all p
p_grid[which.min(loss)] # get minimum


"Simulation"

# probabilities for each value from 0 to 2 W in two tosses
dbinom(0:2, size = 2, prob = 0.7)
# create n=1 samples including two tosses
rbinom(1, size = 2, prob = 0.7)
# create n=10 samples including two tosses
rbinom(10, size = 2, prob = 0.7)

# create large sample, view value distribution
dummy_w <- rbinom(1e5, size = 2, prob = 0.7)
table(dummy_w) / 1e5

# more tosses included
dummy_w <- rbinom(1e5, size = 9, prob = 0.7)
simplehist(dummy_w, xlab = "dummy water count")


"Posterior predictive function"
# Distribution for p=0.6 (as above)
w <- rbinom(1e4, size = 9, prob = 0.6)
# Distributions for all (Posterior predictive function)
w <- rbinom(1e4, size = 9, prob = samples)
simplehist(w)