# Ways to produce data -> Parameter
ways <- c(0, 3, 8, 9, 0)
ways <- c(0, 3, 16, 27, 0)

# Probability of given balls -> Prior probability
prior_p <- c(0, 0.5, 1 / 3, 1 / 6, 0)

# Plausibility -> Likelihood
likelihood <- ways / sum(ways)

# Posterior probability
posterior_p <- likelihood * prior_p / sum(likelihood * prior_p)
posterior_p