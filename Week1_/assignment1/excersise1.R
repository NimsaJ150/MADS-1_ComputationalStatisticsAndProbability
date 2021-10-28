p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(6, size = 9, prob = p_grid)
posterior <- likelihood * prior
# Normalize posterior
posterior <- posterior / sum(posterior)

set.seed(215)
samples <- sample(p_grid, prob = posterior, size = 1e4,
                  replace = TRUE)