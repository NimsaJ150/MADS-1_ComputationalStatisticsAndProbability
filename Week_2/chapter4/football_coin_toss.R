library(rethinking)

# Addition
# Throwing coin 16 times for 1000 people
pos <- replicate(1000, sum(runif(16, -1, 1)))
hist(pos)
plot(density(pos))

# Multiplication
prod(1 + runif(12, 0, 0.1))
growth <- replicate(10000, prod(1 + runif(12, 0, 0.1)))
dens(growth, norm.comp = TRUE)

# Small and big impacts
big <- replicate(10000, prod(1 + runif(12, 0, 0.5)))
small <- replicate(10000, prod(1 + runif(12, 0, 0.01)))

dens(big, norm.comp = TRUE)
dens(small, norm.comp = TRUE)

# Log-multiplication
log.big <- replicate(10000, log(prod(1 + runif(12, 0, 0.5))))
dens(log.big, norm.comp = TRUE)
