data(Howell1)
d <- Howell1
str(d)

# indicator variable
mu_female <- rnorm(1e4, 178, 20)
mu_male <- rnorm(1e4, 178, 20) + rnorm(1e4, 0, 10)
precis(data.frame(mu_female, mu_male))

# index variable
d$sex <- ifelse(d$male == 1, 2, 1)
str(d$sex)

m5.8 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a[sex],
    a[sex] ~ dnorm(178, 20),
    sigma ~ dunif(0, 50)
  ), data = d)
precis(m5.8, depth = 2)

post <- extract.samples(m5.8)
post$diff_fm <- post$a[, 1] - post$a[, 2]
precis(post, depth = 2)
