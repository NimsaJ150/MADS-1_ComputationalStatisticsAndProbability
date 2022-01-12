"
Assignment 1

Author: Jasmin Capka
Mail: jasmin.capka@fs-students.de
"

library(rethinking)

"
1. Excercise
"

p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(6, size = 9, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

set.seed(215)
samples <- sample(p_grid, prob = posterior, size = 1e4,
                  replace = TRUE)

"
1.a) 5e-04
"
sum(samples < 0.2) / 1e4

"
1.b) 0.1173
"
sum(samples > 0.8) / 1e4

"
1.c) 0.8822
"
sum(samples > 0.2 & samples < 0.8) / 1e4
1 - (sum(samples < 0.2) / 1e4 + sum(samples > 0.8) / 1e4)

"
1.d) 0.5175175
"
quantile(samples, 0.2)


"
2. Excercise
"

# define grid
p_grid <- seq(from = 0, to = 1, length.out = 1000)
# compute likelihood at each value in grid
likelihood <- dbinom(8, size = 15, prob = p_grid)

"
2.a) -> Model A (uniform prior)
"
# define prior
prior_uniform <- rep(1, 1000)  # uniform prior

# compute product of likelihood and prior
unstd.posterior_uniform <- likelihood * prior_uniform
# standardize the posterior, so it sums to 1
posterior_uniform <- unstd.posterior_uniform / sum(unstd.posterior_uniform)

"
2.b) -> Model B (step prior)
"
# define prior
prior_step <- ifelse(p_grid < 0.5, 0, 1) # step function

# compute product of likelihood and prior
unstd.posterior_step <- likelihood * prior_step
# standardize the posterior, so it sums to 1
posterior_step <- unstd.posterior_step / sum(unstd.posterior_step)

"
Plotting
"
# plot
plot(p_grid, posterior_step, type = "l", col = "blue",
     xlab = "probability of water", ylab = "posterior probability",
     main = "Comparison of posteriors resulting from uniform vs step prior")
lines(p_grid, posterior_uniform, type = "l", col = "green",
      xlab = "probability of water", ylab = "posterior probability")
legend(0, 0.005, legend = c("a)", "b)"), col = c("green", "blue"),
  , lty = 1, ncol = 1)

"
Answers to questions:

What is the difference between these two models?
  They rely on different prior distributions. Model A is based on a uniform distribution as prior function. Therefore, all
  values of p have a equal prior probability. As Model B is based on a step function as prior function with a prior
  probability of 0 for p<0.5, the posterior will always be 0 for p<0.5 as well, even if the true value of p might be
  between 0 and 0.5.
  This means, in Model A and B, different assumptions are made, resulting in two different prior
  distributions. The assumption of Model A is that all values of p might be equally probable, the assumption of Model B is
  that the true value of p is definitely not below 0.5.

How does each posterior distribution compare to the true value of p = 0.7?
"
# create sample
set.seed(215)
sample_uniform <- sample(p_grid, size = 1e5, replace = TRUE, prob = posterior_uniform)
sample_step <- sample(p_grid, size = 1e5, replace = TRUE, prob = posterior_step)

# density plots to visualize the samples
dens(sample_uniform)
dens(sample_step)
"
  Having a look at both posterior distributions, the parameter value with the highest posterior probability is the same
  for both. Therefore, this maximum a posteriori estimate is below 0.7 for both.
"
p_grid[which.max(posterior_uniform)]  # 0.5335335
p_grid[which.max(posterior_step)]  # 0.5335335
"
  Having a look at the mean and the median values of samples of both distributions, the median and mean of Model A is about 0.7
  smaller than the one from Model B. Nevertheless, even the mean and meadian of Model B are still appoximately 0.1 lower
  than the true value of p, 0.7.
"
mean(sample_uniform)  # 0.5297055
mean(sample_step)  # 0.6064728
median(sample_uniform)  # 0.5305305
median(sample_step)  # 0.5945946
"
  When looking at the interval surrounding of the true value of p, 0.7, for Model A only 27.7% of the posterior probability lies
  between 0.6 and 0.8. In contrast, for model B, 46.3% of the posterior probability lies between 0.6 and 0.8.
"
sum(posterior_uniform[p_grid > 0.6 & p_grid < 0.8])  # 0.2767154
sum(posterior_step[p_grid > 0.6 & p_grid < 0.8])  # 0.4625874
"
  Taking a look at the 50% Posterior Intervals (PI) the value of p=0.7 is included in neither of the distributions.
  Nevertheless, the PI interval of Model B is slightly denser and shifted to the right compared to the PI interval of
  Model A.
  Lookin at the 50% Highest Posterior Density Intervals (HPDI), the true value of p is within both. Still, the interval
  for model B is denser than the interval for Model A.
"
PI(sample_uniform, prob = 0.5)  # 25% -> 0.4484484, 75% -> 0.6126126
PI(sample_step, prob = 0.5) # 25% -> 0.5465465, 75% -> 0.6546547

HPDI(sample_uniform, prob = 0.5) # 0.5895896, 0.7707708
HPDI(sample_step, prob = 0.5)  # 0.6046046, 0.7637638
"
  In conclusion, both models have the same maximum a posteriori estimate, which is below the true value of p, 0.7.
  In general, the posterior probability for p<0.5 of Model B is zero. This leads to a shift of averaging values toward
  the right in comparison to Model A as can be seen when comparing mean and median values of both distributiona as well
  as the Posterior intervals. Another effect is a higher density for p>=0.5 in Model B. This can be seen in the
  probability density of the intervals surrounding the true value of P and the Highest Posterior Density Intervals.
  Therefore, Model B indicates a higher probability of p being 0.7 than Model A.

Which prior is better and why?
  In general, when no assumptions can be made about a prior distribution, the uniform distribution should be selected as
  used in Model A. It assign an equal probability to all possible values of p and, thus, the differences in posterior
  probability of values p depends completey on the likelihood. A step function with outcomes 0 and 1 as prior
  distribution leads to a certain interval of p having always a posterior probability of 0. This can be dangerous if the
  real value of p lies in that interval.
  Nevertheless, in the globe example, the step function can be preferred and leads to better results. This is because
  the correct assumption of the globe being covered by water by more than 50% was made. The real value of p is 0.7 and,
  thus, greater than 0.5 (end of the interval with posterior probability of 0). This results in a higher density and
  more accurate mean and median of posterior probability for the step prior compared to the uniform prior. Therefore, in
  this case, the step prior is superior.
"

"
3. Excercise
How many times will you have to toss the globe to achieve that the 99% percentile interval of the posterior distribution of
p is only 0.05 wide?

My approach is to simulate dummy observations. This means simulating globe tosses and their subsequent outcomes. As the true value
of p is known to be 0.7 and as the globe tosses follow a binomial distribution (only L or W), the rbinom() function can
be used to generate a single dummy data observation resulting in a list with the results of n globe tosses.

As the question asks at what value of n the 99% percentile interval will be only 0.05 wide, I use a loop to calculate
the 99% percentile interval for a specific n and increase n (the amount of globe tosses) by 1 each loop.
The p_grid and prior can be defined before the loop as the value of n has no impact on them. Then, starting with n=1,
I create a dummy data observation consisting of n tosses. I calculate the likelihood and posterior distribution with
this dummy observation and create a sample. Then I am able to calculate the 99% percentile interval. In case the PI is
wider than 0.05, I increase n by 1, otherwise the loop ends.

As this method depends on the random creation of samples and the random creation of observations, I repeat the whole
process several times to get multiple final values of n.
Then I calculate the average value, which answers the question on how often you have to toss the globe on average to
achieve that the 99% percentile interval of the posterior distribution of p is only 0.05 wide for the first time.
Nevertheless, because of randomness, the next observation with (n+1) tosses may again have a 99% percentile interval,
that is wider than 0.05.

To solve this issue, one can repeat the calculations of the 99% percentile interval several times for a fixed n and
derive the probability of that value n resulting in a 99% percentile interval smaller than 0.05. Defining a threshold
probability 'p_thresh', the value n can be found, which answers the question on how often you have to toss the globe to
achieve that the 99% percentile interval of the posterior distribution of p is only 0.05 wide in 'p_thresh'% of
observations.

In conclusion, the amount of tosses of the globe needed to achieve that the 99% percentile interval of the posterior
distribution of p is only 0.05 wide is heavily depending on random events such as the creation of observations or
samples.
The 99% percentile interval of the posterior distribution of p can be found to be 0.05 wide for the first times, at
values of n being higher than 1700. This can be seen in the code example below.
Nevertheless, to achieve that the 99% percentile interval of the posterior distribution of p is on average (or to a
certain percentage value) only 0.05 wide, the probability of the 99% percentile interval being only 0.05 wide needs to
be calculated for a fixed n. This will result in n larger than 1700.
"
# define grid and prior
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)

# init a list to store the different final values of n
result_list <- NULL

# repeat several times
for (i in 0:20) {

  # start at n=1
  n <- 1

  # repeat until 99% percentile interval is smaller or equal 0.05
  repeat {

    # simulate dummy observation
    dummy_w <- rbinom(1, size = n, prob = 0.7)

    # calculate likelihood and posterior
    likelihood <- dbinom(dummy_w, size = n, prob = p_grid)
    posterior <- likelihood * prior
    posterior <- posterior / sum(posterior)

    # create sample
    samples <- sample(p_grid, prob = posterior, size = 1000,
                      replace = TRUE)

    # calculate 99% percentile interval
    percentiles <- PI(samples, prob = 0.99)

    # if 99% percentile interval is smaller or equal 0.05
    if (percentiles[2] - percentiles[1] <= 0.05) {
      # exit repeat
      break
    } else {
      # increase n by 1
      n <- n + 1
    }
  }

  # print final value of n in this loop and append to sequence
  print(n)
  result_list <- c(result_list, n)
}

# calculate the mean
mean(result_list)