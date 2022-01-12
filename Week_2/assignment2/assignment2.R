"
Assignment 2

Author: Jasmin Capka
Mail: jasmin.capka@fs-students.de
"

library(rethinking)
library(dagitty)

"
1. Excercise
"
data(Howell1)
d <- Howell1
d2 <- d[d$age >= 18,]
xbar <- mean(d2$weight)
m4.3 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * (weight - xbar),
    a ~ dnorm(178, 20),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  ), data = d2)

"
In order to predict expected heights and their credibility intervals for 5
individuals, their weight needs to be inserted into the model:
"
set.seed(111)

# define sequence of weights to compute predictions for
weight.seq <- c(46, 61, 35, 52, 56)

# use link to compute mu for each weight in weight.seq
mu <- link(m4.3, data = data.frame(weight = weight.seq))

# summarize the distribution of mu
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.89)

mu.mean
# 155.5171 169.0793 145.5714 160.9420 164.5586

mu.PI
#         [,1]     [,2]     [,3]     [,4]     [,5]
# 5%  155.0743 167.8824 144.7437 160.3047 163.6955
# 94% 155.9476 170.2010 146.3948 161.5718 165.3659

"
The expected heights can be found in mu.mean and the 89% credibility intervals
can be found in mu.PI. This leads to the following results:

Individual | weight | expected_height | 89% interval
--------------------------------------------------------------
      1    |   46   |   155.5171      | [155.0743, 155.9476]
      2    |   61   |   169.0793      | [167.8824, 170.2010]
      3    |   35   |   145.5714      | [144.7437, 146.3948]
      4    |   52   |   160.9420      | [160.3047, 161.5718]
      5    |   56   |   164.5586      | [163.6955, 165.3659]
"


"
2. Excercise
"
# loading the data
data(Howell1)
d <- Howell1

d$weight_s <- (d$weight - mean(d$weight)) / sd(d$weight) # standardize weights
d$weight_s2 <- d$weight_s^2 # square of weight_s
d$weight_s3 <-d$weight_s^3 # cube of weight_s

"
Parabolic model with standardization
The following code plots the prior predictive distribution for the second order 
polynomial regression model in Chapter 4:
"
# second order polynomial regression model in chapter 4
m2.1 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1 * weight_s + b2 * weight_s2,
    a ~ dnorm(178, 20),
    b1 ~ dlnorm(0, 1),
    b2 ~ dnorm(0, 1),
    sigma ~ dunif(0, 50)
  ), data = d)

precis(m2.1)

# use extract.prior to inspect the prior
prior <- extract.prior(m2.1)

# modify the code that simulates and plots predictive distributions
plot(NULL, xlim = range(d$weight_s), ylim = c(-100, 400),
     xlab = "weight (standardized)", ylab = "height")
abline(h = 0, lty = 2)
abline(h = 272, lty = 1, lwd = 0.5)
mtext("mu<-a+b1*weight_s+b2*weight_s^2")
for (i in 1:30) curve(prior$a[i] + prior$b1[i] * x + prior$b2[i] * x^2,
                      from = min(d$weight_s),
                      to = max(d$weight_s),
                      add = TRUE,
                      col = col.alpha("black", 0.2))
"
Can you modify the prior distributions of a, b1 and b2 so that the prior
predictions stay within the biologically reasonable outcomes?
  For the standardized second order polynomial model, the prior distributions already stay 
  between the reasonable outcomes. Nevertheless, it is possible to further
  adjust the priors by existing general knowledge about height and weight:
  
  a is the intercept, meaning the expected value of height when weight_s is 0, 
  or in other words when weight is at its mean value. I would choose around 172cm as it is 
  approx. the average height of people in Germany and lower the standard deviation
  to 15cm. This is because I assume that 95.45% have a height between 142cm and
  202cm (double standard deviation).
  
  b1 is the linear component of the curve. It answers the question: by how many 
  cm changes the height when weight_s is increased by 1. An increase of weight_s by 1
  means an increase by sd(d$weight) kg:
"
sd(d$weight)  # 14.71918
"
  Which means an increase by approx. 15 kg. I would assume that an increase by
  15kg, on average, would imply an increase in height and, therefore, would set the average
  expected increase to 1 (meaning a positive value) and just keep the standard
  deviation as defined before.
  
  b2 is the square component. I would assume that the curve opens downward, 
  because a low weight definitely has a low height, especially in case of children.
  Adults have a higher weight and also a higher height. Nevertheless, for adults,
  there exists a wider spread of weight because of different bodytypes for almost
  the same height. This means coming from children to adults the connection 
  'higher weight means grester height' is not as strong anymore and can be
  modelled by a flattening behaviour.
  I would therefore set a negative b2. I would choose -1 as it is still pretty
  close to the value of 0 in the defined model and keep the standard deviation
  at 1.
  
  This leads to this new model with new priors and slightly different prior
  predictive distributions:
"
m2.1_new <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1 * weight_s + b2 * weight_s2,
    a ~ dnorm(172, 15),
    b1 ~ dlnorm(1, 1),
    b2 ~ dnorm(-1, 1),
    sigma ~ dunif(0, 50)
  ), data = d)

precis(m2.1_new)

prior <- extract.prior(m2.1_new)

plot(NULL, xlim = range(d$weight_s), ylim = c(-100, 400),
     xlab = "weight (standardized)", ylab = "height")
abline(h = 0, lty = 2)
abline(h = 272, lty = 1, lwd = 0.5)
mtext("mu<-a+b1*weight.s+b2*weight.s^2")
for (i in 1:30) curve(prior$a[i] + prior$b1[i] * x + prior$b2[i] * x^2,
                      from = min(d$weight_s),
                      to = max(d$weight_s),
                      add = TRUE,
                      col = col.alpha("black", 0.2))
"
From the description of the exercise, I expected the prior predictive
distributions to go beyond the reasonable outcomes.
As this was not the case for the second order polynomial model, the third order
polynomial will be inspected as well.


Cubic model with standardization
The following code plots the prior predictive distribution for the third order 
polynomial regression model in Chapter 4:
"
# third order polynomial regression model in chapter 4
m2.2 <-quap(
  alist(
    height ~dnorm(mu,sigma),
    mu <-a+b1*weight_s+b2*weight_s2+b3*weight_s3,
    a ~dnorm(178,20),
    b1 ~dlnorm(0,1),
    b2 ~dnorm(0,10),
    b3 ~dnorm(0,10),
    sigma ~dunif(0,50)
  ) ,data=d)

precis(m2.2)

# use extract.prior to inspect the prior
prior <- extract.prior(m2.2)

# modify the code that simulates and plots predictive distributions
plot(NULL, xlim = range(d$weight_s), ylim = c(-100, 400),
     xlab = "weight (standardized)", ylab = "height")
abline(h = 0, lty = 2)
abline(h = 272, lty = 1, lwd = 0.5)
mtext("mu<-a+b1*weight.s+b2*weight.s^2+b3*weight.s^3")
for (i in 1:30) curve(prior$a[i] + prior$b1[i] * x + prior$b2[i] * x^2 + prior$b3[i] * x^3,
                      from = min(d$weight_s),
                      to = max(d$weight_s),
                      add = TRUE,
                      col = col.alpha("black", 0.2))
"
Can you modify the prior distributions of a, b1 and b2 so that the prior
predictions stay within the biologically reasonable outcomes?
  For the third order polynomial model, the prior distributions actually do go 
  beyond the reasonable outcomes. The following adjustments can be made with general
  existing knowledge about weight and height:
  
  a, b1 and b2 have the same meaning in this cubic as in the quadratic model.
  Therefore, I would apply the same new distributions as proposed in the section
  before for these three parameters.
  
  b3 is the cubic component. For the same reason as for b2, because of a
  flattening behaviour for higher weights and the existance of children with low 
  height and low weight, I would expect b3 to be positive and, therefore,
  set it to 1. Additionally, from my existing imagination of the connection of 
  height and weight, I would assume that b3 does not have an extremely high
  impact, because I expect the connection to be described by a flattening curve.
  Therefore, I would also reduce the standard deviation to 1.
  
  This is the resulting new cubic model:
"
m2.2_new <-quap(
  alist(
    height ~dnorm(mu,sigma),
    mu <-a+b1*weight_s+b2*weight_s2+b3*weight_s3,
    a ~dnorm(172,15),
    b1 ~dlnorm(1,1),
    b2 ~dnorm(-1,1),
    b3 ~dnorm(1,1),
    sigma ~dunif(0,50)
  ) ,data=d)

prior <- extract.prior(m2.2_new)

plot(NULL, xlim = range(d$weight_s), ylim = c(-100, 400),
     xlab = "weight (standardized)", ylab = "height")
abline(h = 0, lty = 2)
abline(h = 272, lty = 1, lwd = 0.5)
mtext("mu<-a+b1*weight.s+b2*weight.s^2+b3*weight.s^3")
for (i in 1:30) curve(prior$a[i] + prior$b1[i] * x + prior$b2[i] * x^2 + prior$b3[i] * x^3,
                      from = min(d$weight_s),
                      to = max(d$weight_s),
                      add = TRUE,
                      col = col.alpha("black", 0.2))
"
  For this new model with its new prior predictive distributions the prior 
  predictions stay within the biologically reasonable outcomes.
"


"
3. Excercise

The description implies that the price is not only depending on the size of a
house, but also on the location of it. Therefore, we have a relationship, 
where the posterior association of both with the outcome increases, when both
predictor variables are in the regression.
This means, price (P) is depending on size (S) and location (L).
"

alist(
  ## S->P<-L
  P ~ dnorm(mu, sigma),
  mu <- a + bS * S + a[location_code],
  a ~ dnorm(0, 0.2),
  bS ~ dnorm(0, 0.5),
  a[location_code] ~ dnorm(0, 0.5),
  sigma ~ dunif(0, 50)
)

"
Looking at the model definition, the price is normally distributed with mu and
sigma.
The mu is constructed by the intercept a, the parameter bS, the size and the
index variable location code.
When using these definitions to fit a model, for the index variable
location_code, the prior distribution from the book associated with four 
categories can be applied. For a and bS appropriate priors would still need
to be defined.
"


"
4. Excercise

The implied conditional independencies for the relation (M->A->D) can be found
by using the dagitty package: 
"
DMA_dag <- dagitty('dag{M->A->D}')
impliedConditionalIndependencies(DMA_dag)
# D _||_ M | A
"
The result indicates that D is independent of M, conditional on A: 'D _||_ M | A'.
To test whether the data is consistent with this implication, three different 
models can be fitted and compared, following the book from page 131 on (in the 
book it is already shown, that the data is consistent with this implication as 
it is the same implication as the one for the second DAG in the book).

The first model displays the influence of M on D, the second one of A on D, and
the third on of A and M on D.
"
# loading the data
data(WaffleDivorce)
d <- WaffleDivorce

# standardize variables
d$D <- standardize(d$Divorce)
d$M <- standardize(d$Marriage)
d$A <- standardize(d$MedianAgeMarriage)

# fit model D <- M
m4.1 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM * M,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d)
precis(m4.1)

# fit model D <- A
m4.2 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bA * A,
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d)
precis(m4.2)

# fit model M -> D <- A
m4.3 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM * M + bA * A,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d)
precis(m4.3)

# compare coefficients
plot(coeftab(m4.1, m4.2, m4.3), par = c("bA", "bM"))

"
Comparing the posterior mean for marriage rate, bM, is close to zero in the 
third model wehereas it was approximately 0.35 in the first model. The standard
deviation indicates the probability of bM on both sides of 0 in the third model 
as well.
This can also be seen in the plot: bM is only associated with divorce, when age
is not part of the model.
Therefore, the data is consistent with the DAG 'M->A->D'.
"
