data(milk)
d <- milk
levels(d$clade)

# index variable
d$clade_id <-as.integer(d$clade)

d$K <-standardize(d$kcal.per.g)
m5.9 <-quap(
  alist(
    K ~dnorm(mu,sigma),
    mu <-a[clade_id],
    a[clade_id] ~dnorm(0,0.5),
    sigma ~dexp(1)
  ) ,data=d)

labels <-paste("a[",1:4,"]:",levels(d$clade),sep="")
plot( precis(m5.9,depth=2,pars="a"),labels=labels,
      xlab="expected kcal(std)")

# add random index variables
set.seed(63)
d$house <-sample(rep(1:4,each=8),size=nrow(d))

m5.10 <-quap(
  alist(
    K ~dnorm(mu,sigma),
    mu <-a[clade_id]+h[house],
    a[clade_id] ~dnorm(0,0.5),
    h[house] ~dnorm(0,0.5),
    sigma ~dexp(1)
  ) ,data=d)

plot( precis(m5.10,depth=2,pars="h"),labels=labels,
      xlab="expected kcal(std)")
