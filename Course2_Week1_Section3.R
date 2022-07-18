set.seed(32)

m = 10000
a = 2.0
b = 1.0/3.0

theta = rgamma(n=m, shape=a, rate=b)

hist(theta, freq=FALSE)
curve(dgamma(x,shape=a, rate=b), col="blue", add=TRUE)

sum(theta)/m
mean(theta)
a/b

var(theta)
a/b^2

ind = theta < 5.0
mean(ind)

pgamma(q=5.0, shape=a, rate=b)

quantile(theta,probs=0.9)
qgamma(p=0.9,shape=a,rate=b)

###

set.seed(32)

m = 10000
a = 2.0
b = 1.0/3.0

theta = rgamma(n=m, shape=a, rate=b)

se = sd(theta) / sqrt(m)

mean(theta) - 2.0*se
mean(theta) + 2.0*se

ind = theta < 5
mean(ind)
pgamma(5.0, shape=a, rate=b)

se = sd(ind) / sqrt(m)

###

# 1. Simulate phi_i from Beta(2,2)
# 2. Simulate y_i from Binom(10,phi_i)

m = 1e5

y = numeric(m)
phi = numeric(m)

# Slow
for (i in 1:m) {
  phi[i] = rbeta(1,shape1=2.0,shape2=2.0)
  y[i] = rbinom(1,size=10,prob=phi[i])
}

# Fast
phi = rbeta(m,shape1=2.0,shape2=2.0)
y = rbinom(m,size=10,prob=phi)

table(y)/m
plot(table(y)/m)

mean(y)

###

# 1. Not the long one or the one without the square
# 2. integral 0 to pi
# 3. prob of value comparison
# 4. norm 2,0.02
# 5. Not 1.7 - once you have m samples (call them theta i *) calculate 1/m sum(m i=1) theta i * /1 - theta i *
# 6. 0.77
# theta = rbeta(9999, 5, 3)
# alpha = theta / (1 - theta)
# mean( alpha )
# mean( alpha > 1.0 )
# 7. Not 0.52 - first draw a large number of samples from the standard normal dist, then use quantile function
# 8. 0.032 - all good here


m = 1e5

phi = rbeta(m,shape1=5.0,shape=3.0)

y = rbinom(n=m, size=1, prob=phi)

reg = rbeta(m,1,0)

ind = reg > 0.5
mean(ind)

mean(y)

mean(phi)

theta(phi)


phi = rbinom(m,size=1,0.5)

ind = phi < 0.3
mean(ind)

1+qnorm(0.3)

sqrt(5.2/5000)

se = sd(blah) / sqrt(5000)
se
