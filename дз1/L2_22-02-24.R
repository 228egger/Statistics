n=1000
K=3
mu=sample(1:50,3,prob=rep(1/50,50))
sigma=sample(1:5,3,prob=rep(1/5,5))
alpha=c(0.3,0.3,0.4)
Y=sample(1:3,size=n,prob=alpha,replace=TRUE)
table(Y)
x=rep(NA,n)
for (k in 1:n){
  x[k]=rnorm(1,mean=mu[Y[k]],sd=sigma[Y[k]])
}
plot(density(x))
plot(density(x[x<40]))
hist(x)
install.packages("mixtools")
library(mixtools)
?normalmixEM
d=normalmixEM(x,k=3)
plot(d,density=TRUE)

mu_em=d$mu
sigma_em=d$sigma
alpha_em=d$lambda
Y_em=sample(1:3,size=n,prob=alpha_em,replace=TRUE)
x_em=rep(NA,n)
for (k in 1:n){
  x_em[k]=rnorm(1,mean=mu_em[Y_em[k]],sd=sigma_em[Y_em[k]])
}
plot(density(x))
lines(density(x_em),col="red")

d=normalmixEM(x,k=2)
plot(d,density=TRUE)

mu0=rep(NA,K)
for (k in 1:K){
  mu0[k]=quantile(x,k/3)
}
sigma0=rep(NA,K)
for (k in 1:K){
  sigma0[k]=runif(1,min=0,max=sd(x))
}
alpha0=rep(1/3,3)
d=normalmixEM(x,k=3,mu=mu0,sigma=sigma0,lambda=alpha0)
plot(d,density=TRUE)
