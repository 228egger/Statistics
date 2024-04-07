theta=2
n=1000
nrep=1000
theta1=rep(NA,nrep)
theta2=rep(NA,nrep)
for (k in 1:nrep){
  x=runif(n,max=theta)
  theta1[k]=(n+1)*min(x)
  theta2[k]=((n+1)/n)*max(x)
}
theta1
theta2
sum((theta1-theta)^2)/nrep
mean((theta1-theta)^2)
mean((theta2-theta)^2)
boxplot(theta1,theta2)
data=data.frame(theta1,theta2)
boxplot(data)
cbind(theta1,theta2)

alpha=1
beta=2
F_inv=function(y,a,b){
  return(b*(y^(1/a)))
}

nrep=100
n=1000
a_mle=rep(NA,nrep)
a_mm=rep(NA,nrep)
b_mle=rep(NA,nrep)
b_mm=rep(NA,nrep)
for (k in 1:nrep){
  u=runif(n)
  x=sapply(u,F_inv,a=alpha,b=beta)
  b_mle[k]=max(x)
  a_mle[k]=1/(log(b_mle[k])-mean(log(x)))
  aux=(mean(x)^2)/(mean(x^2)-(mean(x))^2)
  a_mm[k]=-1+sqrt(1+aux)
  b_mm[k]=mean(x)*(a_mm[k]+1)/a_mm[k]
}
boxplot(cbind(a_mle,a_mm))
boxplot(cbind(b_mle,b_mm),col="wheat",xaxt="n")
axis(side=1, at=1:2,label=c("ML","MM"))
grid()

n=1000
shape1=1
shape2=1
x=rbeta(n,shape1,shape2)
install.packages("stats4")
library(stats4)
?mle
minus_logL=function(sh1,sh2){
  return(-sum(dbeta(x,shape1=sh1,shape2=sh2,log=TRUE)))
}
mle(minus_logL,start=c(0.1,0.1),lower=c(0,0))

install.packages("gmm")
library(gmm)
?gmm
g=function(theta,u){
  sh1=theta[1]
  sh2=theta[2]
  m1=sh1/(sh1+sh2)-u
  m2=sh1*sh2/(((sh1+sh2)^2)*(sh1+sh2+1))+(sh1/(sh1+sh2))^2-u^2
  return(cbind(m1,m2))
}
gmm(g,x,c(0.1,0.1))

install.packages("fitdistrplus")
library(fitdistrplus)
?fitdist

shape1=0.1
shape2=10
x=rbeta(n,shape1,shape2)
res1=fitdist(x,"beta",method="mle")
res1$estimate
res2=fitdist(x,"beta",method="mme")
res2$estimate
