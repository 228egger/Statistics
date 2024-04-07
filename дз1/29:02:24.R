alpha/(alpha+beta)=0.45
alpha=(9/11)*beta
install.packages("bayesrules")
library(bayesrules)
plot_beta(alpha=9,beta=11,mean=TRUE,mode=TRUE)
plot_beta(alpha=27,beta=33,mean=TRUE,mode=TRUE)
plot_beta(alpha=45,beta=55,mean=TRUE,mode=TRUE)
alpha=45
beta=55
alpha/(alpha+beta)
# Y=\sum_k X_k |\theta ~ B(n,\theta)
n=50
y=30
plot_binomial_likelihood(y=y,n=n,mle=TRUE)
plot_beta_binomial(alpha=alpha,beta=beta,y=y,n=n)
plot_beta_binomial(alpha=5,beta=5,y=y,n=n)
summarize_beta_binomial(alpha=alpha,beta=beta,y=y,n=n)
alpha0vec=seq(from=5,to=55,by=10)
beta0vec=seq(from=15,to=65,by=10)
theta_est=matrix(NA,nrow=length(alpha0vec),ncol=length(beta0vec))
for (i in 1:length(alpha0vec)){
  for (j in 1:length(beta0vec)){
    print(plot_beta_binomial(alpha=alpha0vec[i],beta=beta0vec[j],y=y,n=n))
    readline(prompt="Press enter")
    res=summarize_beta_binomial(alpha=alpha0vec[i],beta=beta0vec[j],y=y,n=n)
    theta_est[i,j]=res$mean[2]
  }
}
theta_est

n=1000
theta=rbeta(n,alpha,beta)
data=rep(NA,n)
for (k in 1:n){
  data[k]=rbinom(1,1,theta[k])
}
mean(data)
alpha/(alpha+beta)
alpha0vec=seq(from=1,to=50,by=10)
beta0vec=seq(from=1,to=50,by=10)
theta_est=matrix(NA,nrow=length(alpha0vec),ncol=length(beta0vec))
for (i in 1:length(alpha0vec)){
  for (j in 1:length(beta0vec)){
    res=summarize_beta_binomial(alpha=alpha0vec[i],beta=beta0vec[j],y=sum(data),n=n)
    theta_est[i,j]=res$mean[2]
  }
}
theta_est


x=rnorm(1000)
plot(ecdf(x))
lines(pnorm(sort(x))~sort(x),col="red")
lines(pt(sort(x),15)~sort(x),col="blue")
fun_ecdf=ecdf(x)
values=fun_ecdf(x)
new_ecdf=function(u){
  if (sort(x)[1]<=u){
    max(which(sort(x)<=u))/length(x)
  }
  else 0
}
sapply(x,new_ecdf)
values
ks.test(x,"pnorm")
ks.test(x,"pnorm",mean=mean(x),sd=sd(x))
ks.test(x,"pcauchy")

