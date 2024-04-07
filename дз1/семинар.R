thetha = 2
n=1000
nrep = 100
theta1=rep(NA, nrep)
theta2=rep(NA, nrep)
for (k in 1:nrep){
  x = runif(n = 1000, max = thetha)
  theta1[k] = (n + 1) * min(x)
  theta2[k] = ((n+1)/n) * max(x)
}

mean((theta1 - thetha)^2)
mean((theta2 - thetha)^2)

boxplot(theta1, theta2)