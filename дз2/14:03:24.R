n=1000
eta=sample(1:6,size=n,prob=c(0.5,rep(0.1,5)),replace=TRUE)
m=c(0,seq(from=-1,to=1,by=0.5))
s=c(1,rep(0.1,5))
x=rep(NA,n)
for (k in 1:n){
  x[k]=rnorm(1,mean=m[eta[k]],sd=s[eta[k]])
}
p=function(x){
  y=dnorm(x)/2
  for (j in 0:4){
    y=y+0.1*dnorm(x,mean=m[j+2],sd=s[j+2])
  }
  return(y)
}
?density
plot(density(x,bw=0.06))
u=seq(from=-3.5,to=3.5,length=1000)
lines(sapply(u,p)~u,col="magenta")
band=c("nrd","nrd0","ucv","bcv","SJ")
plot(density(x,bw=band[1]))
for (k in 2:length(band)){
  lines(density(x,bw=band[k]),col=k)
}
legend("toplefаt",legend=band,col=1:length(band),cex=0.5,lty=1)
bw.nrd0(x)
bw.nrd(x)
bw.ucv(x)
bw.bcv(x)
bw.SJ(x)

plot(density(x,bw=0.001))

kernels=eval(formals(density.default)$kernel)
I=rep(NA,length(kernels))
for (k in 1:length(kernels)){
  I[k]=density(x,kernel=kernels[k],bw="ucv",give.Rkern=TRUE)
}
I[2]/I
plot(density(x,bw="bcv"))
for (k in 2:length(kernels)){
  lines(density(x,bw="bcv",kernel=kernels[k]),col=k)
}
legend("topleft",legend=kernels,col=1:length(kernels),cex=0.5,lty=1)


K=function(u){
  return((9/8-(15/8)*(u^2))*(abs(u)<=1))
}
plot(density(x,kernel=K))
