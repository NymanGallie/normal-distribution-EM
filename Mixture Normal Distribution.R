N <- function(x,m,v){
  exp(-((x-m)^2)/(2*v))/(sqrt(2*pi*v))
}

generate <- function(n,k,mu,v,p){
  #x <- c(rnorm(round(n*thet[5]),mean=thet[1],sd=sqrt(thet[3])),rnorm(round(n*(1-thet[5])),mean=thet[2],sd=sqrt(thet[4])))
  x <- vector()
  for (s in 1:k){
    x<-c(x,rnorm(round(n*p[s]),mean=mu[s],sd=sqrt(v[s])))
  }
  return(x)
}

f <- function(x,k,mu,v,p){
  result <- 0
  for (s in 1:k){
    result <- result+p[s]*N(x,mu[s],v[s])
  }
  return(result)
}

pr <- function (n,k){
  original_n <- n
  clusters <- vector()
  
  for (i in 1:(k-1)){
    sep <- sample(1:k,1)
    memberships <- sample(1:round(n/sep),1)
    n <- n-memberships
    #print(memberships)
    clusters <- c(clusters, memberships/original_n)
  }
  return(sample(c(clusters, n/original_n)))
}
pr(1000,5)

pr_mean <- function(S,k){
  result <- rep(0,k)
  for (iter in 1:S){
    result=result+pr(1000,k)
  }
  return(result/S)
}
pr_mean(10000,5)

clust <- sample(2:7,1)
means <- sample(0:20,clust)
va <- sample(1:4,clust,replace=TRUE)
pval <- pr(1000,clust)

X <- sort(generate(10000,length(means),means,va,pval))
hist(X,breaks=50,freq=FALSE)
lines(X,f(X,length(means),means,va,pval),type='l',col='red')

