t_clust <- sample(2:7,1)
t_means <- sample(1:20,t_clust)
t_va <- sample(1:4,t_clust,replace=TRUE)
t_pval <- pr(1000,t_clust)
X <- sort(generate(10000,length(t_means),t_means,t_va,t_pval))
#hist(X,breaks=50,freq=FALSE)

EM <- function(err){
  #t_clust <- sample(2:7,1)
  #t_means <- sample(1:20,t_clust)
  #t_va <- sample(1:4,t_clust,replace=TRUE)
  #t_pval <- pr(1000,t_clust)
  #X <- sort(generate(10000,length(t_means),t_means,t_va,t_pval))
  hist(X,breaks=50,freq=FALSE)
  limit = 100
  
  mu_init <- sample(0:20,t_clust)
  va_init <- sample(1:4,t_clust,replace=TRUE)
  p_init <- pr(1000,t_clust)
  
  while(limit>err){
    
    # E-Step
    gammas <- matrix(1,nrow=t_clust,ncol=length(X))
    for (k in 1:t_clust){
      gamma_vals <- vector()
      for(yi in X){
        gamma_vals<-c(gamma_vals, (p_init[k]*N(yi,mu_init[k],va_init[k]))/sum(p_init*N(yi,mu_init,va_init)))
      }
      gammas[k,]<- gamma_vals
    }
    
    
    #for(yi in y){gamma1<-c(gamma1, (p1*N(yi,mu1,v1))/(p1*N(yi,mu1,v1)+p2*N(yi,mu2,v2)))}
    #for(yi in y){gamma2<-c(gamma2, (p2*N(yi,mu2,v2))/(p1*N(yi,mu1,v1)+p2*N(yi,mu2,v2)))}
    
    # M-step
    new_means<- vector()
    new_va<-vector()
    new_pval<-vector()
    
    for (k in 1:t_clust){
      new_means <- c(new_means, sum(gammas[k,]*X)/sum(gammas[k,]))
      new_va <- c(new_va, sum(gammas[k,]*(X-mu_init[k])^2)/sum(gammas[k,]))
      new_pval<-c(new_pval, sum(gammas[k,])/length(X))
    }
    
    log_like <- sum(log(sum(p_init*N(X,mu_init,va_init))))
    new_log_like <- sum(log(sum(new_pval*N(X,new_means,new_va))))
    limit <- abs(log_like-new_log_like)
    print(limit)
    
    # Set new Mean, Sigma, and p estimates
    mu_init=new_means;va_init=new_va;p_init=new_pval
  }
  lines(X,f(X,length(mu_init),mu_init,va_init,p_init),type='l',col='red')
  lines(X,f(X,length(t_means),t_means,t_va,t_pval),type='l',col='green')
}

EM(1e-6)
