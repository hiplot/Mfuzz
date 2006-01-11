partcoef <- function(eset,crange=seq(4,32,4),mrange=seq(1.05,2,0.1),...){

  F <- matrix(NA,nrow=length(crange),ncol=length(mrange))
  F.n <- matrix(NA,nrow=length(crange),ncol=length(mrange))
  F.min <-  matrix(NA,nrow=length(crange),ncol=length(mrange))
  i=j=0;
  for (c in crange){
    i=i+1;j =0;
    for (m in mrange){
      j=j+1;
      Utmp <- mfuzz(eset,centers=c,m=m,...)[[4]]
      F[i,j] <- sum(Utmp^2)/(dim(Utmp)[[1]]*dim(Utmp)[[2]])
      F.min[i,j] <-  (1/c^2)
      F.n[i,j] <- F[i,j]- F.min[i,j]
    }
  }
dimnames(F) <- list(paste("c:",crange,sep=""),paste("m:",mrange,sep=""))
dimnames(F.n) <- list(paste("c:",crange,sep=""),paste("m:",mrange,sep="")) 
dimnames(F.min) <- list(paste("c:",crange,sep=""),paste("m:",mrange,sep="")) 
  
return(list(F,F.n,F.min))
}
