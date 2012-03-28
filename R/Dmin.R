Dmin <- function(eset,m,crange = seq(4,40,4),repeats=3,visu=TRUE){
  
  DminM <- matrix(0,nrow=length(crange),ncol=repeats)

  for (ii in 1:repeats){
      j <- 0
      for (i in crange){
        cl <- mfuzz(eset,c=i,m=m)
        DminM[j <- j+1,ii] <- min(dist(cl[[1]]))
      }
    }
   DminMav <- apply(DminM,1,mean)
  
  if (visu) plot(crange,DminMav,xlab="Cluster number",ylab="Min. centroid distance")

  return(DminMav)
}
