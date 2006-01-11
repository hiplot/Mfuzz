kmeans2.plot <- function(eset,kl,mfrow=c(1,1)){
clusterindex <- kl[[1]]

for (j in 1:max(clusterindex)){
  tmp <- exprs(eset)[clusterindex==j,]
 
  if (((j-1)%% (mfrow[1] * mfrow[2]))==0){
  X11()
  par(mfrow=mfrow)
  plot.default(x=NA,xlim=c(1,dim(exprs(eset))[[2]]), ylim= c(min(tmp),max(tmp)),
              xlab="Time",ylab="Expression",main=paste("Cluster",j))
 
  } else {
    plot.default(x=NA,xlim=c(1,dim(exprs(eset))[[2]]), ylim= c(min(tmp),max(tmp)),
              xlab="Time",ylab="Expression",main=paste("Cluster",j))
  }
    

  for (jj in 1:dim(tmp)[[1]]){ 
  lines(tmp[jj,],col="black")
}
}
}
