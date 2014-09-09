membership <- function(x,clusters,m){
if (is.vector(x)) x <-  t(as.matrix(x,nrow=1))

u.ij <- matrix(NA, ncol=dim(clusters)[[1]],nrow=dim(x)[[1]]) 

for (i in 1:dim(x)[[1]]){
  u.i <- 0 
  for (j in 1:dim(clusters)[[1]]){
    
    tmp <- 0
    for (k in 1:dim(clusters)[[1]]){
      tmp <- tmp + (as.vector(dist(rbind(x[i,],clusters[j,])))/(as.vector(dist(rbind(x[i,],clusters[k,])))))^(2/(m-1))
    }

    u.i[j] <- 1/tmp
  }

  u.ij[i,] <- u.i/sum(u.i)
}

dimnames(u.ij)[[1]] <- dimnames(x)[[1]]
return(u=u.ij)
}
