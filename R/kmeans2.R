kmeans2 <- function(eset,k,iter.max=100){
 cl<-kmeans(exprs(eset),centers=k,iter.max=iter.max)
}
