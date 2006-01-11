mfuzz <- function(eset,centers,m,...){

cl<-cmeans(exprs(eset),centers=centers,method="cmeans",m=m,...)


}
