
randomise <- function(eset){
  data <- exprs(eset)
  dataR <- matrix(NA,ncol=dim(data)[[2]],nrow=dim(data)[[1]])
  
for (i in 1:dim(data)[[1]]){
     dataR[i,] <- data[i,sample(dim(data)[[2]])]
}

exprs(eset) <- dataR
eset
}
