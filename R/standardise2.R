standardise2 <- function(eset,timepoint=1){
  data <- exprs(eset)
  for (i in 1:dim(data)[[1]]){
    data[i,] <- (data[i,] - data[i,timepoint])/sd(data[i,],na.rm=TRUE) 
  }
exprs(eset) <- data
  eset
}
