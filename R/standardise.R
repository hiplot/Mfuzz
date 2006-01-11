standardise <- function(eset){
  data <- exprs(eset)
  for (i in 1:dim(data)[[1]]){
    data[i,] <- (data[i,] - mean(data[i,],na.rm=TRUE))/sd(data[i,],na.rm=TRUE) 
  }
exprs(eset) <- data
  eset
}
