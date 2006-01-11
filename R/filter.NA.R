filter.NA <- function(eset,thres=0.25){

index <- logical(dim(exprs(eset))[1])

  for (i in 1:dim(exprs(eset))[1]){
     index[i] <- (((sum(is.na(exprs(eset)[i,]))/dim(exprs(eset))[2])) > thres)  
  }
 cat(paste(sum(index),"genes excluded.\n"))

 eset[!index,]
}

  
