filter.std <- function (eset, min.std,visu=TRUE){
  #index <- logical(dim(exprs(eset))[1])
  tmp <- logical(dim(exprs(eset))[1])
  if (is.numeric(min.std)){ 
    data <- exprs(eset)
    for (i in 1:length(tmp)){
      tmp[i]   <- sd(data[i,],na.rm=TRUE)
   #   index[i]  <- ( tmp[i] > min.std)
   
    }
     index <- tmp > min.std
     index[is.na(index)] <- TRUE
    cat(paste(sum(!index),"genes excluded.\n"))
}
  
 
  if (visu){
    plot(sort(tmp),xlab="Ordered genes",ylab="Sd")
  }
 eset[index,]
}



