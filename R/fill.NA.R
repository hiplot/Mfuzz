fill.NA <- function(eset,mode="mean",k=10){
data <- exprs(eset)
  if (mode=="mean"){
   for (i in 1:dim(data)[[1]]){
      data[i,is.na(data[i,])] <- mean(data[i,],na.rm=TRUE)
    }
 }
 
 if (mode=="median"){
    for (i in 1:dim(data)[[1]]){
      data[i,is.na(data[i,])] <- median(data[i,],na.rm=TRUE)
    }  
 }

if (mode=="knn"){
datatmp <- data
  for (i in 1:dim(data)[[1]]){
        if (any(is.na(data[i,]))){
           dt1 <- matrix(data[i,],nrow = dim(data)[[1]],ncol = dim(data)[[2]],byrow =TRUE)        
           D <- sqrt(t(apply((dt1 - datatmp )^2,1,sum, na.rm =TRUE )))  
           nn <- order(D)[2:(k+1)]
           data[i,is.na(data[i,])] <-  apply(t(as.matrix(data[nn,which(is.na(data[i,]))])),1,"mean",na.rm=TRUE)
         }
      }
     }

if (mode=="knnw"){
datatmp <- data
       for(i in 1:dim(data)[[1]]) {
         if (any(is.na(data[i,]))){
           dt1 <- matrix(data[i,],nrow = dim(data)[[1]],ncol = dim(data)[[2]],byrow =TRUE)        
           gene.dist   <- sqrt(t(apply((dt1 - datatmp)^2,1,sum, na.rm =TRUE )))  
           w <- sort(gene.dist)[2:(1+k)]
           W <- 1/w
           W  <- W/sum(W, na.rm = TRUE)
           data[i, is.na(data[i,])] <-  apply(as.matrix(W*data[order(gene.dist)[2:(1+k)],is.na(data[i,])]), 2, sum, na.rm =  TRUE)
         }
       }
     }

exprs(eset) <- data
eset
}   
 

