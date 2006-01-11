overlap.plot <- function(cl,overlap,thres=0.1,scale=TRUE,magni=30,P=NULL){
  
     x <- prcomp(cl[[1]],scale=TRUE)
   if (!missing(P)){
         x[[2]] <- P
     }
   x[[5]] <- t(t(x[[2]])%*% t(cl[[1]]))
   plot(x[[5]][,1],x[[5]][,2],xlab="PC1",ylab="PC2",type="n")
  
   for (i in 1:dim(x[[5]])[[1]]){
     for (j in 1:dim(x[[5]])[[1]]){
       if (thres < overlap[i,j]){
         lines(c(x[[5]][i,1],x[[5]][j,1]),c(x[[5]][i,2],x[[5]][j,2]),col="blue",lwd=magni*overlap[i,j])
       }
     }
   }
  for (i in 1:dim(x[[5]])[[1]]){
     points(x[[5]][i,1],x[[5]][i,2],pch=20,cex=4,col="red",lwd=2)
     points(x[[5]][i,1],x[[5]][i,2],pch=21,cex=4,col="red",lwd=2)
     text(x[[5]][i,1],x[[5]][i,2],i,font=2)
   }
   
x[[2]]
}

       
