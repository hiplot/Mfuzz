cselection <- function(eset,m,crange=seq(4,32,4),repeats=5,visu=TRUE,...){ 

Nonempty <- matrix(0,ncol=length(crange),nrow=repeats) 
i <- 0
for (c in crange){
  i <- i +1
  for (ii in  1:repeats){
  Utmp <- mfuzz(eset,centers=c,m=m)[[4]]#,...)[[4]]
   for (jj in 1:dim(Utmp)[[2]]){
     if((sum(Utmp[,jj] > 0.5))> 0){
       Nonempty[ii,i] <-  Nonempty[ii,i] + 1
     }
   }
 }
}
dimnames(Nonempty) <- list(paste("repeats:",c(1:5),sep=""),paste("c:",crange,sep=""))
if (visu){
plot(crange,Nonempty[1,],pch="x")
for (i in 1:dim(Nonempty)[[1]]) points(crange,Nonempty[i,],pch="x")
lines(c(0,max(crange)),c(0,max(crange)),col="red")
}
Nonempty
}

