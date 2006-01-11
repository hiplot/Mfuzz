overlap <- function(cl){
  O <- matrix(NA,nrow=dim(cl[[4]])[2],ncol=dim(cl[[4]])[2])
  for (i in 1:dim(cl[[4]])[2]){
    for (j in 1:dim(cl[[4]])[2]){
      O[i,j] <-  sum(cl[[4]][,i]*cl[[4]][,j])
    }
     }
  for (i in 1:dim(O)[[2]]){
    O[,i] <- O[,i]/sum(O[,i])
  }
O
}
