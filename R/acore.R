acore <-  function(eset,cl,min.acore=0.5){

  atmp <- list(NULL)
  
  for (i in 1:dim(cl[[1]])[[1]]){
    index <- (cl[[3]]==i & (cl[[4]][,i] > min.acore)) # selection of genes
   atmp[[i]] <- data.frame(NAME=dimnames(exprs(eset))[[1]][index],MEM.SHIP=cl[[4]][index,i])
  }

atmp
      }
