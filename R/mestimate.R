mestimate<- function(eset){
  N <-  dim(exprs(eset))[[1]]
  D <- dim(exprs(eset))[[2]]
  m.sj <- 1 + (1418/N + 22.05)*D^(-2) + (12.33/N +0.243)*D^(-0.0406*log(N) - 0.1134)
  return(m.sj)
}


