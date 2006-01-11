top.count <- function(cl) {
memship <- cl[[4]]
gene.num <- dim(memship)[[1]]
count <- integer(length=gene.num)
count[] <- 0
for(i in 1:dim(memship)[[2]]) {
y <- (max(memship[,i]) <= memship[,i])
index <- which(y)
count[index] <- count[index] + 1
}
return(count)
}
 
