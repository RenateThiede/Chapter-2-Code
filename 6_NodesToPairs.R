ward.paths.nonzero <- ward.paths[ward.paths$V5 != 0,]

mat <- matrix(rep(0,6),ncol=6)
colnames(mat) <- c("ward.1","ward.2","l.node.1","l.node.2","node.1","node.2")
#print(mat)
for (i in 4542:nrow(ward.paths.nonzero)){
  K <- sum(ward.paths.nonzero[i,5:ncol(ward.paths.nonzero)]!=0)
  if (K==1){
    row <- ward.paths.nonzero[i,1:5]
    row <- cbind(row, 0)
    colnames(row) <- c("ward.1","ward.2","l.node.1","l.node.2","node.1","node.2")
    #print(row)
    mat <- rbind(mat,row)
  }
  else if (K>1){
    for (j in 5:(K+4-1)){
      row <- ward.paths.nonzero[i,1:4]
      row <- cbind(row,ward.paths.nonzero[i,j:(j+1)])
      colnames(row) <- c("ward.1","ward.2","l.node.1","l.node.2","node.1","node.2")
      #print(row)
      mat <- rbind(mat,row)
    }
  }
}
#mat <- mat[2:nrow(mat),]

#write.csv(mat,"Data/node_paths.csv")

### To unique pairs
mat.unique.pairs <- mat[,5:6]
mat.unique.pairs <- mat.unique.pairs[mat.unique.pairs$node.2 != 0,]
mat.unique.pairs <- unique(mat.unique.pairs[,c('node.1','node.2')])


