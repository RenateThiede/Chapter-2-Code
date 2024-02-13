library(SparseM);library(Matrix)
mat_sparse <- Matrix(Costs.avg.speed, sparse=TRUE) #Matrix(Costs.avg.dist, sparse=TRUE)
mat_dense <- as.matrix(mat_sparse)

sparse.power.fun = function(P,n){
  Pn <- P
  for (i in 1:(n-1)){
    Pn <- Pn %*% P
  }
  return(Pn)
}

source.idx <- 3
dest.idx <- 2
mat.pow <- mat_sparse
for (i in 2:9){
  #print(i)
  mat.pow <- sparse.power.fun(mat_sparse,i)
  dense <- as.matrix(mat.pow)
  if (round(dense[source.idx,dest.idx],8)>0){
    print(i)
    print(dense[source.idx,dest.idx])
  }
}

i=2
xvec <- rep(0,10000)
yvec <- rep(0,10000)
yvec[1] <- 100
diff=100
while (diff > 1e-06){
  mat.i <- sparse.power.fun(mat_sparse,i)
  mat.i.dense <- as.matrix(mat.i)
  rownames(mat.i.dense) <- colnames(mat.i.dense)
  diff <- abs(mat.i.dense[source.idx,dest.idx] - yvec[i-1])
  #print(diff)
  xvec[i] <- i
  yvec[i] <- mat.i.dense[source.idx,dest.idx]
  i=i+1
}

mat.1000 <- as.matrix(sparse.power.fun(mat_sparse,1000))
mat.10000 <- as.matrix(sparse.power.fun(mat_sparse,10000))




