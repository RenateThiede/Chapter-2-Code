mat.coords <- mat.unique.pairs
mat.coords$node1.x <- 0
mat.coords$node1.y <- 0
mat.coords$node2.x <- 0
mat.coords$node2.y <- 0

for (i in 4:nrow(mat.coords)){
  node1 <- st_geometry(ts.roads.connected, "nodes")[mat.coords[i,'node.1']]
  mat.coords[i,]$node1.x <- node1[[1]][2]
  mat.coords[i,]$node1.y <- node1[[1]][1]
  
  node2 <- st_geometry(ts.roads.connected, "nodes")[mat.coords[i,'node.2']]
  mat.coords[i,]$node2.x <- node2[[1]][2]
  mat.coords[i,]$node2.y <- node2[[1]][1]
}

mat.costs <- mat.coords
mat.costs$time <- 0
mat.costs$dist <- 0
mat.costs$time.text <- 0
mat.costs$dist.text <- 0

write.csv(mat.coords,"Data/coords.csv")
write.csv(mat.costs,"Data/costs.csv")



unequal.x <- mat.coords[mat.coords$node1.x != mat.coords$node2.x,]



