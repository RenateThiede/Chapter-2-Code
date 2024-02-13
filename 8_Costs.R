costs <- read_csv("Data/costs_filled.csv")

### Costs per path ###
ward.paths.costs <- ward.paths.nonzero[,1:4]
colnames(ward.paths.costs) <- c("ward.1","ward.2","lvn.node.1","lvn.node.2")
ward.paths.costs$time <- 0
ward.paths.costs$dist <- 0
ward.paths.costs$speed <- 0

for (i in 1:nrow(ward.paths.nonzero)){
  idx.path.costs <- which(ward.paths.costs$lvn.node.1 == ward.paths.nonzero[i,]$V3 &
                 ward.paths.costs$lvn.node.2 == ward.paths.nonzero[i,]$V4)
  #print(idx.path.costs)
  K <- sum(ward.paths.nonzero[i,5:ncol(ward.paths.nonzero)] != 0)
  #print(K)
  # if (K == 1){ Do nothing if K==1 since the cost is then 0
  #   ward.paths.costs[i,]$time <- 0
  #   ward.paths.costs[i,]$dist <- 0
  #   ward.paths.costs[i,]$speed <- 0
  # }
  if (K>1){
    time <- 0
    dist <- 0
    cost <- 0
    for (j in 5:(4+K-1)){
      idx.cost <- which(costs$node.1 == ward.paths.nonzero[i,j] &
                          costs$node.2 == ward.paths.nonzero[i,j+1])
      #print("idx")
      #print(idx.cost)
      time <- time + costs[idx.cost,'time']
      dist <- dist + costs[idx.cost,'dist']
      #print("time")
      #print(time)
      #print("dist")
      #print(dist)
    }
    ward.paths.costs[i,]$time <- time
    ward.paths.costs[i,]$dist <- dist
  }
}

### Costs per ward pair ###
ward.pairs.costs <- ward.pairs
ward.pairs.costs$time <- 0
ward.pairs.costs$dist <- 0
ward.pairs.costs$speed <- 0

for (i in 1:nrow(ward.pairs.costs)){
  idx <- which(ward.paths.costs$ward.1 == ward.pairs[i,]$ward.1 &
                 ward.paths.costs$ward.2 == ward.pairs[i,]$ward.2)
  ward.pairs.costs[i,]$time <- sum(unlist(ward.paths.costs[idx,'time']))
  ward.pairs.costs[i,]$dist <- sum(unlist(ward.paths.costs[idx,'dist']))
}
ward.pairs.costs$speed <- ward.pairs.costs$dist / ward.pairs.costs$time
ward.pairs.costs[which(is.na(ward.pairs.costs$speed)),]$speed <- 0

### Cost matrices ###
Tshwane.time <- matrix(rep(0,nrow(ward.IDs)**2),nrow=nrow(ward.IDs),ncol=nrow(ward.IDs))
colnames(Tshwane.time) <- ward.IDs$ID
rownames(Tshwane.time) <- ward.IDs$ID
Tshwane.dist <- Tshwane.time
Tshwane.speed <- Tshwane.time

#Assume here that the matrices are symmetric & not sorted & have the same row and colnames
for (i in 1:nrow(ward.pairs.costs)){
  idx.row <- which(rownames(Tshwane.time)==ward.pairs.costs[i,]$ward.1)
  idx.col <- which(colnames(Tshwane.time)==ward.pairs.costs[i,]$ward.2)
  
  Tshwane.time[idx.row,idx.col] <- ward.pairs.costs[i,]$time
  Tshwane.time[idx.col,idx.row] <- ward.pairs.costs[i,]$time
  
  Tshwane.dist[idx.row,idx.col] <- ward.pairs.costs[i,]$dist
  Tshwane.dist[idx.col,idx.row] <- ward.pairs.costs[i,]$dist
  
  Tshwane.speed[idx.row,idx.col] <- ward.pairs.costs[i,]$speed
  Tshwane.speed[idx.col,idx.row] <- ward.pairs.costs[i,]$speed
}

Costs.total.time <- Tshwane.time / rowSums(Tshwane.time)
Costs.total.dist <- Tshwane.dist / rowSums(Tshwane.dist)
Costs.total.speed <- Tshwane.speed / rowSums(Tshwane.speed)

### PROBABILITIES ###
#For time: Higher probability means LESS time (inverse proportionality)
#Probs.time <- 1/Tshwane.time
#Probs.time[which(Probs.time == Inf)] <- 0
#Probs.time <- Probs.time / rowSums(Probs.time)

#For distance: Higher probability means LESS distance (inverse proportionality)
# Probs.dist <- 1/Tshwane.dist
# Probs.dist[which(Probs.dist == Inf)] <- 0
# Probs.dist <- Probs.dist / rowSums(Probs.dist)
# 
# #For speed: Higher probability means MORE speed (direct proportionality)
# Probs.speed <- Tshwane.speed / rowSums(Tshwane.speed)