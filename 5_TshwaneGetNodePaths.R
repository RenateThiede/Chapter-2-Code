#1. Get unique ward pairs
ward.pairs <- as.data.frame(matrix(c("ward.1","ward.2"),nrow=1,ncol=2))

#Obtain pairs of adjacent wards
for (i in 1:nrow(ward.IDs)){
  row.idx <- which(rownames(TshwaneDF)==ward.IDs[i,])
  for (j in 1:nrow(ward.IDs)){
    col.idx <- which(rownames(TshwaneDF)==ward.IDs[j,])
    if (TshwaneDF[row.idx,col.idx]==1){
      row <- c(ward.IDs[i,],ward.IDs[j,])
      ward.pairs<-rbind(ward.pairs,row)
    }
  }
}
ward.pairs <- ward.pairs[2:nrow(ward.pairs),]

#Sort ward pairs - smallest ward code first
for (i in 1:nrow(ward.pairs)){
  if (ward.pairs[i,1] > ward.pairs[i,2]){
    max <- ward.pairs[i,1]
    min <- ward.pairs[i,2]
    ward.pairs[i,1] <- min
    ward.pairs[i,2] <- max
  }
}
ward.pairs <- ward.pairs[order(ward.pairs$V1,ward.pairs$V2),]
ward.pairs <- unique(ward.pairs)
colnames(ward.pairs) <- c("ward.1","ward.2")

#2. All possible unique Louvain pairs
#Pairs of wards with nodes listed
ward.pairs.nodes.list <- ward.pairs
colnames(ward.pairs.nodes.list) <- c("ward.1","ward.2")
ward.pairs.nodes.list$list.1 <- 0
ward.pairs.nodes.list$list.2 <- 0
  
for (i in 1:nrow(ward.pairs)){
  ward.1 <- ward.pairs.nodes.list[i,1]
  ward.2 <- ward.pairs.nodes.list[i,2]
  
  ward.pairs.nodes.list[i,3] <- list(nodes.nearest.df[nodes.nearest.df$ID == ward.1,'nodes'])
  ward.pairs.nodes.list[i,4] <- list(nodes.nearest.df[nodes.nearest.df$ID == ward.2,'nodes'])
} 

#Pairs of wards expanded
ward.pairs.nodes <- matrix(c(0,0,0,0), nrow=1, ncol=4)
colnames(ward.pairs.nodes) <- c("ward.1","ward.2","node.1","node.2")
for (i in 1:nrow(ward.pairs.nodes.list)){
  ward.1 <- ward.pairs.nodes.list[i,]$ward.1
  ward.2 <- ward.pairs.nodes.list[i,]$ward.2
  
  list.1 <- unlist(ward.pairs.nodes.list[i,]$list.1)
  list.2 <- unlist(ward.pairs.nodes.list[i,]$list.2)
  
  grid <- expand.grid(list.1,list.2)
  grid <- cbind(ward.2,grid)
  grid <- cbind(ward.1,grid)
  colnames(grid) <- c("ward.1","ward.2","node.1","node.2")
  ward.pairs.nodes<-rbind(ward.pairs.nodes,grid)
}
ward.pairs.nodes <- ward.pairs.nodes[2:nrow(ward.pairs.nodes),]
###############################################################################
#NEXT: GET UNIQUE PAIRS
ward.pairs.nodes <- ward.pairs.nodes[order(ward.pairs.nodes$ward.1,
                                           ward.pairs.nodes$ward.2,
                                           ward.pairs.nodes$node.1,
                                           ward.pairs.nodes$node.2),]
ward.pairs.nodes <- ward.pairs.nodes %>% 
  filter(!duplicated(paste0(pmax(node.1, node.2), pmin(node.1, node.2))))

#Test:
for (i in 1:nrow(ward.pairs.nodes)){
  wardcode1 <- ward.pairs.nodes[i,]$ward.1
  if (!(ward.pairs.nodes[i,]$node.1 %in% nodes.nearest.df[nodes.nearest.df$ID == wardcode1,]$nodes[[1]])){
    print("flag")
  }
}
for (i in 1:nrow(ward.pairs.nodes)){
  wardcode2 <- ward.pairs.nodes[i,]$ward.2
  if (!(ward.pairs.nodes[i,]$node.2 %in% nodes.nearest.df[nodes.nearest.df$ID == wardcode2,]$nodes[[1]])){
    print("flag")
  }
}

###############################################################################
#3. Now calculate shortest paths between the nodes.
#ward.pairs.nodes$path <- 0
for (i in 357:nrow(ward.pairs.nodes.list)){
  tryCatch({
  ward.1 <- ward.pairs.nodes.list[i,]$ward.1
  ward.2 <- ward.pairs.nodes.list[i,]$ward.2
  ward.pair <- ts.wards[ts.wards$ADM4_PCODE %in% c(ward.1,ward.2),]
  road.pair <- ts.roads.connected %>% 
    activate("nodes") %>% 
    st_filter(ward.pair, .predicate = st_intersects) %>%
    mutate(cmp = group_components())
  
  submatrix <- ward.pairs.nodes[which(ward.pairs.nodes$ward.1 == ward.1 
                                & ward.pairs.nodes$ward.2 == ward.2),]
  for (j in 1:nrow(submatrix)){
    
    node.1.global <- st_geometry(ts.roads.connected, "nodes")[submatrix[j,'node.1']]
    node.2.global <- st_geometry(ts.roads.connected, "nodes")[submatrix[j,'node.2']] 
    
    node.1.local <- st_nearest_feature(node.1.global, road.pair)
    node.2.local <- st_nearest_feature(node.2.global, road.pair)
    
    #Error handling
    # if (node.1.global != st_geometry(road.pair, "nodes")[node.1.local]){
    #   print("flag: Louvain node cast error")
    # }
    # if (node.2.global != st_geometry(road.pair, "nodes")[node.2.local]){
    #   print("flag: Louvain node cast error")
    # }
    # 
    #Calculate path only if the nodes are on the same component
    if (vertex_attr(road.pair, index = node.1.local)$cmp == vertex_attr(road.pair, index = node.2.local)$cmp){
        path = st_network_paths(road.pair, from = node.1.local, to = node.2.local)
        node.path <- path %>%
          pull(node_paths) %>% 
          unlist()
        
        node.path.global <- 0
        for (k in 1:length(node.path)){
          node.local <- st_geometry(road.pair, "nodes")[node.path[k]]
          node.global <- st_nearest_feature(node.local, ts.roads.connected)
          node.path.global <- c(node.path.global,node.global)
        }
        node.path.global <- node.path.global[2:length(node.path.global)]
        idx <- which(ward.pairs.nodes$node.1 == submatrix[j,'node.1'] &
                       ward.pairs.nodes$node.2 == submatrix[j,'node.2'])
        ward.pairs.nodes[idx,]$path <- list(node.path.global)
    }
  }
  },
  error=function(cond){
    message("Here's the original error message:")
    message(cond)
    print(ward.pairs.nodes.list[i,]$ward.1)
    print(ward.pairs.nodes.list[i,]$ward.2)
  })
}

ncol <- max(lengths(ward.pairs.nodes$path))+4
ward.paths <- as.data.frame(matrix(rep(0,ncol*nrow(ward.pairs.nodes)),nrow=nrow(ward.pairs.nodes),ncol=ncol))
for (i in 1:4){
  ward.paths[,i] <- ward.pairs.nodes[,i]
}

for (i in 1:nrow(ward.pairs.nodes)){
  for (j in 1:length(ward.pairs.nodes[i,]$path[[1]])){
    ward.paths[i,j+4] <- ward.pairs.nodes[i,]$path[[1]][j]
  }
}

#write.csv(ward.paths,file="Data/wardpaths.csv")

#ward.pairs.nodes.backup <- ward.pairs.nodes
#ward.pairs.nodes.list.backup <- ward.pairs.nodes.list
#ward.pairs.nodes[unlist(ward.pairs.nodes$path)==0,]
