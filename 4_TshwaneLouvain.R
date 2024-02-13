#5. This is the fifth step. Here, we identify the Louvain nodes per ward.

#Idea:
# For each ward:
#   Subset the road network
#   Obtain the Louvain points of each ward
#   Project them onto the full road network
#   Store this vector in a list.
#   Each list item should also have the ward ID attached.

#nodes.nearest.df.backup <- nodes.nearest.df

ward.IDs <- as.data.frame(ts.wards$ADM4_PCODE)
colnames(ward.IDs) <- "ID"

nodes.nearest.df <- data.frame(ID=ward.IDs$ID)
nodes.nearest.df$nodes <- 0

for (i in 1:nrow(ts.wards)){
  tryCatch({ 
    #i = which(ts.wards$ADM4_PCODE=="ZA7990067")
    ward <- ts.wards[ts.wards$ADM4_PCODE == ward.IDs[i,],]    
    
    #Obtain the Louvain clustering of each ward's road network. This
    #includes only nodes that are within the ward's boundaries.
    road.louvain <- ts.roads.connected %>%
      activate("nodes") %>%
      st_filter(ward, .predicate = st_intersects) #%>% 
    if (nrow(road.louvain %>% st_as_sf())>1){
      road.louvain <- road.louvain %>% 
        filter(!(node_is_isolated()))  
    }  
    road.louvain <- road.louvain %>% 
      mutate(louvain = as.factor(group_louvain()))

    #Obtain a matrix with the coordinates in separate columns and the Louvain
    #cluster index. This will allow us to calculate the cluster centroids.
    road.louvain.nodes <- road.louvain %>% activate("nodes") %>% st_as_sf()
    coords <- as.data.frame(st_coordinates(road.louvain.nodes))
    coords$louvain <- road.louvain.nodes$louvain
      
    #Setup the dataframe that will contain the cluster centroids.
    l = length(unique(road.louvain.nodes$louvain))
    centroids <- as.data.frame(matrix(c(1:l,rep(0,l*3)),nrow=l,ncol=4))
    colnames(centroids) <- c("Group", "X", "Y", "Node")
      
    #Obtain cluster centroids.
    for (j in 1:l){
      cluster <- coords[coords$louvain==j,]
      centroids[j,2] <- mean(cluster$X)
      centroids[j,3] <- mean(cluster$Y)
      point <- st_sfc(st_point(c(centroids[j,2],centroids[j,3])),crs=4326)

      #The below line projects the cluster centroid onto the nearest node within
      #the relevant ward's network.
      idx <- st_nearest_feature(point, road.louvain)
      centroids[j,4] <- st_nearest_feature(st_geometry(road.louvain, "nodes")[idx], ts.roads.connected)
      
      if (st_geometry(ts.roads.connected, "nodes")[centroids[j,4]] != st_geometry(road.louvain, "nodes")[idx]){
        print("flag")
        print(st_geometry(ts.roads.connected, "nodes")[centroids[j,4]])
        print(st_geometry(road.louvain, "nodes")[idx])
      }
    }
      nodes.nearest.df[nodes.nearest.df$ID == ward.IDs[i,],]$nodes <- list(unique(centroids$Node))
  },
  error=function(cond){
    message("Here's the original error message:")
    message(cond)
    print(ts.wards[i,]$ADM4_PCODE)
  })
}

nodes.nearest.df <- nodes.nearest.df[order(nodes.nearest.df$ID),]


