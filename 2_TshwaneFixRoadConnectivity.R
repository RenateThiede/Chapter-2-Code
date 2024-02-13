#3. This is the third step. Add points on the intersections of the road network
# with the wards. Wards are buffered to ensure the points are WITHIN the wards.

# Loop over all wards.
# For each ward:
# 1. Obtain the intersection points between the BUFFERED wards and the road network.
# 2. Add these nodes into the road network.

#ZA7990001
#i = which(ts.wards$ADM4_PCODE=="ZA7990001")

ts.wards.buffered <- st_read("Data/TshwaneWardsBuffered.shp")
ts.wards.buffered <- st_transform(ts.wards.buffered, 4326)

ts.roads.connected <- ts.roads.clean

ts.roads.connected.sf <- ts.roads.connected %>% activate("edges") %>% st_as_sf()

ggplot() + 
  geom_sf(data = ts.wards.buffered, color="grey1", size = 0.5, fill = "grey") + 
  ggtitle("Wards") + 
  geom_sf(data = ts.roads.connected.sf, color="green", size = 0.5, fill = "grey") + 
  coord_sf()

nodes <- st_cast(st_intersection(ts.roads.connected.sf,
                                 st_cast(ts.wards.buffered, "MULTILINESTRING", 
                                         group_or_split = TRUE)), 
                 "MULTIPOINT")
nodes.point <- st_cast(nodes, "POINT")
ts.roads.connected <- st_network_blend(ts.roads.connected, nodes.point$geometry)

ward <- ts.wards[which(ts.wards$ADM4_PCODE=="ZA7990028"),]
road <- ts.roads.connected %>% 
  activate("nodes") %>% 
  st_filter(ward, .predicate = st_intersects) #%>% 
  #activate("nodes") %>% 
  #filter(!node_is_isolated())
