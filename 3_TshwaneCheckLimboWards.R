ts.roads.fix.limbo <- ts.roads.connected
limbo.list <- "a"

for (i in 1:nrow(ts.wards)){
  tryCatch({
    ward <- ts.wards[i,]
    road <- ts.roads.connected %>% 
      activate("nodes") %>% 
      st_filter(ward, .predicate = st_intersects) %>% 
      st_as_sf()
    if (nrow(road)==0){
      print(ward$ADM4_PCODE)
      limbo.list <- c(limbo.list,ward$ADM4_PCODE)
      }
  },
  error=function(cond){
    print(ts.wards[i,]$ADM4_PCODE)
    print("message")
    print(cond)
  })
}