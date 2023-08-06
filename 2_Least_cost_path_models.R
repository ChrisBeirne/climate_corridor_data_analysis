# Run script 1_ before running this script

cost_final_17N <- raster("Data/output/High_res_resistance_surface_final_17N.tif")
cost_final_17N_NO_NA <- raster("Data/output/High_res_resistance_surface_final_17N_NO_NA.tif")


# View start end end_nodes

leaflet() %>%
  addProviderTiles("OpenStreetMap", group="OSM (default)") %>% 
  addProviderTiles(providers$OpenTopoMap, group="Topo") %>%
  addProviderTiles(providers$Esri.WorldImagery, group="Satellite") %>%  # Add satellite data
  addPolygons(data=start_pas, weight = 2, fillColor = "red", label=start_pas$NAME, stroke=F, fillOpacity=1) %>%
  addPolygons(data=st_transform(end_nodes, 4326), weight = 2, fillColor = "blue", label=, stroke=F, fillOpacity=1) %>%
  #addPolygons(data=tmp, weight = 2, fillColor = "blue") %>%
  # Layers control
  addLayersControl(
    baseGroups = c("OSM (default)","Satellite", "Topo"),
    options = layersControlOptions(collapsed = FALSE)
  )

###################################################
# First define the end nodes

# Give each one an ID code
end_nodes$NODE_CODE <- paste0("H",sprintf("%04d", 1:nrow(end_nodes)))

# Convert to 17N
end_nodes_wgs <- st_transform(end_nodes, 4326)
plot(st_geometry(end_nodes), border=F, col="red")


# Centroids of all contiguous forest fragments
end_points <- sf::st_point_on_surface(end_nodes)

# Try adding in some other locations (to account for large polygons)
set.seed(1200)
test <- st_sample(end_nodes, 250, type = "hexagonal")

# Intersect to get the information back
test <- st_intersection(end_nodes, test)

end_points <- rbind(end_points, test)
# Give them an endpoint code
end_points$END_CODE <- paste0("END",sprintf("%04d", 1:nrow(end_points)))

end_points_wgs<- st_transform(end_points, 4326)
# I need to remove any that are too close
#st_nn(end_points)

leaflet() %>%
  addProviderTiles("OpenStreetMap", group="OSM (default)") %>%
  addProviderTiles("Esri.WorldImagery", group="Satellite") %>%
  #addRasterImage(tmp, group="Raster") %>% 
  addPolygons(data=end_nodes_wgs, fillColor = "yellow",
              group="End_nodes") %>% 
  addCircleMarkers(data=end_points_wgs, col="black", radius=1) %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "Satellite"),
    overlayGroups = c("End_nodes"))

# Convert start PA's to 17N
start_pas_wgs <- start_pas
start_pas <- st_transform(start_pas, 31971)

dir.create("data/output/plots")

# Run the loop
# Make a list to store the files
res_terrestrial <- list()
res_shp <- list()
for(i in 1:nrow(start_pas)) #in 401:485)#
{
    A <- as_Spatial(st_point_on_surface(start_pas[i,]))
    counter <- i#-400
    # Find nearest neighbours - in this case 3
    # If you need it to run fast us k =1
    near_locs <- st_nn(start_pas[i,], end_points, k=5)
    near_locs <- unlist(near_locs)
    tmp_shp <- list()
    tmp_frame <- data.frame("ID"=rep(start_pas$NAME[i], length(near_locs)), "endID"=NA, "length_km"=NA, "conductance"=NA)
    # Create the transition matrix by subsetting the conductance surface
    t1 <- st_as_sf(A)
    t2 <- end_points[near_locs,]
    t1 <- rbind(st_coordinates(t1),st_coordinates(t2))
    t1 <- t1 %>%
    as.data.frame %>%
    sf::st_as_sf(coords = c(1,2))
    #plot(t1)
    # BUFFER
    t2 <- st_buffer(t1, 50000)
    # CROP RASTER
    tmp_ras <- crop(cost_final_17N_NO_NA, st_bbox(t2))
    # Create transition
    cond_derived <- transition(tmp_ras,function(x) mean(x), 8)
    for(j in 1:length(near_locs))     #nrow())
    {
    B <- as_Spatial(end_points[near_locs[j],])
    tmp_frame$endID[j] <- B$NODE_CODE
    # Erase the previous
    lc_path <-NULL
    #plot(cond_1km_utm)
    #plot(A, add=T)
    #plot(B, add=T)
    # I have conductance so I need : function(x) 1/mean(x)
    # Get the shortest path
    lc_path <- gdistance::shortestPath(cond_derived, origin=A,
    goal=B, output="SpatialLines")
    #plot(lc_path, add=T)
    # Need a break if this cannot run
    tmp <- st_as_sf(lc_path)
    if(nrow(st_coordinates(tmp$geometry))>2)
    {
    #plot(st_geometry(tmp))
    #plot(cond_1km, add=T)
    #plot(st_geometry(tmp), add=T, lwd=2)
    #plot(st_geometry(st_centroid(terrestrial_17N[i,])), add=T, pch=19)
    #plot(st_geometry(st_centroid(high_pa_17N[near_locs[j],])), add=T, pch=19, col="red")
    # Distance in km
    tmp_frame$length_km[j] <- round(as.numeric(st_length(tmp))/1000,1)
    # Condusctance units
    tmp3 <- raster::extract(cost_final_17N, tmp)
    tmp_frame$conductance[j] <- sum(tmp3[[1]], na.rm=T)
    # Mean conductancer
    tmp_frame$cond_min[j] <- min(tmp3[[1]], na.rm=T)
    tmp_frame$cond_mean[j] <- mean(tmp3[[1]], na.rm=T)
    tmp$ID <- start_pas$NAME[i]
    tmp$endID <- tmp_frame$endID[j]
    tmp_shp[[j]] <- tmp
    print(paste("terrestrial", i, "- loop", j))
    }
    }
    res_terrestrial[[counter]] <- tmp_frame
    res_shp[[counter]] <- tmp_shp
    png(filename=paste0("data/output/plots/",tmp_frame$ID[1], ".jpg"), width = 800, height = 800)
    plot(tmp_ras, main=tmp_frame$ID[1])
    plot(st_geometry(bind_rows(tmp_shp)), add=T)
    plot(st_geometry(t1), add=T,pch=c(19,2,2,2,2,2) )
    dev.off()
}

res_terrestrial_df <- bind_rows(res_terrestrial)
res_terrestrial_shp <- bind_rows(res_shp)

# Note I actually split this loop into five to run them in parallel.

saveRDS(res_terrestrial_df, "tmp/res_ter_401_485.RDS")
saveRDS(res_terrestrial_shp, "tmp/res_ter_shp_401_485.RDS")

# Read in results dataframe and shapefiles

## Dataframes
df1 <- readRDS("tmp/res_ter_1_100.RDS")
df2 <- readRDS("tmp/res_ter_101_200.RDS")
df3 <- readRDS("tmp/res_ter_201_300.RDS")
df4 <- readRDS("tmp/res_ter_301_400.RDS")
df5 <- readRDS("tmp/res_ter_401_485.RDS")

res_df <- bind_rows(df1, df2, df3,df4,df5)

## Shapefiles
shp1 <- readRDS("tmp/res_ter_shp_1_100.RDS")
shp2 <- readRDS("tmp/res_ter_shp_101_200.RDS")
shp3 <- readRDS("tmp/res_ter_shp_201_300.RDS")
shp4 <- readRDS("tmp/res_ter_shp_301_400.RDS")
shp5 <- readRDS("tmp/res_ter_shp_401_485.RDS")

res_shp <- bind_rows(shp1, shp2, shp3,shp4,shp5)
#plot(st_geometry(res_shp))

# Import the corrections (where the simulation didnt work due to landscape shape)
# update_df <- readRDS("tmp/res_ter_problems.RDS")
# update_shp <- readRDS("tmp/res_ter_shp_problems.RDS")

plot(st_geometry(base_shp))
plot(st_geometry(st_transform(res_shp,4326)), add=T)
plot(st_geometry(st_transform(update_shp,4326)), add=T)

# get bbox for the area
# Do ST difference to get ocea
# # Pull all the transects which go into that
# Rerun those

aoi <- st_as_sfc(st_bbox(base_shp))
fail_aoi <- st_difference(aoi, st_buffer(base_shp,1000))
fail_aoi<- st_transform(fail_aoi,st_crs(res_shp))
fail_aoi <- st_union(fail_aoi)
plot(st_geometry(fail_aoi), col="red")
# Subset to paths which enter the red area
fail_res  <- res_shp[st_intersects(res_shp,fail_aoi, sparse=F)==TRUE,]
plot(st_geometry(fail_res),add=T)

# # Remove the ones with issues from the dataframe
res_shp_tmp <- res_shp[!(res_shp$ID %in% fail_res$ID),]
res_df_tmp <- res_df[!(res_df$ID %in% fail_res$ID),]

# Re-run the code for those failed sites
fail_pas <- start_pas[start_pas$NAME %in% fail_res$ID,]

## Create the transition raster for these sites
# BUFFER
t2 <- st_buffer(st_as_sfc(st_bbox(fail_res)), 50000)
plot(st_geometry(t2), add=T)
# CROP RASTER
tmp_ras <- crop(cost_final_17N_NO_NA, st_bbox(t2))
plot(tmp_ras)
# Create transition
cond_derived <- transition(tmp_ras,function(x) mean(x), 8)


# Make a list to store the files
fail_terrestrial <- list()
fail_shp <- list()

i <- 1
j <- 1
for(i in 1:nrow(fail_pas)) #in 401:485)#
{
  A <- as_Spatial(st_point_on_surface(fail_pas[i,]))
  counter <- i#-400
  # If you need it to run fast us k =1
  near_locs <- st_nn(fail_pas[i,], end_points, k=5)
  near_locs <- unlist(near_locs)
  tmp_shp <- list()
  tmp_frame <- data.frame("ID"=rep(fail_pas$NAME[i], length(near_locs)), "endID"=NA, "length_km"=NA, "conductance"=NA)
  # Create the transition matrix by subsetting the conductance surface
  t1 <- st_as_sf(A)
  t2 <- end_points[near_locs,]
  t1 <- rbind(st_coordinates(t1),st_coordinates(t2))
  t1 <- t1 %>%
    as.data.frame %>%
    sf::st_as_sf(coords = c(1,2))

  for(j in 1:length(near_locs))     #nrow())
  {
    B <- as_Spatial(end_points[near_locs[j],])
    tmp_frame$endID[j] <- B$NODE_CODE
    # Erase the previous
    lc_path <-NULL
    #plot(cond_1km_utm)
    #plot(A, add=T)
    #plot(B, add=T)
    # I have conductance so I need : function(x) 1/mean(x)
    # Get the shortest path
    lc_path <- gdistance::shortestPath(cond_derived, origin=A,
                                       goal=B, output="SpatialLines")
    #plot(lc_path, add=T)
    # Need a break if this cannot run
    tmp <- st_as_sf(lc_path)
    if(nrow(st_coordinates(tmp$geometry))>2)
    {
      #plot(st_geometry(tmp))
      #plot(cond_1km, add=T)
      #plot(st_geometry(tmp), add=T, lwd=2)
      #plot(st_geometry(st_centroid(terrestrial_17N[i,])), add=T, pch=19)
      #plot(st_geometry(st_centroid(high_pa_17N[near_locs[j],])), add=T, pch=19, col="red")
      # Distance in km
      tmp_frame$length_km[j] <- round(as.numeric(st_length(tmp))/1000,1)
      # Condusctance units
      tmp3 <- raster::extract(cost_final_17N, tmp)
      tmp_frame$conductance[j] <- sum(tmp3[[1]], na.rm=T)
      # Mean conductancer
      tmp_frame$cond_min[j] <- min(tmp3[[1]], na.rm=T)
      tmp_frame$cond_mean[j] <- mean(tmp3[[1]], na.rm=T)
      tmp$ID <- fail_pas$NAME[i]
      tmp$endID <- tmp_frame$endID[j]
      tmp_shp[[j]] <- tmp
      print(paste("terrestrial", i, "- loop", j))
    }
  }
  fail_terrestrial[[counter]] <- tmp_frame
  fail_shp[[counter]] <- tmp_shp
  png(filename=paste0("data/output/plots/",tmp_frame$ID[1], ".jpg"), width = 800, height = 800)
  plot(tmp_ras, main=tmp_frame$ID[1])
  plot(st_geometry(bind_rows(tmp_shp)), add=T)
  plot(st_geometry(t1), add=T,pch=c(19,2,2,2,2,2) )
  dev.off()
}

res_fail_df <- bind_rows(fail_terrestrial)
res_fail_shp <- bind_rows(fail_shp)

plot(st_geometry(res_fail_shp), add=T)

saveRDS(res_fail_df, "tmp/res_ter_problems.RDS")
saveRDS(res_fail_shp, "tmp/res_ter_shp_problems.RDS")

# Add back in the results

final_shp <- rbind(res_shp_tmp, res_fail_shp)
final_df <- rbind(res_df_tmp, res_fail_df)


# res_df <- res_df[!(res_df$ID %in% update_df$ID),]
# res_shp <- res_shp[!(res_shp$ID %in% update_shp$ID),]
# 
# # Add in the corrections
# final_df <- bind_rows(res_df, update_df)
# final_shp <- bind_rows(res_shp, update_shp)

# Clean up
remove(df1,df2,df3,df4,df5,shp1,shp2,shp3,shp4,shp5, update_df, update_shp, res_df, res_shp)

# Add in unique ID
final_df$corridor_id <- paste0("CORR", sprintf("%04d", 1:nrow(final_df)))
final_shp$corridor_id <- paste0("CORR", sprintf("%04d", 1:nrow(final_shp)))

# CHECK BOTH FRAMES ARE IN SAME ORDER - table(substr(final_df$ID,1,20)== substr(final_shp$ID,1,20))

# Write key files
dir.create("data/output/corridors")

# Data 
st_write(final_shp, "data/output/corridors/all_corridors.shp", append=F)
write.csv(final_df, "data/output/corridors/all_corridors_df.csv")





plot(st_geometry(st_transform(base_shp, st_crs(final_shp))), col="grey")
plot(st_geometry(final_shp), add=T)

# Also save the protected areas and end nodes
dir.create("data/output/nodes")

# Sort out label consistency
colnames(end_nodes)[colnames(end_nodes)=="NODE_CODE"] <- "endID"
end_nodes$area <- round(end_nodes$area,1)
end_nodes$are_km2 <- round(end_nodes$are_km2,1)

colnames(end_points)[colnames(end_points)=="NODE_CODE"]<- "endID"
end_points$are_km2 <- round(end_points$are_km2,1)
end_points$area <- round(end_points$area,1)

sf::st_write(start_pas, "data/output/nodes/start_nodes.shp")
st_write(end_nodes, "data/output/nodes/end_nodes.shp", append=F)
st_write(end_points, "data/output/nodes/end_points.shp", append=F)




