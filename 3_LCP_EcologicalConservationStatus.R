############ Ecological/conservation status of climate corridors ##############
# Date: 8-1-23
# updated: 8-10-23; add end node % protection
# Author: Ian McCullough, immccull@gmail.com
###############################################################################

#### R libraries ####
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(gridExtra)

#### Input data ####
setwd("C:/Users/immcc/Documents/climate_corridor_data_analysis")

# from Chris' analysis (scripts 1 and 2)
LCPs <- terra::vect("paper_data/corridors/all_corridors.shp")
end_nodes <- terra::vect("paper_data/nodes/end_nodes.shp")
start_nodes <- terra::vect("paper_data/nodes/start_nodes.shp")
#start_nodes <- terra::vect("paper_data/nodes/start_nodes_wID.shp") #with dupes removed
corridor_df <- read.csv("paper_data/corridors/all_corridors_df.csv")[,c(2:7)]

# set LCP projection, plot to make sure line up
LCPs <- terra::project(LCPs, "EPSG:31971")
#plot(LCPs)
#plot(end_nodes, add=T, col='red')
#plot(start_nodes, add=T, col='blue')

# Area of interest
aoi <- terra::vect("C:/Users/immcc/Documents/Osa-Conservation-Connectivity-Project/data/spatial/area_of_interest/aoi.shp")
aoi <- terra::project(aoi, "EPSG:31971")

# countries in study area
focal_countries <- terra::vect("C:/Users/immcc/Documents/Osa-Conservation-Connectivity-Project/data/spatial/area_of_interest/focal_countries.shp")
focal_countries <- terra::project(focal_countries, "EPSG:31971")

all_countries <- terra::vect("C:/Users/immcc/Documents/Osa-Conservation-Connectivity-Project/data/spatial/area_of_interest/all_countries_expanded.shp")
all_countries <- terra::project(all_countries, "EPSG:31971")

# Key biodiversity areas
KBAs <- terra::vect("C:/Users/immcc/Documents/Osa-Conservation-Connectivity-Project/data/spatial/KBA/Americas_KBA.shp")
KBAs <- terra::project(KBAs, "EPSG:31971")

# # Protected areas
# focal_pa <- readRDS("C:/Users/immcc/Documents/Osa-Conservation-Connectivity-Project/data/spatial/protected_areas/focal_area_pa_shp.RDS")
# focal_pa <- st_transform(focal_pa,st_crs(31971)) 
# focal_pa <- focal_pa %>% st_sf() %>%  st_cast()
# 
# # filter protected areas
# # for now, per Saura et al. 2018: https://www.sciencedirect.com/science/article/pii/S0006320717312284
# # with help from: 
# # Citation
# # UNEP-WCMC (2019). User Manual for the World Database on Protected Areas and world database on other
# # effective area-based conservation measures: 1.6. UNEP-WCMC: Cambridge, UK. Available at:
# #  http://wcmc.io/WDPA_Manual
# focal_pa <- subset(focal_pa, MARINE %in% c(0,1)) #keep terrestrial and coastal
# focal_pa <- subset(focal_pa, STATUS %in% c('Designated','Established','Inscribed')) #exclude "Proposed"; no "Adopted" in this dataset
# #focal_pa <- subset(focal_pa, IUCN_CAT %in% c('Ia','Ib','II','III','IV','V','VI')) #remove 'Not Reported' and 'Not Applicable'
# #focal_pa <- subset(focal_pa, !(ISO3 %in% c('CRI;PAN'))) #for now, removing one PA shared by Costa Rica/Panama
# focal_pa$ISO3 <- ifelse(focal_pa$ISO3=='CRI;PAN', 'CRI', focal_pa$ISO3) #one PA is desginated as both in CR and Panama; designating as CR for now because more of it occurs in CR
# focal_pa <- subset(focal_pa, !(DESIG_ENG == 'UNESCO-MAB Biosphere Reserve'))
# 
# focal_pa <- terra::vect(focal_pa)   
# focal_pa <- terra::project(focal_pa, "EPSG:31971")
# #writeVector(focal_pa, filename='paper_data/protected_areas/focal_pa.shp', filetype='ESRI Shapefile')
# (sum(terra::expanse(focal_pa))/1000)/(sum(terra::expanse(focal_countries))/1000)

## UPDATE: use all PAs, since some corridors may cross through MX and Colombia
# Protected areas
all_pa <- readRDS("C:/Users/immcc/Documents/Osa-Conservation-Connectivity-Project/data/spatial/protected_areas/all_area_pa_shp.RDS")
all_pa <- st_transform(all_pa,st_crs(31971)) 
all_pa <- all_pa %>% st_sf() %>%  st_cast()

# filter protected areas
# for now, per Saura et al. 2018: https://www.sciencedirect.com/science/article/pii/S0006320717312284
# with help from: 
# Citation
# UNEP-WCMC (2019). User Manual for the World Database on Protected Areas and world database on other
# effective area-based conservation measures: 1.6. UNEP-WCMC: Cambridge, UK. Available at:
#  http://wcmc.io/WDPA_Manual
all_pa <- subset(all_pa, MARINE %in% c("partial","terrestrial")) #keep terrestrial and coastal
all_pa <- subset(all_pa, STATUS %in% c('Designated','Established','Inscribed')) #exclude "Proposed"; no "Adopted" in this dataset
#all_pa <- subset(all_pa, IUCN_CAT %in% c('Ia','Ib','II','III','IV','V','VI')) #remove 'Not Reported' and 'Not Applicable'
#all_pa <- subset(all_pa, !(ISO3 %in% c('CRI;PAN'))) #for now, removing one PA shared by Costa Rica/Panama
all_pa$ISO3 <- ifelse(all_pa$ISO3=='CRI;PAN', 'CRI', all_pa$ISO3) #one PA is desginated as both in CR and Panama; designating as CR for now because more of it occurs in CR
all_pa <- subset(all_pa, !(DESIG_ENG == 'UNESCO-MAB Biosphere Reserve'))

all_pa <- terra::vect(all_pa)   
all_pa <- terra::project(all_pa, "EPSG:31971")
#writeVector(all_pa, filename='paper_data/protected_areas/all_pa.shp', filetype='ESRI Shapefile')
(sum(terra::expanse(all_pa))/1000)/(sum(terra::expanse(all_countries))/1000)

#srtm_all <- terra::rast("C:/Users/immcc/Documents/Osa-Conservation-Connectivity-Project/data/spatial/SRTM90_V4/SRTM90_V4.elevation_all.tif")
#MAT <- terra::rast("C:/Users/immcc/Documents/Osa-Conservation-Connectivity-Project/data/WorldClim/wc2.1_30s_bio/wc2.1_30s_bio_1.tif")
#MAP <- terra::rast("C:/Users/immcc/Documents/Osa-Conservation-Connectivity-Project/data/WorldClim/wc2.1_30s_bio/wc2.1_30s_bio_12.tif")

# process elevation data: only run once, can just load output thereafter
#srtm_all_proj <- terra::project(srtm_all, "EPSG:31971", 
#                                method='average', res=c(90,90))
#terra::writeRaster(srtm_all_proj, filename='paper_data/SRTM/SRTM_90m_31971_all.tif', overwrite=T)
#srtm_all_proj_mask <- terra::mask(srtm_all_proj, aoi, inverse=F,
#                                  filename='paper_data/SRTM/SRTM_90m_31971_aoi.tif')
#srtm <- terra::rast("paper_data/SRTM/SRTM_90m_31971_all.tif")

# Biomass
# biomass <- terra::rast("C:/Users/immcc/Documents/Osa-Conservation-Connectivity-Project/data/spatial/biomass/NASA_biomass_desnity_estimation.tif")
# biomass <- terra::project(biomass, "EPSG:31971",
#                          method='average', res=c(300,300))
# hist(biomass)

# end node % protection (QGIS Overlap Analysis)
end_nodes_protection <- terra::vect("paper_data/nodes/end_nodes_protection.shp")
end_nodes_protection_df <- as.data.frame(end_nodes_protection)
summary(end_nodes_protection_df$all_pa_f_1)
hist(end_nodes_protection_df$all_pa_f_1)
nrow(subset(end_nodes_protection_df, all_pa_f_1==0))

# if already run/saved, read in intermediate outputs
end_nodes_elev_df <- read.csv("paper_data/EcologicalConservationStatus/end_nodes_elevation_stats.csv")
start_nodes_elev_df <- read.csv("paper_data/EcologicalConservationStatus/start_nodes_elevation_stats.csv")
LCPs_buffer100m_elev_df <- read.csv("paper_data/EcologicalConservationStatus/LCP_elevation_stats.csv")
LCPs_biomass_df <- read.csv("paper_data/EcologicalConservationStatus/LCP_biomass_stats.csv")
LCPs_PAs_df <- read.csv("paper_data/EcologicalConservationStatus/LCPs_PA_KBA_summary.csv")
bordercrossing_df <- read.csv('paper_data/EcologicalConservationStatus/bordercrossing_allcountries.csv')

# complete table (merger of others above)
# manually filled in missing start node matches (merge failed due to slight differences in names, often due to accent marks)
full_LCP_df <- read.csv("paper_data/EcologicalConservationStatus/full_LCP_data_all_fixed.csv")

###### Main program ######
## filter LCPs: get rid of duplicates
#unique_corridor_df <- dplyr::distinct(corridor_df)#can't use LCPs because doesn't have right attributes
#unique_corridor_IDs <- unique(unique_corridor_df$)##AUGH there is no unique ID in the corridor_df!!
LCPs_df <- as.data.frame(LCPs)
LCPs_df <- cbind.data.frame(LCPs_df, corridor_df)# rather not do it this way
# all.equal(LCPs_df[,1], LCPs_df[,4])
# all.equal(LCPs_df[,2], LCPs_df[,5])
# this will identify the rows that don't match; can visually inspect
# warning: takes a minute or so
# OK to proceed with cbinded table
# df_temp <- LCPs_df[,c(1,4)] %>%
#   #select('A', 'C', 'D') %>%
#   rowwise %>%
#   mutate(match = n_distinct(unlist(cur_data())) == 1) %>%
#   ungroup()

#LCPs_df <- LCPs_df[,c(1,2,3,6,7,8,9)]
colnames(LCPs_df) <- c('ID','endID','crrdr_d','ID_b','endID_b','length_km','conductance','cond_min','cond_mean')

unique_corridor_df <- dplyr::distinct(LCPs_df, ID, endID, ID_b, endID_b, length_km, 
                        conductance, cond_min, cond_mean, .keep_all=T)
unique_corridor_IDs <- unique(unique_corridor_df$crrdr_d)

#test <- terra::subset(LCPs, LCPs$crrdr_d %in% unique_corridor_IDs)
LCPs_noDupes <- terra::subset(LCPs, LCPs$crrdr_d %in% unique_corridor_IDs)
#length(unique(LCPs_noDupes$crrdr_d))
#writeVector(LCPs_noDupes, file='paper_data/corridors/all_corridors_noDupes.shp')

## Remove duplicate starting nodes
# to be conservative, only removing ones that have the same name and area
# which are likely the same polygon replicated
# create unique ID for starting nodes (will need it later)
start_nodes$startnode_ID <- seq(1, nrow(start_nodes), 1)
start_nodes_df <- as.data.frame(start_nodes)
start_nodes_df <- start_nodes_df[,c('NAME','startnode_ID','LOW_ARE','ISO3')]

test <- start_nodes_df %>%
  distinct(NAME, LOW_ARE, .keep_all = T)
unique_start_nodes_IDs <- test$startnode_ID

start_nodes_unique <- terra::subset(start_nodes, start_nodes$startnode_ID %in% unique_start_nodes_IDs)

#writeVector(start_nodes_unique, "paper_data/nodes/start_nodes_wID.shp", overwrite=T)

# summarize start nodes by country
# NA is Belize Maya (so add 1 more to Belize)
start_nodes_df %>%
  dplyr::group_by(ISO3) %>%
  dplyr::summarize(nStartNodes=n())

## extract elevation data for target end nodes
# NA output are due to nodes located outside our AOI
# end_nodes_elev_mean <- terra::extract(srtm, end_nodes, fun='mean', na.rm=T)
# end_nodes_elev_min <- terra::extract(srtm, end_nodes, fun='min', na.rm=T)
# end_nodes_elev_max <- terra::extract(srtm, end_nodes, fun='max', na.rm=T)
# end_nodes_elev <- cbind.data.frame(end_nodes_elev_mean, end_nodes_elev_min, end_nodes_elev_max)
# end_nodes_elev <- end_nodes_elev[,c(1,2,4,6)]
# colnames(end_nodes_elev) <- c('ID','mean_m','min_m','max_m')
# end_nodes_elev$range_m <- end_nodes_elev$max_m - end_nodes_elev$min_m
# 
# end_nodes_elev_df <- as.data.frame(end_nodes)
# end_nodes_elev_df <- cbind.data.frame(end_nodes_elev_df[,c(2:4)], end_nodes_elev[,c(2:5)])
# #write.csv(end_nodes_elev_df, file='paper_data/EcologicalConservationStatus/end_nodes_elevation_stats.csv', row.names=F)
# 
# # repeat for starting nodes
# start_nodes_elev_mean <- terra::extract(srtm, start_nodes_unique, fun='mean', na.rm=T)
# start_nodes_elev_min <- terra::extract(srtm, start_nodes_unique, fun='min', na.rm=T)
# start_nodes_elev_max <- terra::extract(srtm, start_nodes_unique, fun='max', na.rm=T)
# start_nodes_elev <- cbind.data.frame(start_nodes_elev_mean, start_nodes_elev_min, start_nodes_elev_max)
# start_nodes_elev <- start_nodes_elev[,c(1,2,4,6)]
# colnames(start_nodes_elev) <- c('ID','mean_m','min_m','max_m')
# start_nodes_elev$range_m <- start_nodes_elev$max_m - start_nodes_elev$min_m
# 
# start_nodes_elev_df <- as.data.frame(start_nodes_unique)
# start_nodes_elev_df <- cbind.data.frame(start_nodes_elev_df, start_nodes_elev[,c(2:5)])
# 
# # calculate starting node area
# start_node_area <- terra::expanse(start_nodes_unique, unit='km')
# start_nodes_elev_df$calculatedarea_km2 <- start_node_area
#these should line up! Maybe some were clipped to study area?
#cor(start_nodes_elev_df$GIS_ARE, start_nodes_elev_df$calculatedarea_km2)
#plot(start_nodes_elev_df$GIS_ARE, start_nodes_elev_df$calculatedarea_km2)
#cor(start_nodes_elev_df$LOW_ARE, start_nodes_elev_df$calculatedarea_km2)#good!

#write.csv(start_nodes_elev_df, file='paper_data/EcologicalConservationStatus/start_nodes_elevation_stats.csv', row.names=F)
# 
# ## Basic exploratory plots of start and end nodes
# hist(end_nodes_elev_df$are_km2, main='Target end node area (sq km)',
#      breaks=seq(0,6000,100))
# summary(end_nodes_elev_df$are_km2)
# 
# hist(start_nodes_elev_df$LOW_ARE, main='Starting node area (sq km)',
#      breaks=seq(0,7500,100), xlim=c(0,7500))
# summary(start_nodes_elev_df$LOW_ARE)
# 

#### Climate and elevation data for PAs ####
# prepare/reproject data to UTM 17N
# aoi <- terra::vect("C:/Users/immcc/Documents/Osa-Conservation-Connectivity-Project/data/spatial/area_of_interest/aoi.shp")
# MAT_masked <- terra::mask(MAT, aoi, inverse=F) #mask and crop first before reprojecting for efficiency
# MAT_cropped <- terra::crop(MAT_masked, aoi)
# MAT_cropped <- terra::project(MAT_cropped, "EPSG:31971", 
#                               method='average', res=c())
# 
# MAP_masked <- terra::mask(MAP, aoi, inverse=F) 
# MAP_cropped <- terra::crop(MAP_masked, aoi)
# MAP_cropped <- terra::project(MAP_cropped, "EPSG:31971", 
#                               method='average', res=c())
# 
# focal_pa_MAT_mean <- terra::extract(MAT_cropped, focal_pa, fun='mean', na.rm=T)
# focal_pa_MAT_min <- terra::extract(MAT_cropped, focal_pa, fun='min', na.rm=T)
# focal_pa_MAT_max <- terra::extract(MAT_cropped, focal_pa, fun='max', na.rm=T)
# focal_pa_MAT <- cbind.data.frame(focal_pa_MAT_mean, focal_pa_MAT_min[,2], focal_pa_MAT_max[,2])
# colnames(focal_pa_MAT) <- c('ID','MAT_mean','MAT_min','MAT_max')
# focal_pa_MAT$MAT_range <- focal_pa_MAT$MAT_max-focal_pa_MAT$MAT_min
# 
# focal_pa_MAP_mean <- terra::extract(MAP_cropped, focal_pa, fun='mean', na.rm=T)
# focal_pa_MAP_min <- terra::extract(MAP_cropped, focal_pa, fun='min', na.rm=T)
# focal_pa_MAP_max <- terra::extract(MAP_cropped, focal_pa, fun='max', na.rm=T)
# focal_pa_MAP <- cbind.data.frame(focal_pa_MAP_mean, focal_pa_MAP_min[,2], focal_pa_MAP_max[,2])
# colnames(focal_pa_MAP) <- c('ID','MAP_mean','MAP_min','MAP_max')
# focal_pa_MAP$MAP_range <- focal_pa_MAP$MAP_max-focal_pa_MAP$MAP_min
# 
# focal_pa_srtm_mean <- terra::extract(srtm, focal_pa, fun='mean', na.rm=T)
# focal_pa_srtm_min <- terra::extract(srtm, focal_pa, fun='min', na.rm=T)
# focal_pa_srtm_max <- terra::extract(srtm, focal_pa, fun='max', na.rm=T)
# focal_pa_srtm <- cbind.data.frame(focal_pa_srtm_mean, focal_pa_srtm_min[,2], focal_pa_srtm_max[,2])
# colnames(focal_pa_srtm) <- c('ID','srtm_mean','srtm_min','srtm_max')
# focal_pa_srtm$srtm_range <- focal_pa_srtm$srtm_max-focal_pa_srtm$srtm_min
# 
# focal_pa_df <- as.data.frame(focal_pa)
# focal_pa_df <- focal_pa_df[,c('ISO3','NAME','ORIG_NAME','WDPAID','WDPA_PID')]
# 
# focal_pa_clim_elev <- cbind.data.frame(focal_pa_MAT, focal_pa_MAP[,c(2:5)], focal_pa_srtm[,c(2:5)], focal_pa_df)
# #write.csv(focal_pa_clim_elev, file='paper_data/EcologicalConservationStatus/focal_pa_clim_elev_stats.csv', row.names=F)

# ## Buffer LCPs by resolution of resistance surface (100m)
LCPs_buffer100m <- terra::buffer(LCPs_noDupes, width=100) #only takes seconds
# length(unique(LCPs_buffer100m$endID))
# length(unique(LCPs_buffer100m$crrdr_d))
# #writeVector(LCPs_buffer100m, filename='paper_data/corridors/all_corridors_100mbuff.shp', filetype='ESRI Shapefile', overwrite=T)
# 
# ## extract elevation data for buffered LCPs (caution: slow)
# LCPs_buffer100m_elev_mean <- terra::extract(srtm, LCPs_buffer100m, fun='mean', na.rm=T)
# LCPs_buffer100m_elev_min <- terra::extract(srtm, LCPs_buffer100m, fun='min', na.rm=T)
# LCPs_buffer100m_elev_max <- terra::extract(srtm, LCPs_buffer100m, fun='max', na.rm=T)
# 
# LCPs_buffer100m_elev <- cbind.data.frame(LCPs_buffer100m_elev_mean, LCPs_buffer100m_elev_min, LCPs_buffer100m_elev_max)
# LCPs_buffer100m_elev <- LCPs_buffer100m_elev[,c(1,2,4,6)]
# colnames(LCPs_buffer100m_elev) <- c('ID','mean_m','min_m','max_m')
# LCPs_buffer100m_elev$range_m <- LCPs_buffer100m_elev$max_m - LCPs_buffer100m_elev$min_m
# 
# LCPs_buffer100m_elev_df <- as.data.frame(LCPs_buffer100m)
# LCPs_buffer100m_elev_df <- cbind.data.frame(LCPs_buffer100m_elev_df, LCPs_buffer100m_elev[,c(2:5)])
# #write.csv(LCPs_buffer100m_elev_df, file='paper_data/EcologicalConservationStatus/LCP_elevation_stats.csv', row.names=F)
# 
# ## Basic exploratory plots of elevation data
# hist(LCPs_buffer100m_elev_df$range_m, xlim=c(1000,4000), breaks=seq(0,4000,100),
#      main='Elevational breadth of LCPs',xlab='Elevation (m)', las=1)
# summary(LCPs_buffer100m_elev_df$range_m)

# ## Overlap with KBAs
#first get lengths of LCPs
LCPs_df <- as.data.frame(LCPs_noDupes)
LCPs_df$LCP_length_km <- terra::perim(LCPs_noDupes)/1000

# intersect KBAs and LCPs
LCPs_KBAs <- terra::intersect(KBAs, LCPs_buffer100m)
LCPs_KBAs_df <- as.data.frame(LCPs_KBAs)

# count number of KBAs intersecting with each LCP
LCPs_KBAs_summary <- LCPs_KBAs_df %>%
  dplyr::group_by(crrdr_d) %>%
  dplyr::summarize(nKBAs=n()) %>%
  as.data.frame()
# ones with 0 KBAs won't be in there, so need to join in
LCPs_KBAs_summary <- merge(LCPs_KBAs_summary, LCPs_df, by='crrdr_d', all=T)
LCPs_KBAs_summary <- LCPs_KBAs_summary %>%
  mutate(nKBAs = coalesce(nKBAs, 0))

hist(LCPs_KBAs_summary$nKBAs, main='Number of KBAs crossed by LCPs',
     xlim=c(0,14), breaks=seq(0,13,1), las=1)
summary(LCPs_KBAs_summary$nKBAs)
# #write.csv(LCPs_KBAs_summary, file='paper_data/EcologicalConservationStatus/LCPs_KBAs_summary.csv', row.names=F)


## Overlap with protected areas
# running the next line keeps causing a fatal error; try batching or use QGIS?
#LCPs_PAs <- terra::intersect(focal_pa, LCPs_buffer100m)
#nrow(focal_pa)

#LCPs_PAs1 <- terra::intersect(focal_pa, LCPs_buffer100m[c(1:467),])
#LCPs_PAs2 <- terra::intersect(focal_pa, LCPs_buffer100m[c(468:935),])#iteratively tried increasing the second number; crashed when divided dataset in 2 and 3 equal chunks
#LCPs_PAs3 <- terra::intersect(focal_pa, LCPs_buffer100m[c(936:1403),])
#LCPs_PAs4 <- terra::intersect(focal_pa, LCPs_buffer100m[c(1404:1871),])#crashed

# Plan B, load output from QGIS (Vector Overlay:Intersection)
# this is useful for counting number of PAs that overlap with each LCP
# changed to all pas so can go through MX or Colombia
LCPs_PAs_count <- terra::vect("paper_data/all_pa_LCP100mbuff_intersect/all_pa_LCP100mbuff_intersect.shp")
crs(LCPs_PAs_count) <- "EPSG:31971"
LCPs_PAs_count_df <- as.data.frame(LCPs_PAs_count)
# 
# # count number of protected areas intersecting with each LCP
LCPs_PAs_count_summary <- LCPs_PAs_count_df %>%
  dplyr::group_by(crrdr_d) %>%
  dplyr::summarize(nPAs=n()) %>%
  as.data.frame()
LCPs_PAs_count_summary <- merge(LCPs_PAs_count_summary, LCPs_df, by='crrdr_d', all=T)
LCPs_PAs_count_summary <- LCPs_PAs_count_summary %>%
  mutate(nPAs = coalesce(nPAs, 0))
hist(LCPs_PAs_count_summary$nPAs, main='Number of PAs overlapping with LCPs',
     xlim=c(0,25), breaks=seq(0,25,1), las=1)
summary(LCPs_PAs_count_summary$nPAs)
# 
# # # Percent PA/LCP overlap: Overlap Analysis in QGIS
# changed to all pas so can include MX and Colombia
LCPs_PAs <- terra::vect("paper_data/all_pa_LCP100mbuff_OverlapAnalysis/all_pa_LCP100mbuff_OverlapAnalysis.shp")
LCPs_PAs_df <- as.data.frame(LCPs_PAs)
LCPs_PAs_df$all_pa_area_sqkm <- LCPs_PAs_df$all_pa_fix/1000
LCPs_PAs_df$all_pa_pct_overlap <- LCPs_PAs_df$all_pa_f_1
LCPs_PAs_df <- LCPs_PAs_df[,c(1,2,3,6,7)]
# 
# # merge in PA counts by LCP
LCPs_PAs_df <- merge(LCPs_PAs_df, LCPs_PAs_count_summary[,c(1,2,5)], by='crrdr_d', all.x=T)
# #
# hist(LCPs_PAs_df$nPAs, main='Number of PAs overlapping with each LCP')
# summary(LCPs_PAs_df$nPAs)
# hist(LCPs_PAs_df$focal_pa_pct_overlap, main='LCP % protected')
# summary(LCPs_PAs_df$focal_pa_pct_overlap)
# nrow(subset(LCPs_PAs_df, nPAs < 1))

# add in KBAs
LCPs_PAs_df <- merge(LCPs_PAs_df, LCPs_KBAs_summary[,c(1,2)], by='crrdr_d', all.x=T)
LCPs_PAs_df$nKBAs[is.na(LCPs_PAs_df$nKBAs)] <- 0 #convert NA to 0 for number of KBA column (i.e., places with no overlap indicate 0 overlapping KBAs)

#hist(LCPs_PAs_df$nKBAs, xlim=c(0,13), breaks=seq(0,13,1),
#     main='Number of KBAs overlapping with each LCP')
#summary(LCPs_PAs_df$nKBAs)
#nrow(subset(LCPs_PAs_df, nKBAs < 1))

#write.csv(LCPs_PAs_df, file='paper_data/EcologicalConservationStatus/LCPs_PA_KBA_summary.csv', row.names=F)

## Biomass
# LCP_biomass_mean <- terra::extract(biomass, LCPs_buffer100m, fun='mean', na.rm=T)
# LCP_biomass_min <- terra::extract(biomass, LCPs_buffer100m, fun='min', na.rm=T)
# LCP_biomass_max <- terra::extract(biomass, LCPs_buffer100m, fun='max', na.rm=T)
# 
# LCPs_biomass_df <- cbind.data.frame(as.data.frame(LCPs_noDupes), LCP_biomass_mean, LCP_biomass_min, LCP_biomass_max)
# LCPs_biomass_df <- LCPs_biomass_df[,c(1,2,3,5,7,9)]
# colnames(LCPs_biomass_df) <- c('ID','endID','crrdr_d','biomass_mean','biomass_min','biomass_max')
# #write.csv(LCPs_biomass_df, file='paper_data/EcologicalConservationStatus/LCP_biomass_stats.csv', row.names=F)
# 
# hist(LCPs_biomass_df$biomass_mean, main='LCP biomass', las=1)


#### Multi-panel plot of ecological/conservation status of LCPs ####
end_node_area_plot <- ggplot(data=end_nodes_elev_df, aes(x=are_km2))+
  geom_histogram(breaks=seq(0,6000,50))+
  ggtitle('Target area size')+
  theme_classic()+
  #scale_x_continuous(name='km2')+
  scale_x_continuous(name=expression("km"^2),limits=c(0,6000), breaks=seq(0,6000,1000))+
  #scale_y_continuous(name='Frequency')+
  theme(axis.text.x = element_text(color='black'),
           axis.text.y=element_text(color='black'),
           axis.title.x=element_text(size=9),
           axis.title.y=element_blank())
end_node_area_plot
# jpeg('Figures/end_node_area_plot.jpeg',width = 7,height = 5,units = 'in',res=600)
#   end_node_area_plot
# dev.off()

end_node_elevation_plot <- ggplot(data=end_nodes_elev_df, aes(x=range_m))+
  geom_histogram(breaks=seq(0,3200,50))+
  ggtitle('Target area elevational breadth')+
  theme_classic()+
  scale_x_continuous(name='Elevation (m)',limits=c(0,3200), breaks=seq(0,3200,500))+
  #scale_y_continuous(name='Frequency')+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_text(size=9),
        axis.title.y=element_blank())
end_node_elevation_plot
# jpeg('Figures/end_node_elevation_plot.jpeg',width = 7,height = 5,units = 'in',res=600)
#   end_node_elevation_plot
# dev.off()

LCP_pct_protected_plot <- ggplot(data=LCPs_PAs_df, aes(x=all_pa_pct_overlap))+
  geom_histogram(breaks=seq(0,100,1))+
  ggtitle('Corridor % protected')+
  theme_classic()+
  scale_x_continuous(name='Protection (%)',limits=c(0,100), breaks=seq(0,100,10))+
  #scale_y_continuous(name='Frequency')+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_text(size=9),
        axis.title.y=element_blank())
LCP_pct_protected_plot
# jpeg('Figures/corridor_pct_protected_plot.jpeg',width = 7,height = 5,units = 'in',res=600)
#   LCP_pct_protected_plot
# dev.off()

LCP_PAcount_plot <- ggplot(data=LCPs_PAs_df, aes(x=nPAs))+
  geom_histogram(breaks=seq(0,26,1))+
  ggtitle('Corridor protected area count')+
  theme_classic()+
  scale_x_continuous(name='Number of protected areas',limits=c(0,26), breaks=seq(0,26,2))+
  #scale_y_continuous(name='Frequency')+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_text(size=9),
        axis.title.y=element_blank())
LCP_PAcount_plot
# jpeg('Figures/corridor_PA_count_plot.jpeg',width = 7,height = 5,units = 'in',res=600)
#   LCP_PAcount_plot
# dev.off()

LCP_KBAcount_plot <- ggplot(data=LCPs_PAs_df, aes(x=nKBAs))+
  geom_histogram(breaks=seq(0,13,1))+
  ggtitle('Corridor KBA count')+
  theme_classic()+
  scale_x_continuous(name='Number of KBAs', limits=c(0,13), breaks=seq(0,13,1))+
  #scale_y_continuous(name='Frequency')+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_text(size=9),
        axis.title.y=element_blank())
LCP_KBAcount_plot
# jpeg('Figures/corridor_KBA_count_plot.jpeg',width = 7,height = 5,units = 'in',res=600)
#   LCP_KBAcount_plot
# dev.off()

LCP_length_plot <- ggplot(data=LCPs_PAs_df, aes(x=LCP_length_km))+
  #geom_histogram(binwidth=20)+
  geom_histogram(breaks=seq(0,1200,10))+
  ggtitle('Corridor length')+
  theme_classic()+
  scale_x_continuous(name='km', limits=c(0,1200), breaks=seq(0,1200,200))+
  #scale_y_continuous(name='Frequency')+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_text(size=9),
        axis.title.y=element_blank())
LCP_length_plot
# jpeg('Figures/corridor_length_plot.jpeg',width = 7,height = 5,units = 'in',res=600)
#   LCP_length_plot
# dev.off()

LCP_elevation_plot <- ggplot(data=LCPs_buffer100m_elev_df, aes(x=range_m))+
  #geom_histogram(binwidth=50)+# throws annoying warning
  geom_histogram(breaks=seq(1000,3600,50))+
  ggtitle('Corridor elevational breadth')+
  theme_classic()+
  scale_x_continuous(name='Elevation (m)', limits=c(1000,3600), breaks=seq(1000,3600,500))+
  #scale_x_continuous(name='Elevation (m)')+
  #scale_y_continuous(name='Frequency')+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_text(size=9),
        axis.title.y=element_blank())
LCP_elevation_plot
# jpeg('Figures/corridor_elevation_plot.jpeg',width = 7,height = 5,units = 'in',res=600)
#   LCP_elevation_plot
# dev.off()

LCP_biomass_plot <- ggplot(data=LCPs_biomass_df, aes(x=biomass_mean))+
  #geom_histogram(binwidth=1)+
  geom_histogram(breaks=seq(20,140,1))+
  ggtitle('Corridor forest biomass')+
  theme_classic()+
  scale_x_continuous(name='Biomass (Mt C)', limits=c(20,140), breaks=seq(20,140,20))+
  #scale_y_continuous(name='Frequency')+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_text(size=9),
        axis.title.y=element_blank())
LCP_biomass_plot
# jpeg('Figures/corridor_biomass_plot.jpeg',width = 7,height = 5,units = 'in',res=600)
#    LCP_biomass_plot
# dev.off()

jpeg('Figures/multipanel_EcologicalConservationStatus.jpeg',width = 6,height = 8,units = 'in',res=600)
    grid.arrange(end_node_area_plot, end_node_elevation_plot, LCP_length_plot, LCP_elevation_plot, LCP_PAcount_plot, LCP_pct_protected_plot, 
                 LCP_KBAcount_plot, LCP_biomass_plot, nrow=4)
dev.off()

#### Count international crossings for each LCP ####
# makes sense to use all countries, but filter out later ones only in MX and Colombia
# bordercrossing_list <- list()
# for (i in 1:length(LCPs_noDupes)){
#   test <- LCPs_noDupes[i]
#   test_intersect <- terra::intersect(all_countries, test)
#   test_df <- data.frame(crrdr_d=test$crrdr_d, nCountries=nrow(test_intersect), Countries=toString(sort(unique(test_intersect$iso_a3))))
#   bordercrossing_list[[i]] <- test_df
#   test=NULL
#   test_df=NULL
# }
# bordercrossing_df <- dplyr::bind_rows(bordercrossing_list)
# summary(bordercrossing_df$nCountries)
# nrow(subset(bordercrossing_df, nCountries==1))
# nrow(subset(bordercrossing_df, nCountries==2))
# nrow(subset(bordercrossing_df, nCountries==3))
# nrow(subset(bordercrossing_df, nCountries==4))
# nrow(bordercrossing_df)
#write.csv(bordercrossing_df, file='paper_data/EcologicalConservationStatus/bordercrossing_allcountries.csv', row.names=F)

LCP_bordercrossing_plot <- ggplot(data=bordercrossing_df, aes(x=nCountries))+
  #geom_histogram(binwidth=1)+
  geom_histogram(breaks=seq(0,4,1))+
  ggtitle('Number of countries crossed')+
  theme_classic()+
  scale_x_continuous(name='Number of countries', limits=c(0,4), breaks=seq(0,4,1))+
  #scale_y_continuous(name='Frequency')+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_text(size=9),
        axis.title.y=element_blank())
LCP_bordercrossing_plot
jpeg('Figures/countries_crossed_plot.jpeg',width = 7,height = 5,units = 'in',res=600)
   LCP_bordercrossing_plot
dev.off()

#### Country-level analysis of corridor ecological/conservation status ####
LCP_datasets <- list(LCPs_PAs_df, LCPs_biomass_df, LCPs_buffer100m_elev_df, bordercrossing_df)
full_LCP_df <- Reduce(function(x, y) merge(x, y, all=T), LCP_datasets)

# output from spatial join in QGIS (Join attributes by location), identifying only country with most overlap for each LCP
country_LCP_SJ <- terra::vect("paper_data/focal_country_LCP100mbuff_SJ/focal_country_LCP100mbuff_SJ_mostOverlap.shp")
country_LCP_SJ_df <- as.data.frame(country_LCP_SJ)

full_LCP_df <- merge(full_LCP_df, country_LCP_SJ_df[,c(3,4,5)],by='crrdr_d')
# bring in end node data: elevation and area
full_LCP_df <- merge(full_LCP_df, end_nodes_elev_df[,c(2,3,7)], by='endID')
colnames(full_LCP_df)[15] <- "range_m"
colnames(full_LCP_df)[21] <- "endnode_elevrange_m"
# then protection data
full_LCP_df <- merge(full_LCP_df, end_nodes_protection_df[,c(4,6)], by='endID')
colnames(full_LCP_df)[22] <- 'endnode_pct_protected'

# # bring in start node data
start_node_transfer <- start_nodes_elev_df[,c('ISO3','NAME','range_m','calculatedarea_km2')]
colnames(start_node_transfer) <- c('StartNode_ISO3','StartNodeName','StartNode_elev_range_m','StartNode_calculatedarea_km2')
# # dupes <- start_node_transfer %>%
# #   group_by_all() %>%
# #   filter(n()>1) %>%
# #   ungroup()
# # dupes
start_node_transfer <- dplyr::distinct(start_node_transfer)# some PAs are repeated!
# length(unique(start_node_transfer$StartNodeName))
# 
# # because some PAs are multi-part, it's not clear from this which part LCPs start from
# # can't use spatial join because LCPs may go through additional starting nodes
# # for these cases, just using average area and elevation range across different parts
# 
start_node_transfer <- start_node_transfer %>%
  dplyr::group_by(StartNodeName) %>%
  dplyr::summarize(StartNode_areakm2=mean(StartNode_calculatedarea_km2, na.rm=T),
                   StartNode_elevrange_m=mean(StartNode_elev_range_m, na.rm=T),
                   nParts=n()) %>%
  as.data.frame()

# join to other LCP data: but misses ID matches that don't quite match (such as with accents)
# ended up manually filling in data in Excel
full_LCP_df <- merge(full_LCP_df, start_node_transfer[,c(1:3)], by.x='ID', by.y='StartNodeName', all.x=T)
sum(is.na(full_LCP_df$StartNode_areakm2))
sum(is.na(full_LCP_df$StartNode_elevrange_m))

full_LCP_df7 <- subset(full_LCP_df, iso_a3 %in% c('BLZ','CRI','GTM','NIC','HND','PAN','SLV'))

country_colors7 <- c('BLZ'='blue','CRI'='forestgreen','GTM'='orange','HND'='firebrick',
                     'NIC'='turquoise','PAN'='khaki','SLV'='chartreuse')

nrow(subset(full_LCP_df7, nCountries==1))
nrow(subset(full_LCP_df7, nCountries==2))
nrow(subset(full_LCP_df7, nCountries==3))
nrow(subset(full_LCP_df7, nCountries==4))
nrow(full_LCP_df7)
nrow(subset(full_LCP_df, Countries %in% c('MEX','COL')))

#write.csv(full_LCP_df, file='paper_data/EcologicalConservationStatus/full_LCP_data_all.csv', row.names=F)
#write.csv(full_LCP_df7, file='paper_data/EcologicalConservationStatus/full_LCP_data_7countries.csv', row.names=F)


# number of corridors by primary country
primary_country_LCP_summary <- full_LCP_df %>%
  dplyr::group_by(iso_a3) %>%
  dplyr::summarize(nLCPs=n(),
                   totalLCP_length_km=sum(LCP_length_km))%>%
  as.data.frame()

country_corridor_length_plot <- ggplot(data=as.data.frame(full_LCP_df7), aes(y=LCP_length_km, x=iso_a3, fill=iso_a3)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_blank(),
        plot.title=element_text(size=12),
        legend.position=c('none'))+
  scale_y_continuous(name='Corridor length (km)', limits=c())+
  scale_x_discrete(name='Country')+
  scale_fill_manual(values=country_colors7)+
  ggtitle("a) Corridor length")
country_corridor_length_plot

country_corridor_pct_protection_plot <- ggplot(data=as.data.frame(full_LCP_df7), aes(y=all_pa_pct_overlap, x=iso_a3, fill=iso_a3)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_blank(),
        plot.title=element_text(size=12),
        legend.position=c('none'))+
  scale_y_continuous(name='Protection (%)', limits=c())+
  scale_x_discrete(name='Country')+
  scale_fill_manual(values=country_colors7)+
  ggtitle("c) Percent protected")
country_corridor_pct_protection_plot

country_corridor_count_protection_plot <- ggplot(data=as.data.frame(full_LCP_df7), aes(y=nPAs, x=iso_a3, fill=iso_a3)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_blank(),
        plot.title=element_text(size=12),
        legend.position=c('none'))+
  scale_y_continuous(name='Number of overlapping PAs', limits=c())+
  scale_x_discrete(name='Country')+
  scale_fill_manual(values=country_colors7)+
  ggtitle("d) Number of overlapping PAs")
country_corridor_count_protection_plot

country_corridor_count_KBA_plot <- ggplot(data=as.data.frame(full_LCP_df7), aes(y=nKBAs, x=iso_a3, fill=iso_a3)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_blank(),
        plot.title=element_text(size=12),
        legend.position=c('none'))+
  scale_y_continuous(name='Number of KBAs', limits=c())+
  scale_x_discrete(name='Country')+
  scale_fill_manual(values=country_colors7)+
  ggtitle("e) Number of overlapping KBAs")
country_corridor_count_KBA_plot

country_corridor_biomass_plot <- ggplot(data=as.data.frame(full_LCP_df7), aes(y=biomass_mean, x=iso_a3, fill=iso_a3)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_blank(),
        plot.title=element_text(size=12),
        legend.position=c('none'))+
  scale_y_continuous(name='Mean forest biomass', limits=c())+
  scale_x_discrete(name='Country')+
  scale_fill_manual(values=country_colors7)+
  ggtitle("f) Forest biomass")
country_corridor_biomass_plot

country_corridor_elevation_plot <- ggplot(data=as.data.frame(full_LCP_df7), aes(y=range_m, x=iso_a3, fill=iso_a3)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_blank(),
        plot.title=element_text(size=12),
        legend.position=c('none'))+
  scale_y_continuous(name='Elevational breadth (m)', limits=c())+
  scale_x_discrete(name='Country')+
  scale_fill_manual(values=country_colors7)+
  ggtitle("b) Elevational breadth")
country_corridor_elevation_plot

jpeg('Figures/multipanel_CountryEcologicalConservationStatus.jpeg',width = 6,height = 8,units = 'in',res=600)
grid.arrange(country_corridor_length_plot, country_corridor_elevation_plot, 
             country_corridor_pct_protection_plot, country_corridor_count_protection_plot, 
             country_corridor_count_KBA_plot, country_corridor_biomass_plot, nrow=3)
dev.off()

#### LCP summary table ####
# first get number of end nodes in each country
end_nodes_country <- terra::vect("paper_data/nodes/end_nodes_wPrimaryCountry.shp")

end_nodes_country_summary <- as.data.frame(end_nodes_country) %>%
  dplyr::group_by(iso_a3) %>%
  dplyr::summarize(nEndnodes_inCountry=n()) %>%
  as.data.frame()

# New Belize Maya Forest is in Belize but doesn't have country code filled in
start_nodes_unique$ISO3 <- ifelse(is.na(start_nodes_unique$ISO3)==T, 'BLZ', start_nodes_unique$ISO3)
start_nodes_country_summary <- as.data.frame(start_nodes_unique) %>%
  dplyr::group_by(ISO3) %>%
  dplyr::summarize(nStartnodes_inCountry=n()) %>%
  as.data.frame()


## Major summary table
LCP_summary_table <- full_LCP_df %>%
  dplyr::group_by(iso_a3) %>%
  dplyr::summarize(nLCPs=n(),
                   totalLCP_length_km=sum(LCP_length_km, na.rm=T),
                   minLCP_length_km=min(LCP_length_km, na.rm=T),
                   medianLCP_length_km=median(LCP_length_km, na.rm=T),
                   maxLCP_length_km=max(LCP_length_km, na.rm=T),
                   minLCP_pct_protected=min(all_pa_pct_overlap, na.rm=T),
                   medianLCP_pct_protected=median(all_pa_pct_overlap, na.rm=T),
                   maxLCP_pct_protected=max(all_pa_pct_overlap, na.rm=T),
                   minLCP_nPAs=min(nPAs, na.rm=T),
                   medianLCP_nPAs=median(nPAs, na.rm=T),
                   maxLCP_nPAs=max(nPAs, na.rm=T),
                   minLCP_nKBAs=min(nKBAs, na.rm=T),
                   medianLCP_nKBAs=median(nKBAs, na.rm=T),
                   maxLCP_nKBAs=max(nKBAs, na.rm=T),
                   minLCP_biomass=min(biomass_mean, na.rm=T),
                   medianLCP_biomass=median(biomass_mean, na.rm=T),
                   maxLCP_biomass=max(biomass_mean, na.rm=T),
                   minLCP_elev_range=min(range_m, na.rm=T),
                   medianLCP_elev_range=median(range_m, na.rm=T),
                   maxLCP_elev_range=max(range_m, na.rm=T),
                   min_endnode_elev_range=min(endnode_elevrange_m, na.rm=T),
                   median_endnode_elev_range=median(endnode_elevrange_m, na.rm=T),
                   max_endnode_elev_range=max(endnode_elevrange_m, na.rm=T),
                   min_endnode_area=min(are_km2, na.rm=T),
                   median_endnode_area=median(are_km2, na.rm=T),
                   max_endnode_area=max(are_km2, na.rm=T),
                   nEndnodes=n_distinct(endID),
                   minLCP_nCountries=min(nCountries, na.rm=T),
                   medianLCP_nCountries=median(nCountries, na.rm=T),
                   maxLCP_nCountries=max(nCountries, na.rm=T))

# just 7 focal countries
LCP_summary_table7 <- subset(LCP_summary_table, iso_a3 %in% c('BLZ','CRI','GTM','HND','NIC','PAN','SLV'))
#write.csv(LCP_summary_table,file="paper_data/EcologicalConservationStatus/LCP_summary_table_all_countries.csv", row.names=F)
#write.csv(LCP_summary_table7,file="paper_data/EcologicalConservationStatus/LCP_summary_table_7focal_countries.csv", row.names=F)

cor.test(full_LCP_df7$all_pa_pct_overlap, full_LCP_df7$biomass_mean, method='spearman', alternative='two.sided')
cor.test(full_LCP_df7$all_pa_pct_overlap, full_LCP_df7$biomass_mean, method='pearson', alternative='two.sided')


# fully protected corridors
fully_protected_LCPs <- subset(full_LCP_df, all_pa_pct_overlap >=100)
fully_protected_LCPs %>%
  dplyr::group_by(iso_a3) %>%
  dplyr::summarize(n=n())
