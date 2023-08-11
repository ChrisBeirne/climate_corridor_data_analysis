####################### Formatting summary tables #############################
# Date: 8-9-23
# updated: 8-10-23; all countries
# Author: Ian McCullough, immccull@gmail.com
###############################################################################

#### R libraries ####
library(dplyr)
library(terra)

#### Input data ####
setwd("C:/Users/immcc/Documents/climate_corridor_data_analysis")
# table for only corridors located primarily in focal 7 countries
#full_LCP_df7 <- read.csv("paper_data/EcologicalConservationStatus/full_LCP_data_7countries.csv")
#LCP_summary7 <- read.csv("paper_data/EcologicalConservationStatus/LCP_summary_table_7focal_countries.csv")
LCP_summary_all <- read.csv("paper_data/EcologicalConservationStatus/LCP_summary_table_all_countries.csv")

start_nodes_elev_df <- read.csv("paper_data/EcologicalConservationStatus/start_nodes_elevation_stats.csv")

endnode_country <- terra::vect("paper_data/nodes/end_nodes_wPrimaryCountry.shp")

#### Main program ####
LCP_summary_all$LCP_length_km <- paste(round(LCP_summary_all$minLCP_length_km, 0), round(LCP_summary_all$medianLCP_length_km, 0), round(LCP_summary_all$maxLCP_length_km, 0), sep=', ') 
LCP_summary_all$pct_protected <- paste(round(LCP_summary_all$minLCP_pct_protected, 0), round(LCP_summary_all$medianLCP_pct_protected, 0), round(LCP_summary_all$maxLCP_pct_protected, 0), sep=', ') 
LCP_summary_all$nPAs <- paste(round(LCP_summary_all$minLCP_nPAs, 0), round(LCP_summary_all$medianLCP_nPAs, 0), round(LCP_summary_all$maxLCP_nPAs, 0), sep=', ') 
LCP_summary_all$nKBAs <- paste(round(LCP_summary_all$minLCP_nKBAs, 0), round(LCP_summary_all$medianLCP_nKBAs, 0), round(LCP_summary_all$maxLCP_nKBAs, 0), sep=', ')
LCP_summary_all$biomass <- paste(round(LCP_summary_all$minLCP_biomass, 0), round(LCP_summary_all$medianLCP_biomass, 0), round(LCP_summary_all$maxLCP_biomass, 0), sep=', ')
LCP_summary_all$LCP_elevrange_m <- paste(round(LCP_summary_all$minLCP_elev_range, 0), round(LCP_summary_all$medianLCP_elev_range, 0), round(LCP_summary_all$maxLCP_elev_range, 0), sep=', ')
LCP_summary_all$nCountries <- paste(round(LCP_summary_all$minLCP_nCountries, 0), round(LCP_summary_all$medianLCP_nCountries, 0), round(LCP_summary_all$maxLCP_nCountries, 0), sep=', ')

neat_summary <- LCP_summary_all[,c("iso_a3","nLCPs","LCP_length_km","LCP_elevrange_m","pct_protected","nPAs","nKBAs","nCountries","biomass")]
names(neat_summary) <- c('Country','Potential corridors','Length (km)','Elevation range (m)','Protection (%)','Overlapping protected areas','Overlapping KBAs','Countries crossed',"Forest biomass")
#write.csv(neat_summary, file='Tables/LCP_country_clean_summary.csv', row.names=F)

#### Node summary by country ####

node_summary <- data.frame(Country=LCP_summary_all$iso_a3, DestinationEndNodes=LCP_summary_all$nEndnodes)

# remember, one NA is new Belize Maya protected area
start_nodes_elev_df$ISO3 <- ifelse(is.na(start_nodes_elev_df$ISO3)==T, 'BLZ', start_nodes_elev_df$ISO3) 
start_node_summary <- start_nodes_elev_df %>%
  dplyr::group_by(ISO3) %>%
  dplyr::summarize(nStartNodes=n()) %>%
  as.data.frame()

endnode_country_df <- as.data.frame(endnode_country)

endnode_summary <- endnode_country_df %>%
  dplyr::group_by(iso_a3) %>%
  dplyr::summarize(nEndNodes_housed=n()) %>%
  as.data.frame()

node_summary <- merge(node_summary, start_node_summary, by.x='Country', by.y='ISO3', all=T)
node_summary <- merge(node_summary, endnode_summary, by.x='Country', by.y='iso_a3', all=T)  

node_summary <- node_summary[,c(1,3,2,4)]
#write.csv(node_summary, file='Tables/startend_node_country_summary.csv', row.names=F)
