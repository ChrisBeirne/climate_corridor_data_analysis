######################## Compare start and end nodes ##########################
# Date: 8-14-23
# updated: 
# Author: Ian McCullough, immccull@gmail.com
###############################################################################

#### R libraries ####
library(dplyr)
library(terra)
library(ggplot2)
library(gridExtra)

#### Input data ####
setwd("C:/Users/immcc/Documents/climate_corridor_data_analysis")
end_nodes_elev_df <- read.csv("paper_data/EcologicalConservationStatus/end_nodes_elevation_stats.csv")
start_nodes_elev_df <- read.csv("paper_data/EcologicalConservationStatus/start_nodes_elevation_stats.csv")
full_LCP_df <- read.csv("paper_data/EcologicalConservationStatus/full_LCP_data_all_fixed.csv")

end_nodes_protection <- terra::vect("paper_data/nodes/end_nodes_protection.shp")
start_nodes <- terra::vect("paper_data/nodes/start_nodes_wID.shp")

# Biomass
biomass <- terra::rast("C:/Users/immcc/Documents/Osa-Conservation-Connectivity-Project/data/spatial/biomass/NASA_biomass_desnity_estimation.tif")
biomass <- terra::project(biomass, "EPSG:31971",
                         method='average', res=c(300,300))

#### Main program ####
# extract biomass for start and end nodes
start_biomass_mean <- terra::extract(biomass, start_nodes, fun='mean', na.rm=T)
end_biomass_mean <- terra::extract(biomass, end_nodes_protection, fun='mean', na.rm=T)
summary(start_biomass_mean)
summary(end_biomass_mean)

# wrangle end node data for all end nodes, regardless of whether they turned up in LCPs
end_nodes_protection_df <- as.data.frame(end_nodes_protection)
end_nodes_protection_df <- end_nodes_protection_df[,c(3,4,6)]
names(end_nodes_protection_df) <- c('areakm2','endID','pct_protected')

end_nodes_all_df <- merge(end_nodes_protection_df, end_nodes_elev_df[,c(3,7)])
end_nodes_all_df$NodeGroup <- 'End'
end_nodes_all_df$MeanBiomass <- end_biomass_mean[,2]
end_nodes_all_df_wID <- end_nodes_all_df
end_nodes_all_df <- end_nodes_all_df[,c(2:6)]


# wrangle start node data for all start nodes (all included in LCP analysis)
start_df <- start_nodes_elev_df[,c('range_m','calculatedarea_km2')]
names(start_df) <- c('range_m','areakm2')
start_df$pct_protected <- 100
start_df$NodeGroup <-'Start'
start_df <- start_df[,c(2,3,1,4)]
start_df$MeanBiomass <- start_biomass_mean[,2]

startend_all_df <- rbind.data.frame(end_nodes_all_df, start_df)
#startend_all_df$NodeGroup <- as.factor(startend_all_df$NodeGroup)
startend_all_df$NodeGroup <- factor(startend_all_df$NodeGroup, levels=c('Start','End'))

# grouped boxplot
titlefontsize <- 10

allnode_area_plot <- ggplot(startend_all_df, aes(x=NodeGroup, y=areakm2, fill=NodeGroup)) + 
  geom_boxplot()+
  ggtitle('a) Area')+
  theme_classic()+
  theme(axis.text.y=element_text(color='black'),
        axis.text.x=element_text(color='black'),
        legend.position='none',
        plot.title=element_text(size=titlefontsize),
        axis.title.x=element_blank())+
  scale_y_continuous(name='Area (km2)', limits=c(0,100))+
  scale_fill_manual(values=c('darkkhaki','forestgreen'))

allnode_elev_plot <- ggplot(startend_all_df, aes(x=NodeGroup, y=range_m, fill=NodeGroup)) + 
  geom_boxplot()+
  ggtitle('b) Elevation range')+
  theme_classic()+
  theme(axis.text.y=element_text(color='black'),
        axis.text.x=element_text(color='black'),
        plot.title=element_text(size=titlefontsize),
        legend.position='none',
        axis.title.x=element_blank())+
  ylab("Elevation (m)")+
  scale_fill_manual(values=c('darkkhaki','forestgreen'))

allnode_protection_plot <- ggplot(startend_all_df, aes(x=NodeGroup, y=pct_protected, fill=NodeGroup)) + 
  geom_boxplot()+
  ggtitle('c) % Protected')+
  theme_classic()+
  theme(axis.text.y=element_text(color='black'),
        axis.text.x=element_text(color='black'),
        plot.title=element_text(size=titlefontsize),
        legend.position='none',
        axis.title.x=element_blank())+
  ylab("% Protected")+
  scale_fill_manual(values=c('darkkhaki','forestgreen'))

allnode_biomass_plot <- ggplot(startend_all_df, aes(x=NodeGroup, y=MeanBiomass, fill=NodeGroup)) + 
  geom_boxplot()+
  ggtitle('d) Forest biomass')+
  theme_classic()+
  theme(axis.text.y=element_text(color='black'),
        axis.text.x=element_text(color='black'),
        plot.title=element_text(size=titlefontsize),
        legend.position='none',
        axis.title.x=element_blank())+
  ylab("Mean biomass (Mt C)")+
  scale_fill_manual(values=c('darkkhaki','forestgreen'))

jpeg('Figures/start_vs_endnodes_allnodes.jpeg',width = 7,height = 5,units = 'in',res=600)
  grid.arrange(allnode_area_plot, allnode_elev_plot, allnode_protection_plot, allnode_biomass_plot,nrow=1)
dev.off()

### repeat analysis only with end nodes that ended up in LCP analysis
endnodes_LCP <- unique(full_LCP_df$endID) 
end_nodes_LCP_df <- subset(end_nodes_all_df_wID, endID %in% endnodes_LCP)[,c(2:6)]

startend_LCP_df <- rbind.data.frame(end_nodes_LCP_df, start_df)
startend_LCP_df$NodeGroup <- factor(startend_LCP_df$NodeGroup, levels=c('Start','End'))

# grouped boxplot
titlefontsize <- 10

LCPnode_area_plot <- ggplot(startend_LCP_df, aes(x=NodeGroup, y=areakm2, fill=NodeGroup)) + 
  geom_boxplot()+
  ggtitle('a) Area')+
  theme_classic()+
  theme(axis.text.y=element_text(color='black'),
        axis.text.x=element_text(color='black'),
        legend.position='none',
        plot.title=element_text(size=titlefontsize),
        axis.title.x=element_blank())+
  scale_y_continuous(name='Area (km2)', limits=c(0,100))+
  scale_fill_manual(values=c('darkkhaki','forestgreen'))

LCPnode_elev_plot <- ggplot(startend_LCP_df, aes(x=NodeGroup, y=range_m, fill=NodeGroup)) + 
  geom_boxplot()+
  ggtitle('b) Elevation range')+
  theme_classic()+
  theme(axis.text.y=element_text(color='black'),
        axis.text.x=element_text(color='black'),
        plot.title=element_text(size=titlefontsize),
        legend.position='none',
        axis.title.x=element_blank())+
  ylab("Elevation (m)")+
  scale_fill_manual(values=c('darkkhaki','forestgreen'))

LCPnode_protection_plot <- ggplot(startend_LCP_df, aes(x=NodeGroup, y=pct_protected, fill=NodeGroup)) + 
  geom_boxplot()+
  ggtitle('c) % Protected')+
  theme_classic()+
  theme(axis.text.y=element_text(color='black'),
        axis.text.x=element_text(color='black'),
        plot.title=element_text(size=titlefontsize),
        legend.position='none',
        axis.title.x=element_blank())+
  ylab("% Protected")+
  scale_fill_manual(values=c('darkkhaki','forestgreen'))

LCPnode_biomass_plot <- ggplot(startend_LCP_df, aes(x=NodeGroup, y=MeanBiomass, fill=NodeGroup)) + 
  geom_boxplot()+
  ggtitle('d) Forest biomass')+
  theme_classic()+
  theme(axis.text.y=element_text(color='black'),
        axis.text.x=element_text(color='black'),
        plot.title=element_text(size=titlefontsize),
        legend.position='none',
        axis.title.x=element_blank())+
  ylab("Mean biomass (Mt C)")+
  scale_fill_manual(values=c('darkkhaki','forestgreen'))

jpeg('Figures/start_vs_endnodes_LCPnodes.jpeg',width = 7,height = 5,units = 'in',res=600)
  grid.arrange(LCPnode_area_plot, LCPnode_elev_plot, LCPnode_protection_plot, LCPnode_biomass_plot, nrow=1)
dev.off()


summary(subset(startend_all_df, NodeGroup=='Start'))
summary(subset(startend_all_df, NodeGroup=='End'))
summary(subset(startend_LCP_df, NodeGroup=='Start'))
summary(subset(startend_LCP_df, NodeGroup=='End'))
