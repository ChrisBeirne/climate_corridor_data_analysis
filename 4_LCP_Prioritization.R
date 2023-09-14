##################### Climate corridor prioritization #########################
# Date: 8-8-23
# updated: 9-14-23; log transform some variables in boxplots
# Author: Ian McCullough, immccull@gmail.com
###############################################################################

#### R libraries ####
library(terra)
library(scales)
library(factoextra)
library(plotly)
library(scatterplot3d)
library(colorspace)
library(gplots)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(rstatix)

#### Input data ####
setwd("C:/Users/immcc/Documents/climate_corridor_data_analysis")
# table for only corridors located primarily in focal 7 countries
#full_LCP_df7 <- read.csv("paper_data/EcologicalConservationStatus/full_LCP_data_7countries.csv")
# all countries
full_LCP_df <- read.csv("paper_data/EcologicalConservationStatus/full_LCP_data_all_fixed.csv")

#### Main program ####
# first check correlations and distributions for select variables
pca_variables <- c('iso_a3','crrdr_d','LCP_length_km','all_pa_pct_overlap','nPAs','nKBAs', 
                   'biomass_mean','range_m','are_km2','endnode_elevrange_m', 'endnode_pct_protected',
                   'StartNode_areakm2','StartNode_elevrange_m') 
pca_variable_df <- full_LCP_df[,pca_variables]

# rescale corridor length such that higher number is good
pca_variable_df$LCP_length_km_inv <- scales::rescale(pca_variable_df$LCP_length_km, to=c(1,0.01))

hist(pca_variable_df$LCP_length_km)
hist(pca_variable_df$all_pa_pct_overlap)
hist(pca_variable_df$nPAs)
hist(pca_variable_df$nKBAs)
hist(pca_variable_df$biomass_mean)
hist(pca_variable_df$range_m)
hist(pca_variable_df$are_km2)
hist(pca_variable_df$endnode_elevrange_m)
hist(pca_variable_df$endnode_pct_protected)
hist(pca_variable_df$LCP_length_km_inv)
hist(pca_variable_df$StartNode_areakm2)
hist(pca_variable_df$StartNode_elevrange_m)

cor(pca_variable_df[,c(3:13)], method='spearman')

# Based on the correlation matrix:
# can remove nPAs because highly correlated with LCP length (Spearman's rho=0.76) and nKBAs (rho=0.74)
# can remove end node area (are_km2) because highly correlated with endnode_elevrange_m (rho=0.76)

## rescale all variables prior to PCA
pca_data <- as.data.frame(scale(pca_variable_df[,c(3:ncol(pca_variable_df))]))
rownames(pca_data) <- pca_variable_df$crrdr_d
#cor(pca_data, method='spearman')# just curious

## run PCA
pca_LCP <- princomp(~ all_pa_pct_overlap + nKBAs + biomass_mean + range_m + 
                      endnode_elevrange_m + endnode_pct_protected + LCP_length_km_inv + 
                      StartNode_areakm2 + StartNode_elevrange_m,
                      data=pca_data, cor=F, scores=T)
par(mfrow=c(1,1))
screeplot(pca_LCP, type='l')
summary(pca_LCP)
loadings(pca_LCP)

fviz_pca_var(pca_LCP,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(pca_LCP, addEllipses = T, label='var', 
                habillage=pca_variable_df$iso_a3)

fviz_pca_ind(pca_LCP, addEllipses = F, label='noe', axes=c(1,2),
             habillage=pca_variable_df$iso_a3)


## 3D plots
pca_scores <- as.data.frame(pca_LCP$scores)

plot_ly(pca_scores$Comp.1, y=pca_scores$Comp.2, z=pca_scores$Comp.3, 
        type="scatter3d", mode="markers", size=1, color=pca_variable_df$iso_a3)


unique_colors <- c("blue","gold","forestgreen","orange","firebrick","purple","turquoise","khaki","chartreuse")
colors_hex <- col2hex(unique_colors)
colors_hex <- colors_hex[as.numeric(as.factor(pca_variable_df$iso_a3))]
scatt <- scatterplot3d(pca_scores[,1:3], pch = 16, color=colors_hex, grid=T, box=T)
legend(scatt$xyz.convert(7,2,9), legend = levels(as.factor(pca_variable_df$iso_a3)),
       col = unique_colors, pch = 16, horiz=F)

## calculate LCP prioritization index
pca_scores$priority_index <- sqrt((pca_scores$Comp.1 ^2) + (pca_scores$Comp.2 ^2) +
                                     (pca_scores$Comp.3 ^2))
summary(pca_scores$priority_index)
hist(pca_scores$priority_index)
pca_scores$Country <- pca_variable_df$iso_a3

# set thresholds for corridor priority index
low_cutoff <- 2
high_cutoff <- 4
pca_scores$priority_index_level <- ifelse(pca_scores$priority_index < low_cutoff, 'Low', NA)
pca_scores$priority_index_level <- ifelse(pca_scores$priority_index < high_cutoff & pca_scores$priority_index > low_cutoff, 'Medium', pca_scores$priority_index_level)
pca_scores$priority_index_level <- ifelse(pca_scores$priority_index > high_cutoff, 'High', pca_scores$priority_index_level)
pca_scores$crrdr_d <- pca_variable_df$crrdr_d

pca_scores %>% dplyr::group_by(priority_index_level) %>% tally()

pca_scores$priority_index_rank <- as.integer(rank(desc(pca_scores$priority_index)))

#write.csv(pca_scores, file='paper_data/LCP_prioritization/LCP_priority_index.csv', row.names=F)

#jpeg('Figures/CorridorPriorityIndex_histogram.jpeg',width = 7,height = 5,units = 'in',res=600)
par(mar = c(4, 4, 4, 2))
hist(pca_scores$priority_index, main='Corridor priority index',
       xlab='Priority', las=1, breaks=seq(0,8,0.5),#, cex.main=2, cex.lab=1.5, cex.axis=1.5,
       col=c('gray','gray','gray','gray',
             'orangered','orangered','orangered','orangered',
             'gold','gold','gold','gold','gold','gold'))
abline(v=2, lty='dashed', lwd=2)
abline(v=4, lty='dashed', lwd=2)
text(0.6, 450, "Low", cex=1.25)
text(3, 450, "Medium", cex=1.25)
text(5, 450, "High", cex=1.25)
#dev.off()


priority_index_plot <- ggplot(data=pca_scores, aes(y=priority_index, x=Country, fill=Country)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_blank(),
        legend.position=c('none'))+
  scale_y_continuous(name='Priority index', limits=c())+
  scale_x_discrete(name='Country')+
  #scale_fill_manual(values=country_colors7)+
  scale_fill_manual(values=unique_colors)+
  ggtitle("Priority index")
priority_index_plot


### Merging LCPs with priority index for mapping in QGIS
LCPs <- terra::vect("paper_data/corridors/all_corridors_noDupes.shp")
LCPs <- terra::project(LCPs, "EPSG:31971")

LCPs_priority_index <- terra::merge(LCPs, pca_scores, by='crrdr_d')
#terra::writeVector(LCPs_priority_index, "paper_data/corridors/LCP_priority_index.shp", overwrite=T)

#### Comparing characteristics of high, medium and low priority corridors ####
# perhaps a bit circular, but helps interpretation

full_LCP_PCA_df <- merge(full_LCP_df, pca_scores[,c(12:14)], by='crrdr_d')
full_LCP_PCA_df$priority_index_level <- factor(full_LCP_PCA_df$priority_index_level, levels=c('High','Medium','Low'))

boxplot(LCP_length_km ~ priority_index_level, data=full_LCP_PCA_df, las=1,
        col=c('gold','orange','gray'), xlab='', main='Corridor length', ylab='km')
boxplot(all_pa_pct_overlap ~ priority_index_level, data=full_LCP_PCA_df, las=1,
        col=c('gold','orange','gray'), xlab='', main='Corridor % protected', ylab='%')
boxplot(nPAs ~ priority_index_level, data=full_LCP_PCA_df, las=1,
        col=c('gold','orange','gray'), xlab='', main='Overlapping protected areas', ylab='Number')
boxplot(nKBAs ~ priority_index_level, data=full_LCP_PCA_df, las=1,
        col=c('gold','orange','gray'), xlab='', main='Overlapping KBAs', ylab='Number')
boxplot(biomass_mean ~ priority_index_level, data=full_LCP_PCA_df, las=1,
        col=c('gold','orange','gray'), xlab='', main='Corridor mean forest biomass', ylab='Mt C')
boxplot(range_m ~ priority_index_level, data=full_LCP_PCA_df, las=1,
        col=c('gold','orange','gray'), xlab='', main='Corridor elevational breadth', ylab='m')
boxplot(nCountries ~ priority_index_level, data=full_LCP_PCA_df, las=1,
        col=c('gold','orange','gray'), xlab='', main='Ocountries crossed', ylab='Number')
boxplot(are_km2 ~ priority_index_level, data=full_LCP_PCA_df, las=1,
        col=c('gold','orange','gray'), xlab='', main='End node area', ylab='km2')#, ylim=c(0,1000))
boxplot(endnode_elevrange_m ~ priority_index_level, data=full_LCP_PCA_df, las=1,
        col=c('gold','orange','gray'), xlab='', main='End node elevational breadth', ylab='m')
boxplot(endnode_pct_protected ~ priority_index_level, data=full_LCP_PCA_df, las=1,
        col=c('gold','orange','gray'), xlab='', main='End node % protected', ylab='%')
boxplot(StartNode_areakm2 ~ priority_index_level, data=full_LCP_PCA_df, las=1,
        col=c('gold','orange','gray'), xlab='', main='Start node area', ylab='km2')
boxplot(StartNode_elevrange_m ~ priority_index_level, data=full_LCP_PCA_df, las=1,
        col=c('gold','orange','gray'), xlab='', main='Start node elevational breadth', ylab='m')
# ones that appear more interesting
# end node % protected
# end node area (if cut axes)
# LCP elev range
# LCP % protected
# nKBAs
# nPAs

## some plotting parameters for all plots
titlefontsize <- 10


LCP_length_plot <- ggplot(full_LCP_PCA_df, aes(priority_index_level, y=LCP_length_km, fill=priority_index_level)) + 
  geom_boxplot()+
  ggtitle('a) Corridor length**')+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        plot.title=element_text(size=titlefontsize),
        axis.title.x=element_blank(),
        legend.position='none')+
  #xlab('Priority index level')+
  ylab('Distance (km)')+
  scale_fill_manual(values=c('gold','orange','gray'))
LCP_length_plot

LCP_elev_plot <- ggplot(full_LCP_PCA_df, aes(priority_index_level, y=range_m, fill=priority_index_level)) + 
  geom_boxplot()+
  ggtitle('b) Corridor elev range*')+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        plot.title=element_text(size=titlefontsize),
        axis.title.x=element_blank(),
        legend.position='none')+
  #xlab('Priority index level')+
  ylab('Elevation (m)')+
  scale_fill_manual(values=c('gold','orange','gray'))
LCP_elev_plot

LCP_biomass_plot <- ggplot(full_LCP_PCA_df, aes(priority_index_level, y=biomass_mean, fill=priority_index_level)) + 
  geom_boxplot()+
  ggtitle('c) Corridor biomass*')+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        plot.title=element_text(size=titlefontsize),
        axis.title.x=element_blank(),
        legend.position='none')+
  #xlab('Priority index level')+
  #ylab('Area (km2)')+
  scale_y_continuous(name='Mean biomass (Mt C)', limits=c())+
  scale_fill_manual(values=c('gold','orange','gray'))
LCP_biomass_plot

LCP_pct_protected_plot <- ggplot(full_LCP_PCA_df, aes(priority_index_level, y=all_pa_pct_overlap, fill=priority_index_level)) + 
  geom_boxplot()+
  ggtitle('d) Corridor protected*')+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        plot.title=element_text(size=titlefontsize),
        axis.title.x=element_blank(),
        legend.position='none')+
  #xlab('Priority index level')+
  ylab('% Protected')+
  scale_fill_manual(values=c('gold','orange','gray'))
LCP_pct_protected_plot

LCP_nPAs_plot <- ggplot(full_LCP_PCA_df, aes(priority_index_level, y=nPAs, fill=priority_index_level)) + 
  geom_boxplot()+
  ggtitle('e) Corridor PAs**')+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        plot.title=element_text(size=titlefontsize),
        axis.title.x=element_blank(),
        legend.position='none')+
  #xlab('Priority index level')+
  ylab('Number of PAs')+
  scale_fill_manual(values=c('gold','orange','gray'))
LCP_nPAs_plot

LCP_KBA_plot <- ggplot(full_LCP_PCA_df, aes(priority_index_level, y=nKBAs, fill=priority_index_level)) + 
  geom_boxplot()+
  ggtitle('f) Corridor KBAs**')+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        plot.title=element_text(size=titlefontsize),
        axis.title.x=element_blank(),
        legend.position='none')+
  #xlab('Priority index level')+
  ylab('Number of KBAs')+
  scale_fill_manual(values=c('gold','orange','gray'))
LCP_KBA_plot

endnode_elev_plot <- ggplot(full_LCP_PCA_df, aes(priority_index_level, y=endnode_elevrange_m, fill=priority_index_level)) + 
  geom_boxplot()+
  ggtitle('g) End elev range**')+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        plot.title=element_text(size=titlefontsize),
        axis.title.x=element_blank(),
        legend.position='none')+
  #xlab('Priority index level')+
  ylab('Elevation (m)')+
  scale_fill_manual(values=c('gold','orange','gray'))
endnode_elev_plot

endnode_area_plot <- ggplot(full_LCP_PCA_df, aes(priority_index_level, y=log(are_km2), fill=priority_index_level)) + 
  geom_boxplot()+
  ggtitle('h) End area**')+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        plot.title=element_text(size=titlefontsize),
        axis.title.x=element_blank(),
        legend.position='none')+
  #xlab('Priority index level')+
  #ylab('log(Area (sq km)'))+
  scale_y_continuous(name='log(Area (sq km))', limits=c())+
  scale_fill_manual(values=c('gold','orange','gray'))
endnode_area_plot

endnode_protection_plot <- ggplot(full_LCP_PCA_df, aes(priority_index_level, y=endnode_pct_protected, fill=priority_index_level)) + 
  geom_boxplot()+
  ggtitle('i) End protected**')+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        plot.title=element_text(size=titlefontsize),
        axis.title.x=element_blank(),
        legend.position='none')+
  #xlab('Priority index level')+
  #ylab('Area (km2)')+
  scale_y_continuous(name='% Protected', limits=c())+
  scale_fill_manual(values=c('gold','orange','gray'))
endnode_protection_plot

startnode_elev_plot <- ggplot(full_LCP_PCA_df, aes(priority_index_level, y=log(StartNode_elevrange_m), fill=priority_index_level)) + 
  geom_boxplot()+
  ggtitle('j) Start elev range^')+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        plot.title=element_text(size=titlefontsize),
        axis.title.x=element_blank(),
        legend.position='none')+
  #xlab('Priority index level')+
  ylab('log(Elevation (m))')+
  scale_fill_manual(values=c('gold','orange','gray'))
startnode_elev_plot

startnode_area_plot <- ggplot(full_LCP_PCA_df, aes(priority_index_level, y=log(StartNode_areakm2), fill=priority_index_level)) + 
  geom_boxplot()+
  ggtitle('k) Start area*')+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_blank(),
        plot.title=element_text(size=titlefontsize),
        legend.position='none')+
  #xlab('Priority index level')+
  #ylab('Area (km2)')+
  scale_y_continuous(name='log(Area (sq km))', limits=c())+
  scale_fill_manual(values=c('gold','orange','gray'))
startnode_area_plot

# put together into multipanel
jpeg('Figures/multipanel_LCP_highmedlow_boxplots.jpeg',width = 6,height = 8,units = 'in',res=600)
grid.arrange(LCP_length_plot, LCP_elev_plot, LCP_biomass_plot,
             LCP_pct_protected_plot, LCP_nPAs_plot,  LCP_KBA_plot,
             endnode_elev_plot, endnode_area_plot, endnode_protection_plot,
             startnode_elev_plot, startnode_area_plot, nrow=4)
dev.off()

## summarize by priority group
high_med_low_summary <- full_LCP_PCA_df %>%
  dplyr::group_by(priority_index_level) %>%
  dplyr::summarize(min_LCPlength_km = min(LCP_length_km, na.rm=T),
                   median_LCPlength_km=median(LCP_length_km, na.rm=T),
                   max_LCPlength_km=max(LCP_length_km, na.rm=T),
                   
                   min_LCPelevrange_m = min(range_m, na.rm=T),
                   median_LCPelevrange_m=median(range_m, na.rm=T),
                   max_LCPelevrange_m=max(range_m, na.rm=T),
                   
                   min_LCPbiomass = min(biomass_mean, na.rm=T),
                   median_LCPbiomass=median(biomass_mean, na.rm=T),
                   max_LCPbiomass=max(biomass_mean, na.rm=T),
                   
                   min_LCPprotected_pct = min(all_pa_pct_overlap, na.rm=T),
                   median_LCPprotected_pct=median(all_pa_pct_overlap, na.rm=T),
                   max_LCPprotected_pct=max(all_pa_pct_overlap, na.rm=T),
                   
                   min_LCPnPAS = min(nPAs, na.rm=T),
                   median_LCPnPAS=median(nPAs, na.rm=T),
                   max_LCPnPAS=max(nPAs, na.rm=T),
                   
                   min_LCPnKBAS = min(nKBAs, na.rm=T),
                   median_LCPnKBAS=median(nKBAs, na.rm=T),
                   max_LCPnKBAS=max(nKBAs, na.rm=T),
                   
                   min_endnode_elevrange_m = min(endnode_elevrange_m, na.rm=T),
                   median_endnode_elevrange_m=median(endnode_elevrange_m, na.rm=T),
                   max_endnode_elevrange_m=max(endnode_elevrange_m, na.rm=T),
                   
                   min_endnode_areasqkm = min(are_km2, na.rm=T),
                   median_endnode_areasqkm=median(are_km2, na.rm=T),
                   max_endnode_areasqkm=max(are_km2, na.rm=T),
                   
                   min_endnode_protected_pct = min(endnode_pct_protected, na.rm=T),
                   median_endnode_protected_pct=median(endnode_pct_protected, na.rm=T),
                   max_endnode_protected_pct=max(endnode_pct_protected, na.rm=T),
                   
                   min_startnode_elevrange_m = min(StartNode_elevrange_m, na.rm=T),
                   median_startnode_elevrange_m=median(StartNode_elevrange_m, na.rm=T),
                   max_startnode_elevrange_m=max(StartNode_elevrange_m, na.rm=T),
                   
                   min_startnode_areasqkm = min(StartNode_areakm2, na.rm=T),
                   median_startnode_areasqkm=median(StartNode_areakm2, na.rm=T),
                   max_startnode_areasqkm=max(StartNode_areakm2, na.rm=T),
                   nGroup=n())

#write.csv(high_med_low_summary, file='paper_data/EcologicalConservationStatus/high_med_low_priority_corridors.csv', row.names=F)

## Check for significant differences across groups
# use non-parametric approach due to some non-normal distributions

# LCP variables
kruskal.test(full_LCP_PCA_df$LCP_length_km~ full_LCP_PCA_df$priority_index_level)
full_LCP_PCA_df %>% 
  dunn_test(LCP_length_km ~ priority_index_level, p.adjust.method = "holm") 

kruskal.test(full_LCP_PCA_df$range_m~ full_LCP_PCA_df$priority_index_level)
full_LCP_PCA_df %>% 
  dunn_test(range_m ~ priority_index_level, p.adjust.method = "holm") 

kruskal.test(full_LCP_PCA_df$biomass_mean~ full_LCP_PCA_df$priority_index_level)
full_LCP_PCA_df %>% 
  dunn_test(biomass_mean ~ priority_index_level, p.adjust.method = "holm") 

kruskal.test(full_LCP_PCA_df$all_pa_pct_overlap~ full_LCP_PCA_df$priority_index_level)
full_LCP_PCA_df %>% 
  dunn_test(all_pa_pct_overlap ~ priority_index_level, p.adjust.method = "holm") 

kruskal.test(full_LCP_PCA_df$nPAs~ full_LCP_PCA_df$priority_index_level)
full_LCP_PCA_df %>% 
  dunn_test(nPAs ~ priority_index_level, p.adjust.method = "holm") 

kruskal.test(full_LCP_PCA_df$nKBAs~ full_LCP_PCA_df$priority_index_level)
full_LCP_PCA_df %>% 
  dunn_test(nKBAs ~ priority_index_level, p.adjust.method = "holm") 

# end node variables
kruskal.test(full_LCP_PCA_df$endnode_elevrange_m~ full_LCP_PCA_df$priority_index_level)
full_LCP_PCA_df %>% 
  dunn_test(endnode_elevrange_m ~ priority_index_level, p.adjust.method = "holm") 

kruskal.test(log(full_LCP_PCA_df$endnode_elevrange_m)~ full_LCP_PCA_df$priority_index_level)

full_LCP_PCA_df$log_endnode_elevrange_m <- log(full_LCP_PCA_df$endnode_elevrange_m)
full_LCP_PCA_df %>% 
  dunn_test(log_endnode_elevrange_m ~ priority_index_level, p.adjust.method = "holm") 

kruskal.test(full_LCP_PCA_df$are_km2~ full_LCP_PCA_df$priority_index_level)
full_LCP_PCA_df %>% 
  dunn_test(are_km2 ~ priority_index_level, p.adjust.method = "holm") 

kruskal.test(full_LCP_PCA_df$endnode_pct_protected~ full_LCP_PCA_df$priority_index_level)
full_LCP_PCA_df %>% 
  dunn_test(endnode_pct_protected ~ priority_index_level, p.adjust.method = "holm") 

# start node variables
# This KW test did not show significant effect of priority level
kruskal.test(full_LCP_PCA_df$StartNode_elevrange_m~ full_LCP_PCA_df$priority_index_level)
# full_LCP_PCA_df %>% 
#   dunn_test(StartNode_elevrange_m ~ priority_index_level, p.adjust.method = "holm") 


kruskal.test(full_LCP_PCA_df$StartNode_areakm2~ full_LCP_PCA_df$priority_index_level)
full_LCP_PCA_df %>% 
  dunn_test(StartNode_areakm2 ~ priority_index_level, p.adjust.method = "holm") 

