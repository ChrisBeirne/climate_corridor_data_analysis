# Final analysis for climate corridor paper

# Create file folders
dir.create("data")
dir.create("data/input")
dir.create("data/output")

# Load packages

list.of.packages <- c("stars",       
                      "sf",
                      "dplyr",            
                      "leaflet",
                      "googledrive",
                      "purrr",
                      "remotes",
                      "reticulate",
                      "MetBrewer",
                      "terra",
                      "raster",
                      "gdistance",
                      "nngeo",
                      "stringr")

# Check which ones you dont have
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#Install the ones you dont have
if(length(new.packages)) install.packages(new.packages)
# Load the packages
lapply(list.of.packages, require, character.only = TRUE)

# Auth google drive
drive_auth()

2
# Import data files

# Note - Derived files are created in the connectivity modelling script: 
# https://github.com/ChrisBeirne/Osa-Conservation-Connectivity-Project

# Specify aoi
aoi <-  ext(-84, -82.5,8.38,9.38)

#################################
# Aoi folder
gd_folder = "https://drive.google.com/drive/folders/1uLmpJVwMsv_OX5Tpd0eh3dfwJSz2aV1g"
folder_id = drive_get(as_id(gd_folder))
files = drive_ls(folder_id)

    ## base aoi
    tmp <- files[substr(files$name,1,8)=="base_aoi",]
    # Download - note #fails if already exists
       for (file_i in seq_along(tmp$name)) {
          try({
           drive_download(
             as_id(tmp$id[file_i]),
             path = paste0("data/input/", tmp$name[file_i])
              )
           })
         }
    
    base_shp <- st_read("data/input/base_aoi.shp", quiet = TRUE)
    
    ## Focal countries
    tmp <- files[substr(files$name,1,15)=="focal_countries",]
    # Download - note #fails if already exists
    for (file_i in seq_along(tmp$name)) {
      try({
        drive_download(
          as_id(tmp$id[file_i]),
          path = paste0("data/input/", tmp$name[file_i])
        )
      })
    }
    
    focal_shp <- st_read("data/input/focal_countries.shp", quiet = TRUE)
    # plot(focal_shp)
    
    
    ## High elevation contiguous habitat
    tmp <- files[substr(files$name,1,22)=="high_elevation_forests",]
    # Download - note #fails if already exists
    for (file_i in seq_along(tmp$name)) {
      try({
        drive_download(
          as_id(tmp$id[file_i]),
          path = paste0("data/input/", tmp$name[file_i])
        )
      })
    }
    
    end_nodes <- st_read("data/input/high_elevation_forests_5km2_17N.shp", quiet = TRUE)

    
    ## Waterbody masl
    ## High elevation contiguous habitat
    tmp <- files[files$name=="waterbody_mask.tif",]
    # Download - note #fails if already exists
    for (file_i in seq_along(tmp$name)) {
      try({
        drive_download(
          as_id(tmp$id[file_i]),
          path = paste0("data/input/", tmp$name[file_i])
        )
      })
    }
    
    mask <- raster("data/input/waterbody_mask.tif")
    
    # Contiguous start PA's
    tmp <- files[substr(files$name,1,28)=="low_elevation_pas_contiguous",]
    # Download - note #fails if already exists
    for (file_i in seq_along(tmp$name)) {
      try({
        drive_download(
          as_id(tmp$id[file_i]),
          path = paste0("data/input/", tmp$name[file_i])
        )
      })
    }
    
    start_pas_cont <- st_read("data/input/low_elevation_pas_contiguous.shp")

########################################
    # Individual protected areas
    
    gd_folder = "https://drive.google.com/drive/u/1/folders/1KgNuMrKPyskUmroOBGISt3FxAsaBLmRv"
    folder_id = drive_get(as_id(gd_folder))
    files = drive_ls(folder_id)
    
    ## base aoi
    tmp <- files[substr(files$name,1,23)=="lowland_protected_areas",]
    # Download - note #fails if already exists
    for (file_i in seq_along(tmp$name)) {
      try({
        drive_download(
          as_id(tmp$id[file_i]),
          path = paste0("data/input/", tmp$name[file_i])
        )
      })
    }
    
    start_pas <- st_read("data/input/lowland_protected_areas.shp", quiet = TRUE)
    
    start_pas$LOW_AREA_KM <- as.numeric(st_area(start_pas)/(1000*1000))
    start_pas <- start_pas[start_pas$LOW_AREA_KM>=5,]
    
    # Clean the names

    start_pas$NAME <- iconv(str_replace_all(start_pas$NAME, "[^[:alnum:]]", " "), from = 'UTF-8', to = 'ASCII//TRANSLIT')
    
    
    
    
    
########################################

  ## land cover layer (100m)
      # landcover folder
      gd_folder = "https://drive.google.com/drive/u/1/folders/11asmjqIxnbno-EwW9Qe8CqzvZk1SofmX"
      folder_id = drive_get(as_id(gd_folder))
      files = drive_ls(folder_id)
      
      ## landcover
      tmp <- files[substr(files$name,1,24)=="ESACCI-global-100m_merge",]
      # Download - note #fails if already exists
      for (file_i in seq_along(tmp$name)) {
        try({
          drive_download(
            as_id(tmp$id[file_i]),
            path = paste0("data/input/", tmp$name[file_i])
          )
        })
      }
      
      # Labels
      tmp <- files[files$name=="ESA_v2_labels.csv",]
      # Download - note #fails if already exists
      for (file_i in seq_along(tmp$name)) {
        try({
          drive_download(
            as_id(tmp$id[file_i]),
            path = paste0("data/input/", tmp$name[file_i])
          )
        })
      }
      
      lc_100m <- rast("data/input/ESACCI-global-100m_merged.tif")
      lc_key <- read.csv("data/input/ESA_v2_labels.csv", header=T, sep=",")

###########################################

## Biomass layer

    # biomass folder
    gd_folder = "https://drive.google.com/drive/u/1/folders/1LB-gpbCbb0c8E8xpCc_gEOd6CmL69Sd3"
    folder_id = drive_get(as_id(gd_folder))
    files = drive_ls(folder_id)
    
    ## base aoi
    tmp <- files[files$name=="NASA_biomass_desnity_estimation.tif",]
    # Download - note #fails if already exists
    for (file_i in seq_along(tmp$name)) {
      try({
        drive_download(
          as_id(tmp$id[file_i]),
          path = paste0("data/input/", "NASA_biomass_desnity_estimation.tif")
        )
      })
    }
    
    biomass <- rast("data/input/NASA_biomass_desnity_estimation.tif")
    



    
    
        
########################################################################
########################################################################
########################################################################
# Create the conductnace layer

    # Format the files
    base_terra <- vect(base_shp)
    lc_key$rgb <- paste0("#",lc_key$rgb ) 
    lc_100m <- mask(lc_100m, base_terra)
    
    # Set up reclassify table
    m2     <- c(0, NA,
                10, 1000,
                20, 150,
                30, 30,
                40, 30,
                50, NA,
                60, 40,
                70, NA,
                80, 30,
                90, 20,
                95, 500,
                100,2
    )
    m2 <- matrix(m2, ncol=2, byrow = TRUE)
    
    # Reclassify the full thing?
    lc_re <- classify(lc_100m, m2, right=TRUE)
    # Convert to raster and take a look
    lc_re_ras <- raster(lc_re)
    plot(lc_re_ras)
    
    # Create the biomass modifier
    biomass <- clamp(biomass, upper=100)
    #plot(biomass)
    biomass_modifier <- biomass/100
    plot(biomass_modifier)
    
    #We then mask out non-forest areas, and apply the modifier, which creates a surface which looks like:
    just_forest <- ifel(lc_100m == 10, 1, NA)
    
    #plot(just_forest)
    # Match the resolutions
    biomass_modifier <-  resample(biomass_modifier, just_forest)
    #plot(biomass_modifier)
    #plot(just_forest,add=T)
    
    # Keep non-forest areas the same too!                      
    final_modifier <- mask(biomass_modifier,just_forest)
    # Keep none forest as they are -> 1 (as we ultimately take the prodict)
    final_modifier <- subst(final_modifier, NA, 1)

    
    
    # Implement the modifier
    cost_final <- lc_re*final_modifier
    
    # Mask out large water bodies - use the archived conductance layer
    mask[mask>0]<-1
    #plot(cond_1km)
    tmp <- rast(mask)
    tmp <- as.polygons(tmp)
    
    tmp <- project(tmp,cost_final )
    #plot(tmp)
    cost_final  <-mask(cost_final, tmp)
    plot(cost_final)
    
# Write this raster and upload it to google drive
    
writeRaster(cost_final, "Data/output/High_res_resistance_surface_final.tif", overwrite=T)
    
tmp <- project(cost_final, "+proj=utm +zone=17 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")

# Also make a version with NA's removed (required for least cost path models)
tmp2 <- classify(tmp, cbind(NA, 0.000001))
cost_final_17N <- as(tmp,"Raster")
cost_final_17N_NO_NA <- as(tmp2,"Raster")

writeRaster(cost_final_17N, "Data/output/High_res_resistance_surface_final_17N.tif", overwrite=T)
writeRaster(cost_final_17N_NO_NA, "Data/output/High_res_resistance_surface_final_17N_NO_NA.tif", overwrite=T)

# Try a 200m Res

remove(files, final_modifier, biomass, biomass_modifier, folder_id, just_forest,
       m2, mask, tmp, tmp2, lc_re_ras, lc_ras, lc_re)
       


