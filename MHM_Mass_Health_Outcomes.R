# MHM Map + Crude Prevalence 
# w/ clinic locations & hyperlinks to each one

library(tigris)
library(dplyr)
library(leaflet)
library(readxl)

attach(census_tract_data)
attach(MHM_Data)


# change GEOID to a numeric variable
eva$GEOID <- as.double(eva$GEOID)
class(eva$GEOID)


# how many unique variables for service category

####### ARTHRITIS #############################################################


# arthritis = primary care, 
mhm_filtered <- subset(MHM_Data, country == "United States")
mhm_data_filtered <- mhm_filtered[complete.cases(mhm_filtered$latitude, mhm_filtered$longitude), ]
primarycare_mhm <- mhm_data_filtered[grep("Primary Care", mhm_data_filtered$general_service_category), ]
mhm_filtered_mass <- subset(primarycare_mhm, state_province %in% c("Massachusetts", "Ma"))


mass <- census_tract_data[census_tract_data$StateAbbr == "MA", ]
subset_mass_arthritis <- mass[,c("TractFIPS","ARTHRITIS_CrudePrev" )]
colnames(subset_mass_arthritis) <- c("GEOID", "value")


eva <- tracts(state="MA", cb=T)

# Convert the datum of the eva_sf object to WGS84
eva_sf <- st_as_sf(eva)

converted_sf <- st_transform(eva_sf, crs = "+proj=longlat +datum=WGS84")

leaflet() %>%
  addTiles() %>%
  addPolygons(data = converted_sf, popup = ~NAME)

eva %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(popup=~NAME)

eva_merged_arthritis <- geo_join(eva, subset_mass_arthritis, "GEOID", "GEOID")

pal <- colorNumeric("Reds", domain=eva_merged_arthritis$value)

eva_merged_arthritis <- subset(eva_merged_arthritis, (value))


popup_suff_arth <- paste0("Crude Prevalence: ", as.character(eva_merged_arthritis$value),
                          "\nTract: ", as.character(eva_merged_arthritis$TRACTCE))


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-71.0382679, 42.3489054, zoom = 7) %>%
  addPolygons(data=eva_merged_arthritis,
              fillColor =~pal(eva_merged_arthritis$value),
              fillOpacity = .7,
              weight = .4,
              smoothFactor = .2,
              popup =~popup_suff_arth) %>%
  addMarkers(data = mhm_filtered_mass, lng=mhm_filtered_mass$longitude, lat=mhm_filtered_mass$latitude, 
             popup = paste0("<a href='", mhm_filtered_mass$website, "' target='_blank'>", mhm_filtered_mass$clinic_name, "</a>")) %>%
  
  addLegend(pal=pal,
            values=eva_merged_arthritis$value,
            position = "bottomright",
            title="Crude Prevalence Arthritis")


####### ASTHMA #################################################################

# asthma = asthma, primary care 
mhm_filtered <- subset(MHM_Data, country == "United States")
mhm_data_filtered <- mhm_filtered[complete.cases(mhm_filtered$latitude, mhm_filtered$longitude), ]
primarycare_mhm <- mhm_data_filtered[grep("Primary Care", mhm_data_filtered$general_service_category), ]
asthma_mhm <- mhm_data_filtered[grep("Asthma", mhm_data_filtered$general_service_category), ]
mhm_filtered_mass <- subset(primarycare_mhm, asthma_mhm, state_province %in% c("Massachusetts", "Ma"))


mass <- census_tract_data[census_tract_data$StateAbbr == "MA", ]
subset_mass_asthma <- mass[,c("TractFIPS","CASTHMA_CrudePrev" )]
colnames(subset_mass_asthma) <- c("GEOID", "value")


eva <- tracts(state="MA", cb=T)

# Convert the datum of the eva_sf object to WGS84
eva_sf <- st_as_sf(eva)

converted_sf <- st_transform(eva_sf, crs = "+proj=longlat +datum=WGS84")

leaflet() %>%
  addTiles() %>%
  addPolygons(data = converted_sf, popup = ~NAME)

eva %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(popup=~NAME)

eva_merged_asthma <- geo_join(eva, subset_mass_asthma, "GEOID", "GEOID")

pal <- colorNumeric("Reds", domain=eva_merged_asthma$value)

eva_merged_asthma <- subset(eva_merged_asthma, (value))


popup_suff_asthma <- paste0("Crude Prevalence: ", as.character(eva_merged_asthma$value),
                          "\nTract: ", as.character(eva_merged_asthma$TRACTCE))


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-71.0382679, 42.3489054, zoom = 7) %>%
  addPolygons(data=eva_merged_asthma,
              fillColor =~pal(eva_merged_asthma$value),
              fillOpacity = .7,
              weight = .4,
              smoothFactor = .2,
              popup =~popup_suff_asthma) %>%
  addMarkers(data = mhm_filtered_mass, lng=mhm_filtered_mass$longitude, lat=mhm_filtered_mass$latitude, 
             popup = paste0("<a href='", mhm_filtered_mass$website, "' target='_blank'>", mhm_filtered_mass$clinic_name, "</a>")) %>%
  
  addLegend(pal=pal,
            values=eva_merged_asthma$value,
            position = "bottomright",
            title="Crude Prevalence Asthma")


####### HIGH BP ################################################################

# high bp = primary care, 
mhm_filtered <- subset(MHM_Data, country == "United States")
mhm_data_filtered <- mhm_filtered[complete.cases(mhm_filtered$latitude, mhm_filtered$longitude), ]
primarycare_mhm <- mhm_data_filtered[grep("Primary Care", mhm_data_filtered$general_service_category), ]
mhm_filtered_mass <- subset(primarycare_mhm, state_province %in% c("Massachusetts", "Ma"))


mass <- census_tract_data[census_tract_data$StateAbbr == "MA", ]
subset_mass_highBP <- mass[,c("TractFIPS","ARTHRITIS_CrudePrev" )]
colnames(subset_mass_highBP) <- c("GEOID", "value")


eva <- tracts(state="MA", cb=T)

# Convert the datum of the eva_sf object to WGS84
eva_sf <- st_as_sf(eva)

converted_sf <- st_transform(eva_sf, crs = "+proj=longlat +datum=WGS84")

leaflet() %>%
  addTiles() %>%
  addPolygons(data = converted_sf, popup = ~NAME)

eva %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(popup=~NAME)

eva_merged_highBP <- geo_join(eva, subset_mass_highBP, "GEOID", "GEOID")

pal <- colorNumeric("Reds", domain=eva_merged_highBP$value)

eva_merged_highBP <- subset(eva_merged_highBP, (value))


popup_suff_highBP <- paste0("Crude Prevalence: ", as.character(eva_merged_highBP$value),
                          "\nTract: ", as.character(eva_merged_highBP$TRACTCE))


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-71.0382679, 42.3489054, zoom = 7) %>%
  addPolygons(data=eva_merged_highBP,
              fillColor =~pal(eva_merged_highBP$value),
              fillOpacity = .7,
              weight = .4,
              smoothFactor = .2,
              popup =~popup_suff_arth) %>%
  addMarkers(data = mhm_filtered_mass, lng=mhm_filtered_mass$longitude, lat=mhm_filtered_mass$latitude, 
             popup = paste0("<a href='", mhm_filtered_mass$website, "' target='_blank'>", mhm_filtered_mass$clinic_name, "</a>")) %>%
  
  addLegend(pal=pal,
            values=eva_merged_highBP$value,
            position = "bottomright",
            title="High Blood Pressure")

####### CANCER (Excluding Skin Cancer) #########################################



# arthritis = primary care, 
mhm_filtered <- subset(MHM_Data, country == "United States")
mhm_data_filtered <- mhm_filtered[complete.cases(mhm_filtered$latitude, mhm_filtered$longitude), ]
primarycare_mhm <- mhm_data_filtered[grep("Primary Care", mhm_data_filtered$general_service_category), ]
mhm_filtered_mass <- subset(primarycare_mhm, state_province %in% c("Massachusetts", "Ma"))


mass <- census_tract_data[census_tract_data$StateAbbr == "MA", ]
subset_mass_arthritis <- mass[,c("TractFIPS","ARTHRITIS_CrudePrev" )]
colnames(subset_mass_arthritis) <- c("GEOID", "value")


eva <- tracts(state="MA", cb=T)

# Convert the datum of the eva_sf object to WGS84
eva_sf <- st_as_sf(eva)

converted_sf <- st_transform(eva_sf, crs = "+proj=longlat +datum=WGS84")

leaflet() %>%
  addTiles() %>%
  addPolygons(data = converted_sf, popup = ~NAME)

eva %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(popup=~NAME)

eva_merged_arthritis <- geo_join(eva, subset_mass_arthritis, "GEOID", "GEOID")

pal <- colorNumeric("Reds", domain=eva_merged_arthritis$value)

eva_merged_arthritis <- subset(eva_merged_arthritis, (value))


popup_suff_arth <- paste0("Crude Prevalence: ", as.character(eva_merged_arthritis$value),
                          "\nTract: ", as.character(eva_merged_arthritis$TRACTCE))


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-71.0382679, 42.3489054, zoom = 7) %>%
  addPolygons(data=eva_merged_arthritis,
              fillColor =~pal(eva_merged_arthritis$value),
              fillOpacity = .7,
              weight = .4,
              smoothFactor = .2,
              popup =~popup_suff_arth) %>%
  addMarkers(data = mhm_filtered_mass, lng=mhm_filtered_mass$longitude, lat=mhm_filtered_mass$latitude, 
             popup = paste0("<a href='", mhm_filtered_mass$website, "' target='_blank'>", mhm_filtered_mass$clinic_name, "</a>")) %>%
  
  addLegend(pal=pal,
            values=eva_merged_arthritis$value,
            position = "bottomright",
            title="Crude Prevalence Arthritis")


####### ASTHMA #################################################################


# cancer = primary care 
mhm_filtered <- subset(MHM_Data, country == "United States")
mhm_data_filtered <- mhm_filtered[complete.cases(mhm_filtered$latitude, mhm_filtered$longitude), ]
primarycare_mhm <- mhm_data_filtered[grep("Primary Care", mhm_data_filtered$general_service_category), ]
mhm_filtered_mass <- subset(primarycare_mhm, state_province %in% c("Massachusetts", "Ma"))


mass <- census_tract_data[census_tract_data$StateAbbr == "MA", ]
subset_mass_cancer <- mass[,c("TractFIPS","CANCER_CrudePrev" )]
colnames(subset_mass_cancer) <- c("GEOID", "value")


eva <- tracts(state="MA", cb=T)

# Convert the datum of the eva_sf object to WGS84
eva_sf <- st_as_sf(eva)

converted_sf <- st_transform(eva_sf, crs = "+proj=longlat +datum=WGS84")

leaflet() %>%
  addTiles() %>%
  addPolygons(data = converted_sf, popup = ~NAME)

eva %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(popup=~NAME)

eva_merged_cancer <- geo_join(eva, subset_mass_cancer, "GEOID", "GEOID")

pal <- colorNumeric("Reds", domain=eva_merged_cancer$value)

eva_merged_cancer <- subset(eva_merged_cancer, (value))


popup_suff_cancer <- paste0("Crude Prevalence: ", as.character(eva_merged_cancer$value),
                            "\nTract: ", as.character(eva_merged_cancer$TRACTCE))


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-71.0382679, 42.3489054, zoom = 7) %>%
  addPolygons(data=eva_merged_cancer,
              fillColor =~pal(eva_merged_cancer$value),
              fillOpacity = .7,
              weight = .4,
              smoothFactor = .2,
              popup =~popup_suff_cancer) %>%
  addMarkers(data = mhm_filtered_mass, lng=mhm_filtered_mass$longitude, lat=mhm_filtered_mass$latitude, 
             popup = paste0("<a href='", mhm_filtered_mass$website, "' target='_blank'>", mhm_filtered_mass$clinic_name, "</a>")) %>%
  
  addLegend(pal=pal,
            values=eva_merged_cancer$value,
            position = "bottomright",
            title="Crude Prevalence Cancer (Not Including Skin Cancer)")



####### HIGH Cholesterol #######################################################


# HIGH Cholesterol = primary care 
mhm_filtered <- subset(MHM_Data, country == "United States")
mhm_data_filtered <- mhm_filtered[complete.cases(mhm_filtered$latitude, mhm_filtered$longitude), ]
primarycare_mhm <- mhm_data_filtered[grep("Primary Care", mhm_data_filtered$general_service_category), ]
mhm_filtered_mass <- subset(primarycare_mhm, state_province %in% c("Massachusetts", "Ma"))


mass <- census_tract_data[census_tract_data$StateAbbr == "MA", ]
subset_mass_highchol <- mass[,c("TractFIPS","HIGHCHOL_CrudePrev" )]
colnames(subset_mass_highchol) <- c("GEOID", "value")


eva <- tracts(state="MA", cb=T)

# Convert the datum of the eva_sf object to WGS84
eva_sf <- st_as_sf(eva)

converted_sf <- st_transform(eva_sf, crs = "+proj=longlat +datum=WGS84")

leaflet() %>%
  addTiles() %>%
  addPolygons(data = converted_sf, popup = ~NAME)

eva %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(popup=~NAME)

eva_merged_highchol <- geo_join(eva, subset_mass_highchol, "GEOID", "GEOID")

pal <- colorNumeric("Reds", domain=eva_merged_highchol$value)

eva_merged_highchol <- subset(eva_merged_highchol, (value))


popup_suff_highchol <- paste0("Crude Prevalence: ", as.character(eva_merged_highchol$value),
                            "\nTract: ", as.character(eva_merged_highchol$TRACTCE))


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-71.0382679, 42.3489054, zoom = 7) %>%
  addPolygons(data=eva_merged_highchol,
              fillColor =~pal(eva_merged_highchol$value),
              fillOpacity = .7,
              weight = .4,
              smoothFactor = .2,
              popup =~popup_suff_highchol) %>%
  addMarkers(data = mhm_filtered_mass, lng=mhm_filtered_mass$longitude, lat=mhm_filtered_mass$latitude, 
             popup = paste0("<a href='", mhm_filtered_mass$website, "' target='_blank'>", mhm_filtered_mass$clinic_name, "</a>")) %>%
  
  addLegend(pal=pal,
            values=eva_merged_highchol$value,
            position = "bottomright",
            title="Crude Prevalence High Cholesterol")


####### Chronic Kidney Disease #################################################

# CKD = primary care
mhm_filtered <- subset(MHM_Data, country == "United States")
mhm_data_filtered <- mhm_filtered[complete.cases(mhm_filtered$latitude, mhm_filtered$longitude), ]
primarycare_mhm <- mhm_data_filtered[grep("Primary Care", mhm_data_filtered$general_service_category), ]
mhm_filtered_mass <- subset(primarycare_mhm, state_province %in% c("Massachusetts", "Ma"))


mass <- census_tract_data[census_tract_data$StateAbbr == "MA", ]
subset_mass_CKD <- mass[,c("TractFIPS","KIDNEY_CrudePrev" )]
colnames(subset_mass_CKD) <- c("GEOID", "value")


eva <- tracts(state="MA", cb=T)

# Convert the datum of the eva_sf object to WGS84
eva_sf <- st_as_sf(eva)

converted_sf <- st_transform(eva_sf, crs = "+proj=longlat +datum=WGS84")

leaflet() %>%
  addTiles() %>%
  addPolygons(data = converted_sf, popup = ~NAME)

eva %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(popup=~NAME)

eva_merged_CKD <- geo_join(eva, subset_mass_CKD, "GEOID", "GEOID")

pal <- colorNumeric("Reds", domain=eva_merged_CKD$value)

eva_merged_CKD <- subset(eva_merged_CKD, (value))


popup_suff_CKD <- paste0("Crude Prevalence: ", as.character(eva_merged_CKD$value),
                              "\nTract: ", as.character(eva_merged_CKD$TRACTCE))


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-71.0382679, 42.3489054, zoom = 7) %>%
  addPolygons(data=eva_merged_CKD,
              fillColor =~pal(eva_merged_CKD$value),
              fillOpacity = .7,
              weight = .4,
              smoothFactor = .2,
              popup =~popup_suff_CKD) %>%
  addMarkers(data = mhm_filtered_mass, lng=mhm_filtered_mass$longitude, lat=mhm_filtered_mass$latitude, 
             popup = paste0("<a href='", mhm_filtered_mass$website, "' target='_blank'>", mhm_filtered_mass$clinic_name, "</a>")) %>%
  
  addLegend(pal=pal,
            values=eva_merged_CKD$value,
            position = "bottomright",
            title="Crude Prevalence Chronic Kidney Disease")



####### Chronic Obstructive Pulmonary Disease ##################################


# COPD = primary care
mhm_filtered <- subset(MHM_Data, country == "United States")
mhm_data_filtered <- mhm_filtered[complete.cases(mhm_filtered$latitude, mhm_filtered$longitude), ]
primarycare_mhm <- mhm_data_filtered[grep("Primary Care", mhm_data_filtered$general_service_category), ]
mhm_filtered_mass <- subset(primarycare_mhm, state_province %in% c("Massachusetts", "Ma"))


mass <- census_tract_data[census_tract_data$StateAbbr == "MA", ]
subset_mass_COPD <- mass[,c("TractFIPS","COPD_CrudePrev" )]
colnames(subset_mass_COPD) <- c("GEOID", "value")


eva <- tracts(state="MA", cb=T)

# Convert the datum of the eva_sf object to WGS84
eva_sf <- st_as_sf(eva)

converted_sf <- st_transform(eva_sf, crs = "+proj=longlat +datum=WGS84")

leaflet() %>%
  addTiles() %>%
  addPolygons(data = converted_sf, popup = ~NAME)

eva %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(popup=~NAME)

eva_merged_COPD <- geo_join(eva, subset_mass_COPD, "GEOID", "GEOID")

pal <- colorNumeric("Reds", domain=eva_merged_COPD$value)

eva_merged_COPD <- subset(eva_merged_COPD, (value))


popup_suff_COPD <- paste0("Crude Prevalence: ", as.character(eva_merged_COPD$value),
                         "\nTract: ", as.character(eva_merged_COPD$TRACTCE))


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-71.0382679, 42.3489054, zoom = 7) %>%
  addPolygons(data=eva_merged_CKD,
              fillColor =~pal(eva_merged_COPD$value),
              fillOpacity = .7,
              weight = .4,
              smoothFactor = .2,
              popup =~popup_suff_COPD) %>%
  addMarkers(data = mhm_filtered_mass, lng=mhm_filtered_mass$longitude, lat=mhm_filtered_mass$latitude, 
             popup = paste0("<a href='", mhm_filtered_mass$website, "' target='_blank'>", mhm_filtered_mass$clinic_name, "</a>")) %>%
  
  addLegend(pal=pal,
            values=eva_merged_COPD$value,
            position = "bottomright",
            title="Crude Prevalence Chronic Obstructive Pulmonary Disease")


####### Coronary Heart Disease #################################################

# CHD = primary care
mhm_filtered <- subset(MHM_Data, country == "United States")
mhm_data_filtered <- mhm_filtered[complete.cases(mhm_filtered$latitude, mhm_filtered$longitude), ]
primarycare_mhm <- mhm_data_filtered[grep("Primary Care", mhm_data_filtered$general_service_category), ]
mhm_filtered_mass <- subset(primarycare_mhm, state_province %in% c("Massachusetts", "Ma"))


mass <- census_tract_data[census_tract_data$StateAbbr == "MA", ]
subset_mass_CHD <- mass[,c("TractFIPS","CHD_CrudePrev" )]
colnames(subset_mass_CHD) <- c("GEOID", "value")


eva <- tracts(state="MA", cb=T)

# Convert the datum of the eva_sf object to WGS84
eva_sf <- st_as_sf(eva)

converted_sf <- st_transform(eva_sf, crs = "+proj=longlat +datum=WGS84")

leaflet() %>%
  addTiles() %>%
  addPolygons(data = converted_sf, popup = ~NAME)

eva %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(popup=~NAME)

eva_merged_CHD <- geo_join(eva, subset_mass_CHD, "GEOID", "GEOID")

pal <- colorNumeric("Reds", domain=eva_merged_CHD$value)

eva_merged_CHD <- subset(eva_merged_CHD, (value))


popup_suff_CHD <- paste0("Crude Prevalence: ", as.character(eva_merged_CHD$value),
                          "\nTract: ", as.character(eva_merged_CHD$TRACTCE))


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-71.0382679, 42.3489054, zoom = 7) %>%
  addPolygons(data=eva_merged_CHD,
              fillColor =~pal(eva_merged_CHD$value),
              fillOpacity = .7,
              weight = .4,
              smoothFactor = .2,
              popup =~popup_suff_CHD) %>%
  addMarkers(data = mhm_filtered_mass, lng=mhm_filtered_mass$longitude, lat=mhm_filtered_mass$latitude, 
             popup = paste0("<a href='", mhm_filtered_mass$website, "' target='_blank'>", mhm_filtered_mass$clinic_name, "</a>")) %>%
  
  addLegend(pal=pal,
            values=eva_merged_CHD$value,
            position = "bottomright",
            title="Crude Prevalence Coronary Heart Disease")



####### Depression #############################################################


# Depression = primary care, behavioral health
mhm_filtered <- subset(MHM_Data, country == "United States")
mhm_data_filtered <- mhm_filtered[complete.cases(mhm_filtered$latitude, mhm_filtered$longitude), ]
primarycare_mhm <- mhm_data_filtered[grep("Primary Care", mhm_data_filtered$general_service_category), ]
behavioral_healh_mhm <- mhm_data_filtered[grep("Behavioral Health", mhm_data_filtered$general_service_category), ]
mhm_filtered_mass <- subset(primarycare_mhm, behavioral_healh_mhm, state_province %in% c("Massachusetts", "Ma"))


mass <- census_tract_data[census_tract_data$StateAbbr == "MA", ]
subset_mass_DEPRESSION <- mass[,c("TractFIPS","DEPRESSION_CrudePrev" )]
colnames(subset_mass_DEPRESSION) <- c("GEOID", "value")


eva <- tracts(state="MA", cb=T)

# Convert the datum of the eva_sf object to WGS84
eva_sf <- st_as_sf(eva)

converted_sf <- st_transform(eva_sf, crs = "+proj=longlat +datum=WGS84")

leaflet() %>%
  addTiles() %>%
  addPolygons(data = converted_sf, popup = ~NAME)

eva %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(popup=~NAME)

eva_merged_DEPRESSION <- geo_join(eva, subset_mass_DEPRESSION, "GEOID", "GEOID")

pal <- colorNumeric("Reds", domain=eva_merged_DEPRESSION$value)

eva_merged_DEPRESSION <- subset(eva_merged_DEPRESSION, (value))


popup_suff_DEPRESSION <- paste0("Crude Prevalence: ", as.character(eva_merged_DEPRESSION$value),
                         "\nTract: ", as.character(eva_merged_DEPRESSION$TRACTCE))


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-71.0382679, 42.3489054, zoom = 7) %>%
  addPolygons(data=eva_merged_DEPRESSION,
              fillColor =~pal(eva_merged_DEPRESSION$value),
              fillOpacity = .7,
              weight = .4,
              smoothFactor = .2,
              popup =~popup_suff_DEPRESSION) %>%
  addMarkers(data = mhm_filtered_mass, lng=mhm_filtered_mass$longitude, lat=mhm_filtered_mass$latitude, 
             popup = paste0("<a href='", mhm_filtered_mass$website, "' target='_blank'>", mhm_filtered_mass$clinic_name, "</a>")) %>%
  
  addLegend(pal=pal,
            values=eva_merged_DEPRESSION$value,
            position = "bottomright",
            title="Crude Prevalence Depression")

####### Diagnosed Diabetes  ####################################################

# diabetes = primary care
mhm_filtered <- subset(MHM_Data, country == "United States")
mhm_data_filtered <- mhm_filtered[complete.cases(mhm_filtered$latitude, mhm_filtered$longitude), ]
primarycare_mhm <- mhm_data_filtered[grep("Primary Care", mhm_data_filtered$general_service_category), ]
mhm_filtered_mass <- subset(primarycare_mhm, state_province %in% c("Massachusetts", "Ma"))


mass <- census_tract_data[census_tract_data$StateAbbr == "MA", ]
subset_mass_DIABETES <- mass[,c("TractFIPS","DIABETES_CrudePrev" )]
colnames(subset_mass_DIABETES) <- c("GEOID", "value")


eva <- tracts(state="MA", cb=T)

# Convert the datum of the eva_sf object to WGS84
eva_sf <- st_as_sf(eva)

converted_sf <- st_transform(eva_sf, crs = "+proj=longlat +datum=WGS84")

leaflet() %>%
  addTiles() %>%
  addPolygons(data = converted_sf, popup = ~NAME)

eva %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(popup=~NAME)

eva_merged_DIABETES <- geo_join(eva, subset_mass_DIABETES, "GEOID", "GEOID")

pal <- colorNumeric("Reds", domain=eva_merged_DIABETES$value)

eva_merged_DIABETES <- subset(eva_merged_DIABETES, (value))


popup_suff_DIABETES <- paste0("Crude Prevalence: ", as.character(eva_merged_DIABETES$value),
                         "\nTract: ", as.character(eva_merged_DIABETES$TRACTCE))


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-71.0382679, 42.3489054, zoom = 7) %>%
  addPolygons(data=eva_merged_DIABETES,
              fillColor =~pal(eva_merged_DIABETES$value),
              fillOpacity = .7,
              weight = .4,
              smoothFactor = .2,
              popup =~popup_suff_DIABETES) %>%
  addMarkers(data = mhm_filtered_mass, lng=mhm_filtered_mass$longitude, lat=mhm_filtered_mass$latitude, 
             popup = paste0("<a href='", mhm_filtered_mass$website, "' target='_blank'>", mhm_filtered_mass$clinic_name, "</a>")) %>%
  
  addLegend(pal=pal,
            values=eva_merged_DIABETES$value,
            position = "bottomright",
            title="Crude Prevalence Diabetes")



####### Obesity ################################################################

# Obesity = primary care
mhm_filtered <- subset(MHM_Data, country == "United States")
mhm_data_filtered <- mhm_filtered[complete.cases(mhm_filtered$latitude, mhm_filtered$longitude), ]
primarycare_mhm <- mhm_data_filtered[grep("Primary Care", mhm_data_filtered$general_service_category), ]
mhm_filtered_mass <- subset(primarycare_mhm, state_province %in% c("Massachusetts", "Ma"))


mass <- census_tract_data[census_tract_data$StateAbbr == "MA", ]
subset_mass_OBESITY <- mass[,c("TractFIPS","OBESITY_CrudePrev" )]
colnames(subset_mass_OBESITY) <- c("GEOID", "value")


eva <- tracts(state="MA", cb=T)

# Convert the datum of the eva_sf object to WGS84
eva_sf <- st_as_sf(eva)

converted_sf <- st_transform(eva_sf, crs = "+proj=longlat +datum=WGS84")

leaflet() %>%
  addTiles() %>%
  addPolygons(data = converted_sf, popup = ~NAME)

eva %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(popup=~NAME)

eva_merged_OBESITY <- geo_join(eva, subset_mass_OBESITY, "GEOID", "GEOID")

pal <- colorNumeric("Reds", domain=eva_merged_OBESITY$value)

eva_merged_OBESITY <- subset(eva_merged_OBESITY, (value))


popup_suff_OBESITY <- paste0("Crude Prevalence: ", as.character(eva_merged_OBESITY$value),
                              "\nTract: ", as.character(eva_merged_OBESITY$TRACTCE))


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-71.0382679, 42.3489054, zoom = 7) %>%
  addPolygons(data=eva_merged_OBESITY,
              fillColor =~pal(eva_merged_OBESITY$value),
              fillOpacity = .7,
              weight = .4,
              smoothFactor = .2,
              popup =~popup_suff_OBESITY) %>%
  addMarkers(data = mhm_filtered_mass, lng=mhm_filtered_mass$longitude, lat=mhm_filtered_mass$latitude, 
             popup = paste0("<a href='", mhm_filtered_mass$website, "' target='_blank'>", mhm_filtered_mass$clinic_name, "</a>")) %>%
  
  addLegend(pal=pal,
            values=eva_merged_OBESITY$value,
            position = "bottomright",
            title="Crude Prevalence Obesity")



####### All Teeth Lost #########################################################

glimpse(census_tract_data)

# all teeth lost = dental care
mhm_filtered <- subset(MHM_Data, country == "United States")
mhm_data_filtered <- mhm_filtered[complete.cases(mhm_filtered$latitude, mhm_filtered$longitude), ]
dental_mhm <- mhm_data_filtered[grep("Dental", mhm_data_filtered$general_service_category), ]
mhm_filtered_mass <- subset(dental_mhm, state_province %in% c("Massachusetts", "Ma"))


mass <- census_tract_data[census_tract_data$StateAbbr == "MA", ]
subset_mass_TEETHLOST <- mass[,c("TractFIPS","TEETHLOST_CrudePrev" )]
colnames(subset_mass_TEETHLOST) <- c("GEOID", "value")


eva <- tracts(state="MA", cb=T)

# Convert the datum of the eva_sf object to WGS84
eva_sf <- st_as_sf(eva)

converted_sf <- st_transform(eva_sf, crs = "+proj=longlat +datum=WGS84")

leaflet() %>%
  addTiles() %>%
  addPolygons(data = converted_sf, popup = ~NAME)

eva %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(popup=~NAME)

eva_merged_TEETHLOST <- geo_join(eva, subset_mass_TEETHLOST, "GEOID", "GEOID")

pal <- colorNumeric("Reds", domain=eva_merged_TEETHLOST$value)

eva_merged_TEETHLOST <- subset(eva_merged_TEETHLOST, (value))


popup_suff_TEETHLOST <- paste0("Crude Prevalence: ", as.character(eva_merged_TEETHLOST$value),
                             "\nTract: ", as.character(eva_merged_TEETHLOST$TRACTCE))


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-71.0382679, 42.3489054, zoom = 7) %>%
  addPolygons(data=eva_merged_TEETHLOST,
              fillColor =~pal(eva_merged_TEETHLOST$value),
              fillOpacity = .7,
              weight = .4,
              smoothFactor = .2,
              popup =~popup_suff_TEETHLOST) %>%
  addMarkers(data = mhm_filtered_mass, lng=mhm_filtered_mass$longitude, lat=mhm_filtered_mass$latitude, 
             popup = paste0("<a href='", mhm_filtered_mass$website, "' target='_blank'>", mhm_filtered_mass$clinic_name, "</a>")) %>%
  
  addLegend(pal=pal,
            values=eva_merged_TEETHLOST$value,
            position = "bottomright",
            title="Crude Prevalence for All Teeth Lost")


####### Stroke #################################################################

# stroke = primary care
mhm_filtered <- subset(MHM_Data, country == "United States")
mhm_data_filtered <- mhm_filtered[complete.cases(mhm_filtered$latitude, mhm_filtered$longitude), ]
primarycare_mhm <- mhm_data_filtered[grep("Primary Care", mhm_data_filtered$general_service_category), ]
mhm_filtered_mass <- subset(primarycare_mhm, state_province %in% c("Massachusetts", "Ma"))


mass <- census_tract_data[census_tract_data$StateAbbr == "MA", ]
subset_mass_STROKE <- mass[,c("TractFIPS","STROKE_CrudePrev" )]
colnames(subset_mass_STROKE) <- c("GEOID", "value")


eva <- tracts(state="MA", cb=T)

# Convert the datum of the eva_sf object to WGS84
eva_sf <- st_as_sf(eva)

converted_sf <- st_transform(eva_sf, crs = "+proj=longlat +datum=WGS84")

leaflet() %>%
  addTiles() %>%
  addPolygons(data = converted_sf, popup = ~NAME)

eva %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(popup=~NAME)

eva_merged_STROKE <- geo_join(eva, subset_mass_STROKE, "GEOID", "GEOID")

pal <- colorNumeric("Reds", domain=eva_merged_STROKE$value)

eva_merged_STROKE <- subset(eva_merged_STROKE, (value))


popup_suff_STROKE <- paste0("Crude Prevalence: ", as.character(eva_merged_STROKE$value),
                             "\nTract: ", as.character(eva_merged_STROKE$TRACTCE))


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-71.0382679, 42.3489054, zoom = 7) %>%
  addPolygons(data=eva_merged_STROKE,
              fillColor =~pal(eva_merged_STROKE$value),
              fillOpacity = .7,
              weight = .4,
              smoothFactor = .2,
              popup =~popup_suff_STROKE) %>%
  addMarkers(data = mhm_filtered_mass, lng=mhm_filtered_mass$longitude, lat=mhm_filtered_mass$latitude, 
             popup = paste0("<a href='", mhm_filtered_mass$website, "' target='_blank'>", mhm_filtered_mass$clinic_name, "</a>")) %>%
  
  addLegend(pal=pal,
            values=eva_merged_STROKE$value,
            position = "bottomright",
            title="Crude Prevalence Stroke")










