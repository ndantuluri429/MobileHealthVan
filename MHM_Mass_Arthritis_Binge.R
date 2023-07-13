# MHM Map + Crude Prevalence 
# w/ clinic locations & hyperlinks to each one

library(tigris)
library(dplyr)
library(leaflet)
library(readxl)

attach(census_tract_data)
attach(MHM_Data)


# change GEOID to a numeric variable
glimpse(eva)
class(eva$GEOID)

eva$GEOID <- as.double(eva$GEOID)


mhm_filtered <- subset(MHM_Data, country == "United States")
mhm_data_filtered <- mhm_filtered[complete.cases(mhm_filtered$latitude, mhm_filtered$longitude), ]
mhm_filtered_mass <- subset(mhm_data_filtered, state_province %in% c("Massachusetts", "Ma"))



####### ARTHRITIS #############################################################


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
  addPolygons(data=eva_merged_binge,
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

####### BINGE DRINKING ########################################################

mass <- census_tract_data[census_tract_data$StateAbbr == "MA", ]
subset_mass_binge <- mass[,c("TractFIPS","BINGE_CrudePrev" )]
colnames(subset_mass_binge) <- c("GEOID", "value")


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

eva_merged_binge <- geo_join(eva, subset_mass_binge, "GEOID", "GEOID")

pal <- colorNumeric("Reds", domain=eva_merged_binge$value)

eva_merged_binge <- subset(eva_merged_binge,(value))


popup_suff_binge <- paste0("Crude Prevalence: ", as.character(eva_merged_binge$value),
                           "\nTract: ", as.character(eva_merged_binge$TRACTCE))


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-71.0382679, 42.3489054, zoom = 7) %>%
  addPolygons(data=eva_merged_binge,
              fillColor =~pal(eva_merged_binge$value),
              fillOpacity = .7,
              weight = .4,
              smoothFactor = .2,
              popup =~popup_suff_arth) %>%
  addMarkers(data = mhm_filtered_mass, lng=mhm_filtered_mass$longitude, lat=mhm_filtered_mass$latitude, 
             popup = paste0("<a href='", mhm_filtered_mass$website, "' target='_blank'>", mhm_filtered_mass$clinic_name, "</a>")) %>%
  
  addLegend(pal=pal,
            values=eva_merged_binge$value,
            position = "bottomright",
            title="Crude Prevalence Binge Drinking")





