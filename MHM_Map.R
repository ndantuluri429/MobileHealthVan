# geolocations of mobile health vans


library(tidyverse)
library(ggplot2)
library(dplyr)
library(mapview)
library(sf)

# accessing and viewing file in R
attach(MHM_Data)
view(MHM_Data) 


# mapping location of all of the clinics
clinic_locations <- MHM_Data %>% 
  drop_na(clinic_name, latitude, longitude)

clinic_locations %>% 
  glimpse()

mapview(clinic_locations, 
        xcol = "longitude", 
        ycol = "latitude", 
        crs = 4269, grid = FALSE)



# mapping clinic locations only in USA
clinic_locations_usa <- MHM_Data %>%
  drop_na(country, clinic_name, latitude, longitude) %>%
  filter(country == "United States")

clinic_locations_usa %>% 
  glimpse()

mapview(clinic_locations_usa,
      xcol = "longitude",
      ycol = "latitude",
      crs = 4269, 
      grid = FALSE,
      label = "MHV Clinics") # needs to be corrected

# maps specific to health issues 




