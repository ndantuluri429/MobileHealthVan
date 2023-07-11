# geolocations of mobile health vans


library(tidyverse)
library(ggplot2)
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












