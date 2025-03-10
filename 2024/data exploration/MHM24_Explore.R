###EXPLORING NEW MHM DATASET###

# Install and load necessary packages
install.packages(c("leaflet", "sf", "dplyr","readxl", 
                   "tmap", "tidyverse", "tidyr", "ggplot2",
                   "readr", "stringr", "janitor"))

library(leaflet)
library(sf)
library(dplyr)
library(readxl)
library(tmap)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
library(janitor)

# load and view mobile health map (2024) data
mhm_24_raw_data <- read_excel("/Users/nehadantuluri/Downloads/MHV2024.xlsx")
View(mhm_24_raw_data)


# clean column names w/ janitor
# standardizes the column names by converting them to lowercase, replacing spaces with underscores, and removing any special characters.
mhm_24_raw_data_clean<-clean_names(mhm_24_raw_data)
colnames(mhm_24_raw_data_clean)


# view full dataset with cleaned column names
View(mhm_24_raw_data_clean)


# show first 6 rows & last 6 rows of data set
head(mhm_24_raw_data_clean)
tail(mhm_24_raw_data_clean)


# characteristics of a specific variable
length(mhm_24_raw_data_clean$title) # 1453 data points of this
class(mhm_24_raw_data_clean$title) # chr variable
unique(mhm_24_raw_data_clean$community_type)


## UNIQUENESS OF DATA POINTS
# how many unique values for post_type?
unique(mhm_24_raw_data_clean$post_type) 

# how many unique values for status?
unique(mhm_24_raw_data_clean$status) 

# how many unique values for organization_type
unique(mhm_24_raw_data_clean$organization_type) 

# how many unique values for organization_type_other
unique(mhm_24_raw_data_clean$organization_type_other) 

# how many unique values for source_of_funding 
unique(mhm_24_raw_data_clean$source_of_funding) 

# how many unique values for organization_collaboration_other
unique(mhm_24_raw_data_clean$organization_collaboration_other) 

# how many unique values for clinic_country
unique(mhm_24_raw_data_clean$clinic_country) 

# how many unique values for care_delivered 
unique(mhm_24_raw_data_clean$care_delivered) 

# how many unique values for community_type 
unique(mhm_24_raw_data_clean$community_type) 

# how many unique values for population_served
unique(mhm_24_raw_data_clean$population_served) 




