# MOBILE HEALTH MAP DATA PREPROCESSING 
# By: Neha Dantuluri & Pia Davis

### Note (3/10/2025) This has a bunch of bugs! 

##### LOAD LIBS & PACKAGES #####

# install.packages("pacman")
require(pacman)
pacman:: p_load (pacman, dplyr, GGally, ggplot2, ggthemes, ggvis,
                 httr, lubridate, plotly, rio, rmarkdown, shiny,
                 stringr, tidyr, tidyverse, readxl, tidyverse, 
                 shinycssloaders, shinydashboard, leaflet, tigris,
                 sf, zipcodeR)


# set working directory!!! 
setwd("~/MHM2024")

##### LOAD DATASETS ##### 

# read PLACES dataset
PLACES_CENSUS_CORRECT <- read_csv("PLACES_CENSUS_CORRECT.csv")
# View(PLACES_CENSUS_CORRECT)

# read Mobile Health Map dataset
MHV2023 <- read_excel("MHVDATA2023.xlsx")
# View(MHV2023)

# read HRSA dataset
HRSA2024 <- read_excel("HRSA2024JUN.xlsx")
# View(HRSA2024)


#### MUTATE PLACES DATASET #####

# lowercase column names
names(PLACES_CENSUS_CORRECT) <- tolower(names(PLACES_CENSUS_CORRECT))

# Assign state names to state_names variable & put in alphabetical order
state_names = sort(unique(PLACES_CENSUS_CORRECT$statedesc))

# add latitude and longitude columns from geolocation
PLACES_CENSUS_CORRECT$longitude <- as.numeric(sub(".*\\((-?\\d+\\.\\d+)\\s-?\\d+\\.\\d+\\).*", "\\1", PLACES_CENSUS_CORRECT$geolocation))
PLACES_CENSUS_CORRECT$latitude <- as.numeric(sub(".*\\(-?\\d+\\.\\d+\\s(-?\\d+\\.\\d+)\\).*", "\\1", PLACES_CENSUS_CORRECT$geolocation))

#### MUTATE MOBILE HEALTH MAP DATASET ##### 

# US only clinics
MHV2023_US <- subset(MHV2023, country == "United States")

# clinics only w/ location
MHV2023_USLOCATIONS <- MHV2023_US[complete.cases(MHV2023_US$latitude, MHV2023_US$longitude), ]

# Change value for Whittier Street Health Center Mobile Health Van, zip code was incorrect
Whittier_Change <- which(MHV2023_USLOCATIONS$clinic_name == "Whittier Street Health Center Mobile Health Van")
MHV2023_USLOCATIONS$zip_postal_code[Whittier_Change] <- "02120"

# Observations that have NA values for state, but have postal code
NA_VANS <- which(is.na(MHV2023_USLOCATIONS$geo_state) & nchar(MHV2023_USLOCATIONS$zip_postal_code) == 5)

MHV_Zip2State <- function(postal_code) {
  return(reverse_zipcode(postal_code)$state)
}

for (index in NA_VANS) {
  MHV2023_USLOCATIONS$geo_state[index] <- MHV_Zip2State(MHV2023_USLOCATIONS$zip_postal_code[index])
}

##### PLACES VARIABLES FOR CENSUS DATA #####

# Variables for census tract data 
crudeprevs_descriptions <- c(
  "Current lack of health insurance among adults aged 18-64 years",
  "Arthritis among adults aged >=18 years",
  "Crude prevalence of binge drinking among adults aged >=18 years",
  "High blood pressure among adults aged >=18 years",
  "Taking medicine for high blood pressure control among adults aged >=18 years with high blood pressure",
  "Cancer (excluding skin cancer) among adults aged >=18 years",
  "Current asthma among adults aged >=18 years",
  "Cervical cancer screening among adult women aged 21–65 years",
  "Coronary heart disease among adults aged >=18 years",
  "Visits to doctor for routine checkup within the past year among adults aged >=18 years",
  "Cholesterol screening among adults aged >=18 years",
  "Fecal occult blood test, sigmoidoscopy, or colonoscopy among adults aged 50–75 years",
  "Chronic obstructive pulmonary disease among adults aged >=18 years",
  "Older adult men aged >=65 years who are up to date on a core set of clinical preventive services: Flu shot past year, PPV shot ever, Colorectal cancer screening",
  "Older adult women aged >=65 years who are up to date on a core set of clinical preventive services: Flu shot past year, PPV shot ever, Colorectal cancer screening, and Mammogram past 2 years",
  "Current smoking among adults aged >=18 years",
  "Visits to dentist or dental clinic among adults aged >=18 years",
  "Depression among adults aged >=18 years",
  "Diagnosed diabetes among adults aged >=18 years",
  "Fair or poor health among adults aged >=18 years",
  "High cholesterol among adults aged >=18 years who have been screened in the past 5 years",
  "Chronic kidney disease among adults aged >=18 years",
  "No leisure-time physical activity among adults aged >=18 years",
  "Mammography use among women aged 50–74 years",
  "Mental health not good for >=14 days among adults aged >=18 years",
  "Obesity among adults aged >=18 years",
  "Physical health not good for >=14 days among adults aged >=18 years",
  "Sleeping less than 7 hours among adults aged >=18 years",
  "Stroke among adults aged >=18 years",
  "All teeth lost among adults aged >=65 years",
  "Cognitive decline among adults aged >=18 years",
  "Mobility limitation among adults aged >=18 years",
  "Self-care limitation among adults aged >=18 years",
  "Independent living limitation among adults aged >=18 years",
  "Any disability among adults aged >=18 years"
)

### debug dataframe issue w/ column names & description
## print(colnames(PLACES_CENSUS_CORRECT))
# new_colnames <- colnames(PLACES_CENSUS_CORRECT)[grep("_crudeprev", colnames(PLACES_CENSUS_CORRECT))]
# length(new_colnames)
# print(new_colnames)
# print(crudeprevs_descriptions)


# Print all the new column names
# print(new_colnames)

# Compare with the descriptions
# print(length(new_colnames))
# print(length(crudeprevs_descriptions))

# Identify the columns that do not have descriptions
# missing_descriptions <- setdiff(new_colnames, crudeprevs_descriptions)
# print(missing_descriptions)

new_colnames <- c(
  "access2_crudeprev", "arthritis_crudeprev", "binge_crudeprev", "bphigh_crudeprev",
  "bpmed_crudeprev", "cancer_crudeprev", "casthma_crudeprev", "cervical_crudeprev",
  "chd_crudeprev", "checkup_crudeprev", "cholscreen_crudeprev", "colon_screen_crudeprev",
  "copd_crudeprev", "corem_crudeprev", "corew_crudeprev", "csmoking_crudeprev",
  "dental_crudeprev", "depression_crudeprev", "diabetes_crudeprev", "ghlth_crudeprev",
  "highchol_crudeprev", "kidney_crudeprev", "lpa_crudeprev", "mammouse_crudeprev",
  "mhlth_crudeprev", "obesity_crudeprev", "phlth_crudeprev", "sleep_crudeprev",
  "stroke_crudeprev", "teethlost_crudeprev", "hearing_crudeprev", "vision_crudeprev",
  "cognition_crudeprev", "mobility_crudeprev", "selfcare_crudeprev", "indeplive_crudeprev",
  "disability_crudeprev"
)

# Add descriptions for the missing columns
crudeprevs_descriptions <- c(
  "Current lack of health insurance among adults aged 18-64 years",
  "Arthritis among adults aged >=18 years",
  "Crude prevalence of binge drinking among adults aged >=18 years",
  "High blood pressure among adults aged >=18 years",
  "Taking medicine for high blood pressure control among adults aged >=18 years with high blood pressure",
  "Cancer (excluding skin cancer) among adults aged >=18 years",
  "Current asthma among adults aged >=18 years",
  "Cervical cancer screening among adult women aged 21–65 years",
  "Coronary heart disease among adults aged >=18 years",
  "Visits to doctor for routine checkup within the past year among adults aged >=18 years",
  "Cholesterol screening among adults aged >=18 years",
  "Fecal occult blood test, sigmoidoscopy, or colonoscopy among adults aged 50–75 years",
  "Chronic obstructive pulmonary disease among adults aged >=18 years",
  "Older adult men aged >=65 years who are up to date on a core set of clinical preventive services: Flu shot past year, PPV shot ever, Colorectal cancer screening",
  "Older adult women aged >=65 years who are up to date on a core set of clinical preventive services: Flu shot past year, PPV shot ever, Colorectal cancer screening, and Mammogram past 2 years",
  "Current smoking among adults aged >=18 years",
  "Visits to dentist or dental clinic among adults aged >=18 years",
  "Depression among adults aged >=18 years",
  "Diagnosed diabetes among adults aged >=18 years",
  "Fair or poor health among adults aged >=18 years",
  "High cholesterol among adults aged >=18 years who have been screened in the past 5 years",
  "Chronic kidney disease among adults aged >=18 years",
  "No leisure-time physical activity among adults aged >=18 years",
  "Mammography use among women aged 50–74 years",
  "Mental health not good for >=14 days among adults aged >=18 years",
  "Obesity among adults aged >=18 years",
  "Physical health not good for >=14 days among adults aged >=18 years",
  "Sleeping less than 7 hours among adults aged >=18 years",
  "Stroke among adults aged >=18 years",
  "All teeth lost among adults aged >=65 years",
  "Cognitive decline among adults aged >=18 years",
  "Mobility limitation among adults aged >=18 years",
  "Self-care limitation among adults aged >=18 years",
  "Independent living limitation among adults aged >=18 years",
  "Any disability among adults aged >=18 years",
  "Hearing impairment among adults aged >=18 years",
  "Vision impairment among adults aged >=18 years"
)

# Now create the data frame
new_colnames <- colnames(PLACES_CENSUS_CORRECT)[grep("_crudeprev$", colnames(PLACES_CENSUS_CORRECT))]
variable_descriptions <- data.frame("Variable" = new_colnames, "Description" = crudeprevs_descriptions)
# print(variable_descriptions)


variable_short <- c(
  "Lack of health insurance",
  "Arthritis",
  "Binge Drinking",
  "High Blood Pressure",
  "Medicine for High BP",
  "Cancer",
  "Asthma",
  "Cervical Cancer Screening",
  "Coronary Heart Disease",
  "Routine Checkup",
  "Cholesterol Screening",
  "Fecal occult blood test, sigmoidoscopy, or colonoscopy",
  "Chronic obstructive pulmonary disease",
  "Men Up to Date Core",
  "Women Up to Date Core",
  "Current Smoking",
  "Dental Visits",
  "Depression",
  "Diabetes",
  "Fair / Poor Health",
  "High Cholesterol",
  "Chronic Kidney Disease",
  "No leisure time physical activity",
  "Mammography",
  "Poor Mental Health",
  "Obesity",
  "Poor Physical Health",
  "Poor Sleeping",
  "Stroke",
  "Teeth Lost",
  "Hearing Impairment",
  "Vision Impairment",
  "Cognitive Decline",
  "Mobility Limitation",
  "Self-care Limitation",
  "Independent Living Limitation",
  "Any Disability"
)

# Now transform the data frame to include the short descriptions
variable_descriptions <- transform(variable_descriptions, "Short" = variable_short)
# print(variable_descriptions)


variable_names = sort(variable_descriptions$Short)

# For table displayed on Dashboard, so user can see details of each variable
variable_descriptions_2 <- variable_descriptions[, !colnames(variable_descriptions) %in% "Variable"]
new_order <- c("Short", "Description")
variable_descriptions_2 <- variable_descriptions_2[, new_order]


##### MHV DATA CATEGORIES #####

# Mobile Health Clinic Categories
clinic_categories = sort(c("Dental", "Mammography", "Other Specialties", "Primary Care", "Behavioral Health", "Sexual and Reproductive Health", "Vision", "Asthma", "Pediatrics", "Preventive", "Maternal and Infant Health"))


##### MUTATE HRSA DATA #####

# only mobile vans in HRSA 
HRSA2024 <- HRSA2024 %>%
  filter(`Health Center Location Type Description` == "Mobile Van")

# HRSA  latitude and longitude columns from geolocation
HRSA2024$longitude <- HRSA2024$`Geocoding Artifact Address Primary X Coordinate`
HRSA2024$latitude <- HRSA2024$`Geocoding Artifact Address Primary Y Coordinate`

# This gets all tracts with geo info (from all states)
# could probably replace this with existing variable
state_codes <- c("al", "ak", "az", "ar", "ca", "co", "ct", "de", "fl", "ga", "hi", "id", "il", "in", "ia",
                 "ks", "ky", "la", "me", "md", "ma", "mi", "mn", "ms", "mo", "mt", "ne", "nv", "nh", "nj",
                 "nm", "ny", "nc", "nd", "oh", "ok", "or", "pa", "ri", "sc", "sd", "tn", "tx", "ut", "vt",
                 "va", "wa", "wv", "wi", "wy", "dc")
get_state_tracts <- function(state_code) {
  tracts(cb = TRUE, year = 2015, state = state_code)
}
all_state_tracts <- do.call(rbind, lapply(state_codes, get_state_tracts))
all_state_tracts_sf <- st_as_sf(all_state_tracts)


#MHM
all_mhm_clinics_sf <- st_as_sf(MHV2023_USLOCATIONS, coords = c("longitude", "latitude"), crs = st_crs(all_state_tracts_sf))
all_mhm_clinics_with_tracts <- st_join(all_mhm_clinics_sf, all_state_tracts_sf)

#HRSA
all_hrsa_clinics_sf <- st_as_sf(HRSA2024, coords = c("longitude", "latitude"), crs = st_crs(all_state_tracts_sf))
all_hrsa_clinics_with_tracts <- st_join(all_hrsa_clinics_sf, all_state_tracts_sf)


# Tracts that have clinics in US
all_tracts_with_mhm_clinics <- unique(all_mhm_clinics_with_tracts$GEOID)
all_tracts_with_hrsa_clinics <- unique(all_hrsa_clinics_with_tracts$GEOID)

all_tracts_with_clinic <- c(all_tracts_with_mhm_clinics, all_tracts_with_hrsa_clinics)

# Added column to all_tracts where value 1 indicates that the tract has a clinic, 
# and value 0 indicates that the tract does not have a clinic 

# Ensure that tractfips and all_tracts_with_clinic are in the same format (e.g., both as character strings)
PLACES_CENSUS_CORRECT$tractfips <- as.character(PLACES_CENSUS_CORRECT$tractfips)
all_tracts_with_clinic <- as.character(all_tracts_with_clinic)

# Add the has_clinic column
PLACES_CENSUS_CORRECT$has_clinic <- ifelse(PLACES_CENSUS_CORRECT$tractfips %in% all_tracts_with_clinic, "Yes", "No")


##### ADD CENSUS SDOH DATA!! #####

SDOH_CENSUSDATA <- read_csv("24ACS_SDOH.csv")
# View(SDOH_CENSUSDATA)

# head(SDOH_CENSUSDATA$Geolocation)

# Extract longitude and latitude from the Geolocation column in SDOH_CENSUSDATA
SDOH_CENSUSDATA <- SDOH_CENSUSDATA %>%
  mutate(
    longitude = as.numeric(sub(".*\\((-?\\d+\\.\\d+)\\s-?\\d+\\.\\d+\\).*", "\\1", Geolocation)),
    latitude = as.numeric(sub(".*\\(-?\\d+\\.\\d+\\s(-?\\d+\\.\\d+)\\).*", "\\1", Geolocation))
  )

# Convert latitude and longitude columns to numeric in both datasets
PLACES_CENSUS_CORRECT$latitude <- as.numeric(PLACES_CENSUS_CORRECT$latitude)
PLACES_CENSUS_CORRECT$longitude <- as.numeric(PLACES_CENSUS_CORRECT$longitude)
SDOH_CENSUSDATA$latitude <- as.numeric(SDOH_CENSUSDATA$latitude)
SDOH_CENSUSDATA$longitude <- as.numeric(SDOH_CENSUSDATA$longitude)


# PLACES_CENSUS_CORRECT <- left_join(PLACES_CENSUS_CORRECT, SDOH_CENSUSDATA, by = c("latitude", "longitude"))

# Display the first few rows of the merged dataset to verify the result
# head(PLACES_CENSUS_CORRECT)

