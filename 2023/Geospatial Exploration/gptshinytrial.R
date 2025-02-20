library(shiny)
library(shinydashboard)
library(tigris)
library(sf)
library(leaflet)
library(readxl)
library(zipcodeR)
library(readr)
library(maps)
library(tidyverse)
library(dplyr)



# read census tract data
census_tract_data = read.csv("/Users/nehadantuluri/Downloads/census_tract_data.csv")
names(census_tract_data) <- tolower(names(census_tract_data))


# add latitude and longitude columns from geolocation
census_tract_data$longitude <- as.numeric(sub(".*\\((-?\\d+\\.\\d+)\\s-?\\d+\\.\\d+\\).*", "\\1", census_tract_data$geolocation))
census_tract_data$latitude <- as.numeric(sub(".*\\(-?\\d+\\.\\d+\\s(-?\\d+\\.\\d+)\\).*", "\\1", census_tract_data$geolocation))

# State names
state_names = sort(unique(census_tract_data$statedesc))


# Variables for census tract data 
crudeprevs_descriptions <- c("Current lack of health insurance among adults aged 18-64 years", "Arthritis among adults aged >=18 years", "Crude prevalence of binge drinking among adults aged >=18 years", 
                             "High blood pressure among adults aged >=18 years", "Taking medicine for high blood pressure control among adults aged >=18 years with high blood pressure", "Cancer (excluding skin cancer) among adults aged >=18 years", 
                             "Current asthma among adults aged >=18 years", "cervical cancer screening among adult women aged 21–65 years", "Coronary heart disease among adults aged >=18 years", "Visits to doctor for routine checkup within the past year among adults aged >=18 years",
                             "cholesterol screening among adults aged >=18 years", "Fecal occult blood test, sigmoidoscopy, or colonoscopy among adults aged 50–75 years", "Chronic obstructive pulmonary disease among adults aged >=18 years", 
                             "Older adult men aged >=65 years who are up to date on a core set of clinical preventive services: Flu shot past year, PPV shot ever, Colorectal cancer screening", 
                             "Older adult women aged >=65 years who are up to date on a core set of clinical preventive services: Flu shot past year, PPV shot ever, Colorectal cancer screening, and Mammogram past 2 years", 
                             "Current smoking among adults aged >=18 years", "Visits to dentist or dental clinic among adults aged >=18 years", "Depression among adults aged >=18 years", "Diagnosed diabetes among adults aged >=18 years", 
                             "Fair or poor health among adults aged >=18 years", "High cholesterol among adults aged >=18 years who have been screened in the past 5 years", "Chronic kidney disease among adults aged >=18 years", "No leisure-time physical activity among adults aged >=18 years", 
                             "Mammography use among women aged 50–74 years", "Mental health not good for >=14 days among adults aged >=18 years", "Obesity among adults aged >=18 years", "Physical health not good for >=14 days among adults aged >=18 years", "Sleeping less than 7 hours among adults aged >=18 years", 
                             "Stroke among adults aged >=18 years", "All teeth lost among adults aged >=65 years")
new_colnames <- colnames(census_tract_data)[grep("_crudeprev$", colnames(census_tract_data))]
variable_descriptions <- data.frame("Variable" = new_colnames, "Description" = crudeprevs_descriptions)
variable_short <- c("Lack of health insurance", "Arthritis", "Binge Drinking", "High Blood Pressure", "Medicine for High BP", "Cancer", "Asthma", "Cervical Cancer", "Coronary Heart Disease","Routine Checkup","Cholesterol Screening", "Fecal occult blood test, sigmoidoscopy, or colonoscopy","Chronic obstructive pulmonary disease", "Men Up to Date Core","Women Up to Date Core","Current Smoking", "Dental Visits", "Depression", "Diabetes", "Fair / Poor Health", "High Colesterol", "Chronic Kidney Disease", "No leisure time physical activity","Mammography","Poor Mental Health", "Obesity", "Poor Physical Health", "Poor Sleeping", "Stroke", "Teeth Lost")

variable_descriptions <- transform(variable_descriptions, "Short" = variable_short)



# For table displayed on Dashboard, so user can see details of each variable
variable_descriptions_2 <- variable_descriptions[, !colnames(variable_descriptions) %in% "Variable"]
new_order <- c("Short", "Description")
variable_descriptions_2 <- variable_descriptions_2[, new_order]



# Mobile Health Map Data
mhm_data=read_excel("/Users/nehadantuluri/Downloads/MHM-Data.xlsx")


# US only
mhm_filtered <- subset(mhm_data, country == "United States")


# Clinics with location only
mhm_data_filtered <- mhm_filtered[complete.cases(mhm_filtered$latitude, mhm_filtered$longitude), ]


# Change value for Whittier Street Health Center Mobile Health Van, zip code was incorrect
row_to_change <- which(mhm_data_filtered$clinic_name == "Whittier Street Health Center Mobile Health Van")
mhm_data_filtered$zip_postal_code[row_to_change] <- "02120"


# Observations that have NA values for state, but have postal code
na_indices <- which(is.na(mhm_data_filtered$geo_state) & nchar(mhm_data_filtered$zip_postal_code) == 5)


# Add state to state column based on clinics' zip code
zip_to_state <- function(postal_code) {
  return(reverse_zipcode(postal_code)$state)
}

for (index in na_indices) {
  mhm_data_filtered$geo_state[index] <- zip_to_state(mhm_data_filtered$zip_postal_code[index])
}

# State names
state_names = sort(unique(census_tract_data$statedesc))

# Variables for census tract data 
variable_names = sort(variable_descriptions$Short)


# Mobile Health Clinic Categories
clinic_categories = sort(c("Dental", "Mammography", "Other Specialties", "Primary Care", "Behavioral Health", "Sexual and Reproductive Health", "Vision", "Asthma", "Pediatrics", "Preventive", "Maternal and Infant Health"))


# Census data
census_data <- readRDS("/Users/nehadantuluri/Downloads/family_van_acs.rds")


# HRSA Data
hrsa_data <- read_excel("/Users/nehadantuluri/Downloads/HRSA_Health_Centers.xlsx")

# only mobile vans in HRSA 
HRSA_Mobile_Vans <- hrsa_data %>%
  filter(`Health Center Location Type Description` == "Mobile Van")

# HRSA  latitude and longitude columns from geolocation
HRSA_Mobile_Vans$longitude <- HRSA_Mobile_Vans$`Geocoding Artifact Address Primary X Coordinate`
HRSA_Mobile_Vans$latitude <- HRSA_Mobile_Vans$`Geocoding Artifact Address Primary Y Coordinate`

# HRSA = Convert state abbreviations to full names
HRSA_Mobile_Vans$`Site State Abbreviation` <- state.name[match(HRSA_Mobile_Vans$`Site State Abbreviation`, state.abb)]



# UI
ui <- dashboardPage(
  dashboardHeader(title = "Mobile Health Clinic Data"),
  dashboardSidebar(
    selectInput("state_select", "Select State: ", choices = c("Select a State", state.name), selected = "Select a State"),
    selectInput("variable_select", "Select Variable: ",choices = c("Select a Variable", variable_names), selected = "Select a Variable"),
    selectInput("mhclinic_select", "Select Mobile Health Clinic Category: ",choices = c("All Categories", clinic_categories), selected = "All Categories"),
    actionButton("show_vans", "Show HRSA Mobile Vans"),
    actionButton("show_mhm", "Show MHV Clinics"),
    menuItem("Map", tabName = "mapTab", icon = icon("map"))),
  dashboardBody(
    tabItems(
      tabItem(tabName = "mapTab",
              div(style = "height: calc(100vh - 80px)",
                  leafletOutput("state_map", height = "100%"))
      )
    )
  )
)





# Server
server <- function(input, output) {
  # Load the U.S. tract shapefile, starts at MA because whole country cannot be mapped? I think
  us_tracts <- tracts(state="MA", cb=T, year = 2019)
  
  # Create a reactive variable for state_selected
  state_selected <- reactive({
    if (!is.null(input$state_select) && input$state_select != "Select a State") {
      if (input$state_select == "District of Columbia") {
        return("DC")
      } else {
        return(state.abb[match(input$state_select, state.name)])
      }
    } else {
      return(NULL)
    }
  })
  
  
  
  
  # Observe changes in the state selection and update the map accordingly
  observe({
    # Exit this observe block if input$state_select is NULL or an empty string
    if (is.null(input$state_select) || input$state_select == "") {
      return()
    }
    
    # For state map with tracts, no PLACES data yet
    state_selected <- input$state_select
    state_selected_full <- input$state_select
    
    # Filter the data for the selected state
    if (state_selected == "Select a State") {
      state_map_data <- us_tracts
    } else {
      if (state_selected == "District of Columbia") {
        state_selected <- "DC"
      } else {
        state_selected <- state.abb[match(state_selected, state.name)]
      }
      state_map_data <- tracts(state=state_selected, cb=T, year = 2019)
      state_map_data <- st_transform(state_map_data, 4326)
      
    }
    
    ###======================
    
    # For PLACES data layer
    state_census_tract_data <- census_tract_data[census_tract_data$stateabbr == state_selected(), ]
    
    variable_selected <- input$variable_select
    
    if (is.null(variable_selected) || length(variable_selected) == 0 || variable_selected == "Select a Variable") {
      subset_state_variable <- state_census_tract_data[,c("tractfips","access2_crudeprev")]
      colnames(subset_state_variable) <- c("GEOID", "value")
      subset_state_variable$GEOID <- as.character(subset_state_variable$GEOID)
    } else {
      variable_selected <- variable_descriptions$Variable[variable_descriptions$Short == variable_selected]
      subset_state_variable <- state_census_tract_data[,c("tractfips",variable_selected)]
      colnames(subset_state_variable) <- c("GEOID", "value")
      subset_state_variable$GEOID <- as.character(subset_state_variable$GEOID)
    }
    
    # Define state_variable_merged as a reactive variable
    state_variable_merged <- reactive({
      
    state_map_data$GEOID <- as.character(state_map_data$GEOID)
    
    return(dplyr::left_join(state_map_data, subset_state_variable, by = "GEOID"))
  
  })
    

    ###====================== 
    
    # layering CDC, MHM, & HRSA
    
    # Palette
    pal <- colorNumeric("Reds", domain = NULL, na.color = "gray")         
    
    # Pop up
    popup_value_tract <- paste0("Crude Prevalence: ", as.character(state_variable_merged()$value), "\nTract: ", as.character(state_variable_merged()$GEOID))
    
    # For mapping clinics by state
    if (state_selected != "Select a State") {
      mhm_data_filtered <- subset(mhm_data_filtered, toupper(geo_state) %in% c(toupper(state_selected_full), state_selected)) 
       
    }
    
    ###====================== 
    
    observeEvent(input$show_mhm, {
      # For MHM Clinic Locations
      
      # MHM Clinic Category 
      category_selected <- input$mhclinic_select
      variable_selected <- input$variable_select
      
      # Display clinics from category selected, and all clinics if no category is selected
      if (category_selected != "All Categories" && variable_selected != "Select a Variable") {
        mhm_data_filtered <- mhm_data_filtered[grep(category_selected, mhm_data_filtered$general_service_category), ]
        
        # Add markers for the MHM clinics
        leafletProxy("state_map") %>%
          addMarkers(data = mhm_data_filtered, 
                     lng=mhm_data_filtered$longitude, 
                     lat=mhm_data_filtered$latitude, 
                     popup=mhm_data_filtered$clinic_name,
                     icon = ~leaflet::makeIcon(iconUrl = "http://maps.google.com/mapfiles/ms/icons/purple-dot.png"))
      }
    })
    
    ###====================== 
  
    observeEvent(input$show_vans, {
      # Add markers for the HRSA Mobile Vans when the button is clicked
      leafletProxy("state_map") %>%
        addMarkers(data = HRSA_Mobile_Vans, 
                   lng=HRSA_Mobile_Vans$`Geocoding Artifact Address Primary X Coordinate`, 
                   lat=HRSA_Mobile_Vans$`Geocoding Artifact Address Primary Y Coordinate`, 
                   popup=HRSA_Mobile_Vans$`Health Center Location Type Description`,
                   icon = ~leaflet::makeIcon(iconUrl = "http://maps.google.com/mapfiles/ms/icons/green-dot.png"))
    })
    
    

    
    
    # Render the state map with tracts
    output$state_map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(state_census_tract_data$longitude[1], state_census_tract_data$latitude[1], 
                if (state_selected== "DC") {
                  zoom = 10
                } else if (state_selected== "AK") {
                  zoom = 3
                } else if (state_selected %in% c("RI", "NJ", "CT", "NH", "HI", "VT", "DE")) {
                  zoom = 7
                } else {
                  zoom = 5
                } 
        ) %>% 
        addPolygons(data=state_variable_merged(),
                    fillColor = ~pal(state_variable_merged()$value),
                    fillOpacity = .7,
                    weight = .4,
                    smoothFactor = .2,
                    popup =~popup_value_tract
        ) %>%
        addMarkers(data = mhm_data_filtered, 
                   lng=mhm_data_filtered$longitude, 
                   lat=mhm_data_filtered$latitude, 
                   popup=mhm_data_filtered$clinic_name,
                   icon = ~leaflet::makeIcon(iconUrl = "http://maps.google.com/mapfiles/ms/icons/purple-dot.png")) %>% 
        addLegend(pal=pal,
                  values=state_variable_merged()$value,
                  position = "bottomright",
                  title="Crude Prevalence")
       })
    
    })
}  

shinyApp(ui, server)
  