# MOBILE HEALTH MAP DASHBOARD 
# By: Neha Dantuluri & Pia Davis


### Note (3/10/2025) Dashboard has a bunch of bugs! Can select FL. But once I select "arthritis" for ex, it buffers and screen becomes gray. 

##### LOAD LIBS & PACKAGES #####

# install.packages("pacman")
require(pacman)
pacman:: p_load (pacman, dplyr, GGally, ggplot2, ggthemes, ggvis,
                 httr, lubridate, plotly, rio, rmarkdown, shiny,
                 stringr, tidyr, tidyverse, readxl, tidyverse, 
                 shinycssloaders, shinydashboard, leaflet, tigris,
                 sf, zipcodeR)


####### DASHBOARD ######
ui <- dashboardPage(
  dashboardHeader(title = "Mobile Health Clinic Data"),
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
                menuItem("Map", tabName = "map", icon = icon("map")),
                menuItem("Numbers", tabName = "nums", icon = icon("chart-simple")),
                conditionalPanel("input.sidebar == 'map' | input.sidebar == 'nums' ",
                                 selectInput("state_select", "Select State: ",choices = c("Select a State", state_names), selected = "Select a State"),
                                 selectInput("variable_select", "Select Variable: ",choices = c("Select a Variable", variable_names), selected = "Select a Variable")
                ),
                conditionalPanel("input.sidebar == 'map'",
                                 checkboxInput("show_mhm", "Show MHV Clinics", value = FALSE),
                                 conditionalPanel(
                                   condition = "input.show_mhm > 0",
                                   checkboxGroupInput("mhclinic_select", "Select Clinic Category: ", choices = c("All Categories", clinic_categories), selected = "")
                                 ),
                                 checkboxInput("show_vans", "Show HRSA Mobile Vans", value = FALSE)
                )
    )
  ),
  
  
  dashboardBody(
    tabItems(
      # First Tab Item - Map
      tabItem( tabName = "map",
               fluidRow(
                 box(withSpinner(leafletOutput("state_map")), width = 12)),
               br(),
               br(),
               fluidRow(
                 conditionalPanel(
                   condition = "input.variable_select != 'Select a Variable'", 
                   # box(title = "Crude Prevalence by Tract", solidHeader = TRUE, dataTableOutput("crudeprev_desc"), width = 12, status = "primary"),
                   withSpinner(uiOutput("placeholder"))
                 ),
                 box(title = "Variable Descriptions", dataTableOutput("v_descriptions"), width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE)  
               )
      ),
      tabItem(tabName = "nums",
              box(
                title = "Distribution", solidHeader = T, 
                sliderInput("slider1", "Slider input:", 0, 50, value = 1, step = 5),
                width = 12,
                plotOutput("hist")
              )
      )
    )
  )
)



server <- function(input, output) {
  # Load the U.S. tract shapefile, starts at MA because whole country cannot be mapped? I think
  us_tracts <- states()
  
  # Reactive value to check if "Select a State" is chosen
  selected_state <- reactive({
    input$state_select
  })
  
  # Observe changes in the state selection and update the map accordingly
  observe({
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
      if (state_selected == "AK" | state_selected == "HI") {
        state_map_data <- tracts(state=state_selected, cb=T, year = 2019)
      } else {
        state_map_data <- tracts(state=state_selected, cb=T, year = 2015)
      }
    }
    
    # For PLACES data layer
    if (state_selected != "Select a State") {
      sdoh_data_filtered <- SDOH_CENSUSDATA[SDOH_CENSUSDATA$StateAbbr == state_selected, ]
    } else {
      sdoh_data_filtered <- SDOH_CENSUSDATA
    }
    
    variable_selected <- input$variable_select
    
    if (variable_selected == "Select a Variable") {
      subset_state_variable <- sdoh_data_filtered[, c("LocationID", "CountyName", "StateAbbr", "Data_Value")]
    } else {
      variable_selected <- variable_descriptions$Variable[variable_descriptions$Short == variable_selected]
      subset_state_variable <- sdoh_data_filtered[, c("LocationID", "CountyName", "StateAbbr", variable_selected)]
    }
    
    colnames(subset_state_variable) <- c("Tract", "County", "State", "Value")
    crudeprev_dt <- subset_state_variable
    
    subset_state_variable <- subset_state_variable[, c("Tract", "Value")]
    colnames(subset_state_variable) <- c("GEOID", "value")
    
    state_variable_merged <- geo_join(state_map_data, subset_state_variable, "GEOID", "GEOID")
    
    pal <- colorNumeric("Reds", domain = NULL, na.color = "gray")
    
    popup_value_tract <- reactive ({
      paste0("Value: ", as.character(state_variable_merged$value), "\nTract: ", as.character(state_variable_merged$GEOID))
    })
    
    popup_value_tract2 <- reactive ({
      paste0("Tract: ", as.character(state_variable_merged$GEOID))
    })
    
    output$state_map <- renderLeaflet({
      if (selected_state() == "Select a State") {
        state_map_data %>%
          leaflet() %>%
          addTiles() %>%
          addPolygons(weight = .5, fillOpacity = 0.3, popup = ~NAME) 
      } else {
        if (variable_selected == "Select a Variable") {
          state_map_data %>%
            leaflet() %>%
            addProviderTiles("CartoDB.Positron") %>%
            setView(sdoh_data_filtered$longitude[1], sdoh_data_filtered$latitude[1], 
                    if (state_selected == "DC") {
                      zoom = 10
                    } else if (state_selected == "AK") {
                      zoom = 3
                    } else if (state_selected %in% c("RI", "NJ", "CT", "NH", "HI", "VT", "DE")) {
                      zoom = 6
                    } else {
                      zoom = 5
                    }
            ) %>% 
            addPolygons(data = state_variable_merged, weight = .3, fillOpacity = 0.3, popup = ~popup_value_tract2()) 
        } else {
          state_map_data %>%
            leaflet() %>%
            addProviderTiles("CartoDB.Positron") %>%
            setView(sdoh_data_filtered$longitude[1], sdoh_data_filtered$latitude[1], 
                    if (state_selected == "DC") {
                      zoom = 10
                    } else if (state_selected == "AK") {
                      zoom = 3
                    } else if (state_selected %in% c("RI", "NJ", "CT", "NH", "HI", "VT", "DE")) {
                      zoom = 6
                    } else {
                      zoom = 5
                    }
            ) %>% 
            addPolygons(data = state_variable_merged, fillColor = ~pal(state_variable_merged$value), fillOpacity = .7, weight = .3, smoothFactor = .2, popup = ~popup_value_tract()) %>%
            addLegend(pal = pal, values = state_variable_merged$value, position = "bottomright", title = "Value")
        }
      }
    })
    
    observeEvent(input$show_mhm, { 
      if (input$show_mhm == FALSE | length(input$mhclinic_select) == 0) {
        all_clinic_names <- MHV2023_USLOCATIONS$clinic_name
        lapply(all_clinic_names, function(clinic_name) {
          leafletProxy("state_map") %>%
            removeMarker(layerId = clinic_name)
        }) 
      } else {
        leafletProxy("state_map") %>% 
          addMarkers(data = mhm_data_display, lng = mhm_data_display$longitude, lat = mhm_data_display$latitude, layerId = mhm_data_display$clinic_name, popup = ~paste("<b>", mhm_data_display$clinic_name, "</b>", "<br>", "Services: ", mhm_data_display$general_service_category))
      }
    })
    
    observeEvent(input$show_vans, {
      if (input$show_vans == FALSE) {
        all_hrsa_clinics <- HRSA2024$`Site Name`
        lapply(all_hrsa_clinics, function(`Site Name`) {
          leafletProxy("state_map") %>%
            removeMarker(layerId = `Site Name`)
        }) 
      } else {
        leafletProxy("state_map") %>%
          addMarkers(data = HRSA2024, lng = HRSA2024$longitude, lat = HRSA2024$latitude, layerId = HRSA2024$`Site Name`, popup = ~paste("<b>", str_to_title(HRSA2024$`Health Center Name`), "</b>", "<br>", "Website: ", HRSA2024$`Site Web Address`))
      }
    })
    
    output$placeholder <- renderUI({
      req(input$variable_select)
      box(title = paste0(input$variable_select, " Value by Tract"), solidHeader = TRUE, dataTableOutput("crudeprev_desc"), width = 12, status = "primary")
    })
    
    crudeprev_descending <- crudeprev_dt
    crudeprev_descending <- crudeprev_descending[order(-crudeprev_descending$Value), ]
    crudeprev_descending <- rownames_to_column(crudeprev_descending, "#")
    
    if (input$variable_select != "Select a Variable") {
      output$crudeprev_desc <- renderDataTable(crudeprev_descending)
    }
    
    output$v_descriptions <- renderDataTable(variable_descriptions_2)
    
    percent <- as.integer(input$slider1) / 100
    percent2 <- ceiling(percent * length(crudeprev_dt))
    first_n_percent <- crudeprev_dt[1:percent2, ]
    
    output$hist <- renderPlot(ggplot(first_n_percent) + geom_bar(mapping = aes(x = State)))
    
    
  })
  
  
}

shinyApp(ui, server)

