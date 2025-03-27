#__________________________________
# 1. INSTALL & LOAD LIBRARIES #####
#__________________________________
if (!require(shiny)) install.packages("shiny", dependencies=TRUE)
if (!require(shinythemes)) install.packages("shinythemes", dependencies=TRUE)
if (!require(leaflet)) install.packages("leaflet", dependencies=TRUE)
if (!require(sf)) install.packages("sf", dependencies=TRUE)
if (!require(readr)) install.packages("readr", dependencies=TRUE)
if (!require(tidyverse)) install.packages("tidyverse", dependencies=TRUE)
if (!require(plotly)) install.packages("plotly", dependencies=TRUE)
if (!require(scales)) install.packages("scales", dependencies=TRUE)
if (!require(prophet)) install.packages("prophet", dependencies=TRUE)
if (!require(reshape2)) install.packages("reshape2", dependencies=TRUE)
if (!require(reactable)) install.packages("reactable", dependencies=TRUE)
# Core Shiny Framework
library(shiny)             # Core package for building Shiny web applications
library(shinythemes)       # Provides pre-built Bootstrap themes for Shiny apps

# Spatial Visualization
library(leaflet)           # For creating interactive maps with markers/polygons
library(sf)                # Simple Features for working with spatial vector data (shapefiles)

# Data Manipulation
library(dplyr)             # Essential for data wrangling (filter, mutate, summarize, etc.)
library(tidyverse)         # Meta-package loading core tidyverse packages (dplyr, ggplot2, etc.)
library(readr)             # Fast and consistent way to read rectangular data (CSV, TSV)
library(tidyr)             # For data tidying (pivot_longer, pivot_wider, etc.)

# Visualization
library(ggplot2)           # Grammar of graphics for creating static visualizations
library(plotly)            # Converts ggplot2 graphs to interactive plots with hover tools
library(scales)            # Provides methods for automatic axis scaling and labelling

# Forecasting
library(prophet)         # Facebook's forecasting tool 

# Data Reshaping
library(reshape2)          # For converting between wide and long data formats (melt/dcast)

# Interactive Tables
library(reactable)         # Creates interactive, feature-rich data tables with sorting/filtering


#__________________________________
# 2. LOAD DATA                #####
#__________________________________
# Hospital Locations
hospital_location <- read_csv("data_clean/clean_hospital_location.csv")

# Total COVID cases
total_cases_by_local_authority <- read_csv("data_clean/clean_total_cases_by_local_authority.csv")
total_cases_by_age_sex <- read_csv("data_clean/clean_total_cases_by_age_sex.csv")
# total_cases_by_deprivation <- read_csv("data_clean/clean_total_cases_by_deprivation.csv")
# ethnicity <- read_csv("data_clean/clean_ethnicity.csv") # Monthly hospital Admissions

# Daily COVID trends
daily_case_trends_by_local_authority <- read_csv("data_clean/clean_daily_case_trends_by_local_authority.csv")
daily_case_trends_by_age_sex <- read_csv("data_clean/clean_daily_case_trends_by_age_sex.csv")
# daily_case_trends_by_deprivation <- read_csv("data_clean/clean_daily_case_trends_by_deprivation.csv")
# daily_covid_hospital_admissions <- read_csv("data_clean/clean_daily_covid_hospital_admissions.csv")

# Tests
tests_by_local_authority <- read_csv("data_clean/clean_tests_by_local_authority.csv")

# Population Data
ca_pop_est <- read_csv("data_clean/clean_ca_pop_est.csv")
scotland_pop_proj <- read_csv("data_clean/clean_scotland_pop_proj.csv")
# ca_pop_proj <- read_csv("data_clean/clean_ca_pop_proj.csv")

# Load Scotland spatialdata
# uk_shapefile <- st_read("data_raw/scotland_spatial_data/LAD_DEC_24_UK_BFC.shp") # Scotland data was extracted and put into a geojson
scotland_sf <- st_read("data_clean/scotland_LAD.geojson")
council_area_2019 <- read_csv("data_clean/clean_council_area_2019.csv")

# checking the geojson file
# print(scotland_sf)  # View metadata
# plot(st_geometry(scotland_sf))  # Quick plot


#__________________________________
# 3. WRANGLE DATA             #####
#__________________________________
# List of Scottish council area codes
scotland_codes <- c(
  "S12000005", "S12000006", "S12000008", "S12000010", "S12000011", "S12000011", 
  "S12000013", "S12000014", "S12000015", "S12000017", "S12000018", "S12000018", 
  "S12000019", "S12000020", "S12000021", "S12000023", "S12000024", "S12000026", 
  "S12000027", "S12000028", "S12000029", "S12000029", "S12000030", "S12000033", 
  "S12000034", "S12000035", "S12000036", "S12000038", "S12000038", "S12000039", 
  "S12000039", "S12000040", "S12000041", "S12000041", "S12000042", "S12000042", 
  "S12000044", "S12000045", "S12000045", "S12000046", "S12000047", "S12000048", 
  "S12000049", "S12000050"
)

# # Filter UK Shape file to just Scotland Council Areas
# scotland_sf <- uk_shapefile[uk_shapefile$LAD24CD %in% scotland_codes, ] # no longer needed as scotland shapefiles were extracted in `data_cleaning.r`

scotland_data <- merge(scotland_sf, council_area_2019, by.x = "LAD24CD", by.y = "ca")

# Transform the CRS to WGS84 (EPSG:4326)
scotland_data <- st_transform(scotland_data, crs = 4326)

hospital_location$lng <- as.numeric(hospital_location$lng)
hospital_location$lat <- as.numeric(hospital_location$lat)

hospital_coords <- hospital_location %>%
  select(id,
         location_name,
         postcode,
         hb,
         hscp,
         ca,
         int_zone,
         data_zone,
         lng,
         lat)

# Remove hospitals with NA values
hospital_coords <- hospital_coords %>%
  filter(!location_name %in% c("state_hospital",
                               "golden_jubilee_national_hospital",
                               "royal_hospital_for_children_and_young_people")
  )

# Create geometry column from lng and lat
hospital_coords <- hospital_coords %>%
  st_as_sf(coords = c("lng", "lat"), crs = 27700) # 27700 is British National Grid

# Transform to WGS84 (EPSG:4326)
hospital_coords <- st_transform(hospital_coords, crs = 4326)

# Merge test and pop. data with council data
scotland_merged <- scotland_data %>%
  left_join(ca_pop_est, by = c("LAD24CD" = "ca"))

# Merge test data with council area data
scotland_merged <- scotland_merged %>%
  left_join(tests_by_local_authority, by = c("LAD24CD" = "ca"))

# Extract LAD24CD and LAD24NM from scotland_merged as a regular data frame
council_names <- st_drop_geometry(scotland_merged[, c("LAD24CD", "LAD24NM")]) %>% 
  distinct(LAD24CD, .keep_all = TRUE)

# Join hospital_coords with council_names to get LAD24NM
hospital_coords <- hospital_coords %>%
  left_join(council_names, by = c("ca" = "LAD24CD"))

# National Benchmarks
total_hospitals <- nrow(hospital_coords)
total_population <- scotland_pop_proj %>%
  filter(year == 2023 & sex == "all") %>%
  summarise(total_pop = sum(all_ages, na.rm = TRUE)) %>%
  pull(total_pop)

# Density 
council_areas <- tribble(
  ~LAD24NM, ~area,
  "Aberdeen City", 182,
  "Aberdeenshire", 6317,
  "Angus", 2184,
  "Argyll and Bute", 7023,
  "Clackmannanshire", 158,
  "Dumfries and Galloway", 6446,
  "Dundee City", 55,
  "East Ayrshire", 1275,
  "East Dunbartonshire", 176,
  "East Lothian", 666,
  "East Renfrewshire", 168,
  "City of Edinburgh", 260,
  "Falkirk", 293,
  "Fife", 1340,
  "Glasgow City", 175,
  "Highland", 26484,
  "Inverclyde", 167,
  "Midlothian", 350,
  "Moray", 2237,
  "North Ayrshire", 888,
  "North Lanarkshire", 476,
  "Na h-Eileanan Siar", 3070,
  "Orkney Islands", 1025,
  "Perth and Kinross", 5395,
  "Renfrewshire", 263,
  "Scottish Borders", 4727,
  "Shetland Islands", 1471,
  "South Ayrshire", 1230,
  "South Lanarkshire", 1778,
  "Stirling", 2243,
  "West Dunbartonshire", 176,
  "West Lothian", 427
)

population_2023 <- ca_pop_est %>%
    filter(year == 2023) %>% 
  select(year, ca, sex, all_ages)



population_density_2023 <- population_2023 %>%
  left_join(council_names, by = c("ca" = "LAD24CD")) %>%
  left_join(council_areas, by = "LAD24NM") %>%
  mutate(LAD24NM = ifelse(ca == "S92000003",
                          "Scotland",
                          LAD24NM),
         area = ifelse(ca == "S92000003",
                       30409,
                       area)
         ) %>%
  mutate(density = ifelse(!is.na(area),
                          all_ages / area,
                          NA)
  ) %>% 
    rename(
    population_2023 = all_ages,
    council_area_name = LAD24NM
  )


# National benchmarks
national_hospitals_per_capita <- nrow(hospital_coords)/sum(ca_pop_est$all_ages[ca_pop_est$year == 2023 & ca_pop_est$sex == "all"], na.rm = TRUE)
national_pop_per_hospital <- sum(ca_pop_est$all_ages[ca_pop_est$year == 2023 & ca_pop_est$sex == "all"], na.rm = TRUE)/nrow(hospital_coords)
national_density <- sum(ca_pop_est$all_ages[ca_pop_est$year == 2023 & ca_pop_est$sex == "all"], na.rm = TRUE)/sum(council_areas$area, na.rm = TRUE)

#__________________________________
# 4. SHINY SERVER             #####
#__________________________________
server <- function(input, output, session) {
  # Reactive value to store the selected area
  selected_area <- reactiveVal(NULL)
  
  # Hospital icon
  hospital_icon <- makeIcon(
    iconUrl = "assets/hospital.png", 
    iconWidth = 24, iconHeight = 24
  )
  
  # Toggle for population density
  show_density <- reactiveVal(FALSE)
  
  # Filtered hospitals reactive expression
  hospitals_in_area <- reactive({
    req(selected_area())
    hospitals <- hospital_coords %>% 
      filter(LAD24NM == selected_area())
    
    # Ensure we have valid data
    req(nrow(hospitals) > 0)
    
    # Convert to sf and validate geometry
    hospitals <- st_as_sf(hospitals)
    req(all(c("geometry") %in% names(hospitals)))
    
    hospitals
  })
  
  # Population data reactive expressions
  pop_2023 <- reactive({
    req(selected_area())
    ca_pop_est %>%
      filter(ca == hospitals_in_area()$ca[1] & year == 2023 & sex == "all") %>%
      pull(all_ages)
  })
  
  pop_2018 <- reactive({
    req(selected_area())
    ca_pop_est %>%
      filter(ca == hospitals_in_area()$ca[1] & year == 2018 & sex == "all") %>%
      pull(all_ages)
  })
  
  # Population change reactive
  pop_change <- reactive({
    req(pop_2018(), pop_2023())
    pop_2023() - pop_2018()
  })
  
  pop_change_pct <- reactive({
    req(pop_2018(), pop_change())
    round(pop_change()/pop_2018()*100, 1)
  })
  
  # Test data reactive
  test_data <- reactive({
    req(selected_area())
    tests_by_local_authority %>%
      filter(ca == hospitals_in_area()$ca[1])
  })
  
  # Calculate metrics reactively
  num_hospitals <- reactive({
    nrow(hospitals_in_area())
  })
  
  hospitals_per_capita <- reactive({
    if (!is.na(pop_2023()) && pop_2023() > 0) {
      num_hospitals() / pop_2023()
    } else {
      NA
    }
  })
  
  pop_per_hospital <- reactive({
    if (!is.na(pop_2023()) && pop_2023() > 0) {
      pop_2023() / num_hospitals()
    } else {
      NA
    }
  })
  
  total_tests <- reactive({
    req(test_data())
    test_data()$total_tests
  })
  
  positive_tests <- reactive({
    req(test_data())
    test_data()$positive_tests
  })
  
  negative_tests <- reactive({
    req(total_tests(), positive_tests())
    total_tests() - positive_tests()
  })
  
  # Statistical calculations
  positivity_rate <- reactive({
    req(total_tests(), positive_tests())
    positive_tests() / total_tests()
  })
  
  national_positivity_rate <- reactive({
    sum(tests_by_local_authority$positive_tests) / sum(tests_by_local_authority$total_tests)
  })
  
  z_score <- reactive({
    req(positivity_rate(), national_positivity_rate(), total_tests())
    (positivity_rate() - national_positivity_rate()) / 
      sqrt((national_positivity_rate() * (1 - national_positivity_rate())) / total_tests())
  })
  
  cagr <- reactive({
    req(pop_2018(), pop_2023())
    ((pop_2023() / pop_2018())^(1/5) - 1) * 100
  })
  
  # Population density calculation
  population_density <- reactive({
    req(selected_area())
    density_data <- population_density_2023 %>%
      filter(sex == "all",
             council_area_name == selected_area())
    
    if(nrow(density_data) > 0) {
      list(
        density = as.numeric(density_data$density),
        area = as.numeric(density_data$area),
        population = as.numeric(density_data$population_2023)
      )
    } else {
      NULL
    }
  })
  
  # Render the map
  output$map <- renderLeaflet({
    leaflet(scotland_data) %>%
      addProviderTiles("CartoDB.Positron") %>% 
      addPolygons(
        fillColor = "#1883D3", 
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.3, 
        highlight = highlightOptions(
          weight = 2, 
          color = "#40E0D0", 
          opacity = 0.5, 
          dashArray = "",
          fillOpacity = 0.7, 
          bringToFront = TRUE
        ),
        label = ~LAD24NM, 
        layerId = ~LAD24NM, 
        labelOptions = labelOptions(noHide = FALSE, direction = "auto")
      ) %>%
      fitBounds(-8.8, 54.5, -0.6, 60.9)
  })
  
  # Observe map clicks to show/hide hospital markers
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    selected_area(click$id)
    
    hospitals <- hospitals_in_area()
    req(nrow(hospitals) > 0)  # Ensure we have hospitals to display
    
    # Extract coordinates safely
    coords <- tryCatch({
      st_coordinates(hospitals)
    }, error = function(e) {
      message("Error extracting coordinates: ", e$message)
      return(NULL)
    })
    
    req(coords)  # Only proceed if we have valid coordinates
    
    leafletProxy("map", session) %>%
      clearGroup("hospitals") %>%
      addMarkers(
        lng = coords[,1],
        lat = coords[,2],
        icon = hospital_icon,
        group = "hospitals",
        label = hospitals$location_name,
        clusterOptions = markerClusterOptions()
      )
  })
  
  # Area information UI
  output$area_info <- renderUI({
    if (is.null(selected_area())) {
      return(HTML("<div class='well info-panel'>Click on a council area to see detailed information.</div>"))
    }
    
    req(
      area_name <- selected_area(),
      density_info <- population_density(),
      hospitals <- hospitals_in_area(),
      tests <- test_data(),
      pop23 <- pop_2023(),
      pop18 <- pop_2018(),
      change <- pop_change(),
      change_pct <- pop_change_pct(),
      n_hospitals <- num_hospitals(),
      hosp_per_cap <- hospitals_per_capita(),
      pop_per_hosp <- pop_per_hospital(),
      pos_tests <- positive_tests(),
      total_tests <- total_tests(),
      pos_rate <- positivity_rate(),
      national_pos_rate <- national_positivity_rate(),
      z_val <- z_score()
    )
    
    # Safely extract density values with error handling
    density_value <- tryCatch({
      as.numeric(density_info$density)
    }, error = function(e) {
      NA_real_
      })
    
    area_value <- tryCatch({
      as.numeric(density_info$area)
    }, error = function(e) {
      NA_real_
      })
    
    population_value <- tryCatch({
      as.numeric(density_info$population)
    }, error = function(e) {
      NA_real_
      })
    
    # Calculate national averages
    scotland_density <- population_density_2023 %>%
      filter(ca == "S92000003", sex == "all") %>%
      pull(density) %>%
      as.numeric() %>%
      round(1)
    
    # Only proceed if we have valid density data
    req(!is.na(density_value), !is.na(area_value))
    
    # Calculate metrics with units
    hosp_per_100k <- round(hosp_per_cap * 100000, 1)
    national_hosp_per_100k <- round(national_hospitals_per_capita * 100000, 1)
    national_pop_per_hosp <- round(national_pop_per_hospital)
    
    # Determine comparison colors
    hosp_color <- ifelse(hosp_per_100k >= national_hosp_per_100k, "comparison-good", "comparison-bad")
    pop_hosp_color <- ifelse(pop_per_hosp <= national_pop_per_hosp, "comparison-good", "comparison-bad")
    density_color <- ifelse(density_value <= scotland_density, "comparison-good", "comparison-bad")
    
    # Testing metrics
    neg_tests <- total_tests - pos_tests
    pos_pct <- round(pos_rate * 100, 1)
    neg_pct <- round(neg_tests/total_tests * 100, 1)
    national_pos_pct <- round(national_pos_rate * 100, 1)
    pos_test_color <- ifelse(pos_pct >= national_pos_pct, "comparison-bad", "comparison-good")
    
    tagList(
      h3(paste("Council Area:", area_name)),
      hr(),
      
      # First Row: Population Data
      fluidRow(
        column(6,
               wellPanel(
                 h4("Population Change (2018-2023)"),
                 tags$table(
                   class = "table",
                   tags$tr(tags$th("Year"), tags$th("Population")),
                   tags$tr(tags$td("2018"), tags$td(comma(pop18), "people")),
                   tags$tr(tags$td("2023"), tags$td(comma(pop23), "people")))
               )
        ),
        column(6,
               wellPanel(
                 h4("Change Summary"),
                 div(class = ifelse(change >= 0, "positive-change", "negative-change"),
                     paste0(ifelse(change >= 0, "+", ""), comma(change), 
                            " people (", change_pct, "%)")),
                 p(paste("Annual growth rate:", round(cagr(), 2), "%"))
               )
        )
      ),
      
      # Second Row: Healthcare Metrics
      fluidRow(
        column(6,
               wellPanel(
                 h4("Healthcare Resources"),
                 tags$table(
                   class = "table",
                   tags$tr(
                     tags$td("Number of Hospitals"),
                     tags$td(n_hospitals)
                   ),
                   tags$tr(
                     tags$td("Hospitals per 100k people"),
                     tags$td(
                       span(paste(round(hosp_per_100k, 1)), class = hosp_color),
                       paste0(" (National: ", round(national_hosp_per_100k, 1), ")")
                     )
                   )
                 )
               )
        ),
        column(6,
               wellPanel(
                 h4("Healthcare Coverage"),
                 tags$table(
                   class = "table",
                   tags$tr(
                     tags$td("Area"),
                     tags$td(paste(
                       ifelse(is.na(area_value), "N/A", format(round(area_value, 1), nsmall = 1)),
                       " km²"
                     ))
                   ),
                   tags$tr(
                     tags$td("Population Density"),
                     tags$td(
                       span(
                         paste(
                           ifelse(is.na(density_value), "N/A", format(round(density_value, 1), nsmall = 1)),
                           "p/km²"
                         ), 
                         class = density_color
                       ),
                       paste0(" (Scotland: ", scotland_density, " p/km²)")
                     )
                   ),
                   tags$tr(
                     tags$td("People per Hospital"),
                     tags$td(
                       span(
                         paste(
                           ifelse(is.na(pop_per_hosp), "N/A", comma(round(pop_per_hosp))),
                           " people"
                         ),
                         class = pop_hosp_color
                       ),
                       paste0(" (National: ", comma(round(national_pop_per_hosp)), " people)")
                     )
                   )
                 )
               )
        )
      ),
      
      # Third Row: Testing Data
      fluidRow(
        column(12,
               wellPanel(
                 h4("COVID-19 Testing Data"),
                 fluidRow(
                   column(4,
                          tags$table(
                            class = "table",
                            tags$tr(
                              tags$td("Total Tests"),
                              tags$td(comma(total_tests)))
                          ),
                          tags$tr(
                            tags$td("Positive Tests"),
                            tags$td(
                              span(paste0(comma(pos_tests), " (", pos_pct, "%)"), 
                                   class = pos_test_color))
                          ),
                          tags$tr(
                            tags$td("Negative Tests"),
                            tags$td(paste0(comma(neg_tests), " (", neg_pct, "%)"))
                          )
                   )
                 ),
                 column(8,
                        div(class = "zscore-explanation",
                            h5("Statistical Significance Analysis"),
                            p(paste("Positivity Rate:", pos_pct, "% (National:", national_pos_pct, "%)")),
                            p(paste("Z-score:", round(z_val, 2))),
                            p(span(ifelse(abs(z_val) > 1.96, 
                                          "Statistically Significant", 
                                          "Not Statistically Significant"), 
                                   style = paste0("color:", ifelse(abs(z_val) > 1.96, "#e74c3c", "#2ecc71")))),
                            p("A Z-score > |1.96| indicates a statistically significant difference from national average")
                        )
                 )
               )
        )
      )
    )
  })

# Reset view to Scotland
observeEvent(input$reset_view, {
  leafletProxy("map", session) %>%
    fitBounds(-8.8, 54.5, -0.6, 60.9) %>%
    clearGroup("hospitals")
  selected_area(NULL)
})

# Toggle population density
observeEvent(input$toggle_density, {
  show_density(!show_density())
  
  map_proxy <- leafletProxy("map", session)
  
  if (show_density()) {
    # Join density data with spatial data
    density_data <- scotland_data %>%
      left_join(
        population_density_2023 %>% 
          filter(sex == "all") %>%
          select(council_area_name, density, population_2023, area),
        by = c("LAD24NM" = "council_area_name")
      )
    
    # Scotland's average density
    scotland_density <- population_density_2023 %>%
      filter(ca == "S92000003", sex == "all") %>%
      pull(density) %>%
      round(1)
    
    # Create color palette
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = density_data$density,
      na.color = "#808080"
    )
    
    map_proxy %>% clearGroup("density")
    
    # Add density layer
    map_proxy  %>%
      addPolygons(
        data = density_data,
        fillColor = ~pal(density),
        fillOpacity = 0.7,
        weight = 1,
        color = "white",
        group = "density",
        label = ~paste0(
          LAD24NM, "\n",
          "Area: ", round(area, 1), " km²\n",
          "Density: ", round(density, 1), " p/km²\n",
          "Population: ", comma(population_2023)
        )
      )  %>%
      removeControl(layerId = "density_legend") %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = density_data$density,
        title = paste0("Population Density\n(Scotland: ", scotland_density, " p/km²)"),
        group = "density",
        labFormat = labelFormat(suffix = " p/km²"),
        opacity = 0.7
      )
  } else {
    # Remove density layer
    map_proxy %>%
      clearGroup("density") %>%
      removeControl(layerId = "density_legend")
  }
})

# Reactive data for time series analysis
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  tags$head(
    tags$style(HTML("
      /* Your existing CSS styles */
    "))
  ),
  titlePanel("Scotland COVID-19 Analysis"),
  tabsetPanel(
    id = "mainTabs",
    tabPanel("Interactive Map",
             # Your existing map code
    ),
    tabPanel("Time Series Analysis",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 selectInput("ts_dataset", "Select Dataset:",
                             choices = c("By Local Authority" = "local_authority",
                                         "By Age/Sex" = "age_sex")),
                 conditionalPanel(
                   condition = "input.ts_dataset == 'local_authority'",
                   selectizeInput("ts_area", "Select Council Area(s):",
                                  choices = c("Scotland Total", unique(daily_case_trends_by_local_authority$ca_name)),
                                  multiple = TRUE,
                                  selected = "Scotland Total")
                 ),
                 conditionalPanel(
                   condition = "input.ts_dataset == 'age_sex'",
                   selectInput("ts_age_group", "Select Age Group:",
                               choices = unique(daily_case_trends_by_age_sex$age_group)),
                   selectInput("ts_sex", "Select Sex:",
                               choices = unique(na.omit(daily_case_trends_by_age_sex$sex)))
                 ),
                 dateRangeInput("ts_date_range", "Date Range:",
                                start = as.Date("2020-03-01"),
                                end = as.Date("2023-12-31"),
                                min = as.Date("2020-02-28"), # First date in your data
                                max = as.Date("2023-12-31")),
                 selectInput("ts_metric", "Select Metric:",
                             choices = c("Daily Positive Cases" = "daily_positive",
                                         "Cumulative Positive Cases" = "cumulative_positive",
                                         "Daily Deaths" = "daily_deaths",
                                         "Positive Tests" = "positive_tests",
                                         "Total Tests" = "total_tests")),
                 checkboxInput("ts_log_scale", "Logarithmic Scale", value = FALSE),
                 actionButton("ts_update", "Update Analysis", class = "btn-primary")
               ),
               mainPanel(
                 width = 9,
                 tabsetPanel(
                   tabPanel("Trends",
                            plotlyOutput("ts_plot", height = "500px"),
                            br(),
                            fluidRow(
                              column(6,
                                     wellPanel(
                                       h4("Peak Analysis"),
                                       tableOutput("ts_peaks")
                                     )
                              ),
                              column(6,
                                     wellPanel(
                                       h4("Summary Statistics"),
                                       verbatimTextOutput("ts_summary")
                                     )
                              )
                            )
                   ),
                   tabPanel("Comparisons",
                            plotlyOutput("ts_comparison_plot", height = "500px"),
                            br(),
                            h4("Weekly Averages"),
                            plotlyOutput("ts_weekly_plot")
                   )
                 )
               )
             )
    )
  )
)

  # Reactive data for time series analysis
  ts_data <- reactive({
    req(input$ts_update)
    
    isolate({
      if (input$ts_dataset == "local_authority") {
        df <- daily_case_trends_by_local_authority
        
        # Handle Scotland Total
        if ("Scotland Total" %in% input$ts_area) {
          scotland_total <- df %>%
            group_by(date) %>%
            summarise(
              daily_positive = sum(daily_positive, na.rm = TRUE),
              cumulative_positive = sum(cumulative_positive, na.rm = TRUE),
              daily_deaths = sum(daily_deaths, na.rm = TRUE),
              positive_tests = sum(positive_tests, na.rm = TRUE),
              total_tests = sum(total_tests, na.rm = TRUE)
            ) %>%
            mutate(ca_name = "Scotland Total")
          
          if (length(input$ts_area) > 1) {
            df <- bind_rows(df %>% filter(ca_name %in% input$ts_area), scotland_total)
          } else {
            df <- scotland_total
          }
        } else {
          df <- df %>% filter(ca_name %in% input$ts_area)
        }
        
        # Select the metric
        df <- df %>%
          mutate(value = case_when(
            input$ts_metric == "daily_positive" ~ daily_positive,
            input$ts_metric == "cumulative_positive" ~ cumulative_positive,
            input$ts_metric == "daily_deaths" ~ daily_deaths,
            input$ts_metric == "positive_tests" ~ positive_tests,
            input$ts_metric == "total_tests" ~ total_tests
          )) %>%
          select(date, area_name = ca_name, value)
        
      } else { # age_sex dataset
        df <- daily_case_trends_by_age_sex %>%
          filter(age_group == input$ts_age_group,
                 sex == input$ts_sex) %>%
          mutate(value = case_when(
            input$ts_metric == "daily_positive" ~ daily_positive,
            input$ts_metric == "cumulative_positive" ~ cumulative_positive,
            input$ts_metric == "daily_deaths" ~ daily_deaths,
            TRUE ~ NA_real_
          )) %>%
          select(date, area_name = age_group, value, sex)
      }
      
      # Apply date range filter
      df <- df %>% 
        filter(date >= input$ts_date_range[1] & date <= input$ts_date_range[2])
      
      df
    })
  })
  
  # Time series plot
  output$ts_plot <- renderPlotly({
    df <- ts_data()
    if (nrow(df) == 0) return(NULL)
    
    # Determine the metric name for the y-axis
    metric_name <- case_when(
      input$ts_metric == "daily_positive" ~ "Daily Positive Cases",
      input$ts_metric == "cumulative_positive" ~ "Cumulative Positive Cases",
      input$ts_metric == "daily_deaths" ~ "Daily Deaths",
      input$ts_metric == "positive_tests" ~ "Positive Tests",
      input$ts_metric == "total_tests" ~ "Total Tests"
    )
    
    if (input$ts_dataset == "local_authority") {
      p <- ggplot(df, aes(x = date, y = value, color = area_name)) +
        geom_line() +
        labs(title = paste(metric_name, "by Local Authority"),
             x = "Date", y = metric_name,
             color = "Council Area")
    } else {
      p <- ggplot(df, aes(x = date, y = value, color = sex)) +
        geom_line() +
        labs(title = paste(metric_name, "for", input$ts_age_group),
             x = "Date", y = metric_name,
             color = "Sex")
    }
    
    if (input$ts_log_scale) {
      p <- p + scale_y_log10()
    }
    
    ggplotly(p) %>% layout(hovermode = "x unified")
  })
  
  # Peak analysis
  output$ts_peaks <- renderTable({
    df <- ts_data()
    if (nrow(df) == 0) return(NULL)
    
    if (input$ts_dataset == "local_authority") {
      df %>%
        group_by(area_name) %>%
        filter(value == max(value,
                            na.rm = TRUE)) %>%
        mutate(Date = format(date, "%Y-%m-%d"), 
               Peak = round(value, 1)) %>% 
        select(Area = area_name,
               Date = date,
               Peak = value) %>%
        arrange(desc(Peak)) %>% 
        distinct(Area, Peak, .keep_all = TRUE) 
    } else {
      df %>%
        filter(value == max(value,
                            na.rm = TRUE)) %>%
        select(Age_Group = area_name,
               Sex = sex,
               Date = date,
               Peak = value)
    }
  })
  
  # Summary statistics
  output$ts_summary <- renderPrint({
    df <- ts_data()
    if (nrow(df) == 0) return("No data available for selected filters.")
    
    if (input$ts_dataset == "local_authority") {
      df %>% 
        group_by(area_name) %>% 
        summarise(
          Mean = mean(value, na.rm = TRUE),
          Median = median(value, na.rm = TRUE),
          SD = sd(value, na.rm = TRUE),
          Min = min(value, na.rm = TRUE),
          Max = max(value, na.rm = TRUE)
        )
    } else {
      df %>% 
        summarise(
          Mean = mean(value, na.rm = TRUE),
          Median = median(value, na.rm = TRUE),
          SD = sd(value, na.rm = TRUE),
          Min = min(value, na.rm = TRUE),
          Max = max(value, na.rm = TRUE)
        )
    }
  })
  
  # Comparison plot (weekly averages)
  output$ts_weekly_plot <- renderPlotly({
    df <- ts_data()
    if (nrow(df) == 0) return(NULL)
    
    weekly_df <- df %>%
      mutate(week = floor_date(date, "week")) %>%
      group_by(week, area_name) %>%
      summarise(weekly_avg = mean(value, na.rm = TRUE), .groups = "drop")
    
    if (input$ts_dataset == "local_authority") {
      p <- ggplot(weekly_df, aes(x = week, y = weekly_avg, color = area_name)) +
        geom_line() +
        labs(title = "Weekly Averages",
             x = "Week", y = "Weekly Average",
             color = "Council Area")
    } else {
      p <- ggplot(weekly_df, aes(x = week, y = weekly_avg, color = area_name)) +
        geom_line() +
        labs(title = paste("Weekly Averages for", input$ts_age_group),
             x = "Week", y = "Weekly Average",
             color = "Age Group")
    }
    
    ggplotly(p)
  })
  
  output$ts_peaks <- renderReactable({
    df <- ts_data()
    if (nrow(df) == 0) return(NULL)
    
    # Get current metric name for titles
    metric_name <- switch(input$ts_metric,
                          "daily_positive" = "Daily Cases",
                          "cumulative_positive" = "Total Cases",
                          "daily_deaths" = "Daily Deaths",
                          "positive_tests" = "Positive Tests",
                          "Total Tests")
    
    peaks <- df %>%
      group_by(area_name) %>%
      filter(value == max(value, na.rm = TRUE)) %>%
      mutate(Peak_Date = format(date, "%b %d, %Y"),  # Better date format
             Peak_Value = round(value, 1)) %>%
      select(Area = area_name, Peak_Date, Peak_Value) %>%
      distinct(Area, Peak_Value, .keep_all = TRUE) %>%  # Remove duplicates
      arrange(desc(Peak_Value))
    
    reactable(
      peaks,
      columns = list(
        Area = colDef(name = "Council Area", width = 180),
        Peak_Date = colDef(name = "Date of Peak", align = "center"),
        Peak_Value = colDef(
          name = paste("Peak", metric_name),
          align = "center",
          cell = function(value) {
            if (value >= 1000) {
              format(value, big.mark = ",")  # Add thousands separator
            } else {
              value
            }
          },
          style = function(value) {
            if (value >= quantile(peaks$Peak_Value, 0.75)) {
              list(background = "rgba(255, 0, 0, 0.1)", fontWeight = "bold")
            }
          }
        )
      ),
      bordered = TRUE,
      highlight = TRUE,
      defaultPageSize = 10,
      searchable = TRUE,
      showPageSizeOptions = TRUE,
      style = list(fontFamily = "Arial, sans-serif"),
      theme = reactableTheme(
        headerStyle = list(
          backgroundColor = "#f7f7f8",
          borderColor = "#e1e1e1"
        )
      )
    )
  })
  
  output$current_metric <- renderText({
    switch(input$ts_metric,
           "daily_positive" = "Daily Positive Cases",
           "cumulative_positive" = "Cumulative Positive Cases",
           "daily_deaths" = "Daily Deaths",
           "positive_tests" = "Positive Tests",
           "total_tests" = "Total Tests Conducted")
  })
  
  output$current_date_range <- renderText({
    paste(format(input$ts_date_range[1], "%B %d, %Y"), 
          "to", 
          format(input$ts_date_range[2], "%B %d, %Y"))
  })
  
}
