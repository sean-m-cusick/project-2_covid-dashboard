
#__________________________________
# 1. INSTALL & LOAD LIBRARIES #####
#__________________________________
library(shiny)             # Core package for building Shiny web applications
library(shinythemes)       # Provides pre-built Bootstrap themes for Shiny apps

#__________________________________
# 2. SHINY UI  v.I            #####
#__________________________________
# Version 1
# ui <- fluidPage(
#   theme = shinytheme("cerulean"), # Apply cerulean theme
#   titlePanel("Scotland Council Areas and Hospitals"),
#   sidebarLayout(
#     sidebarPanel(
#       width = 3, # 1/4 width
#       tabsetPanel(
#         tabPanel("Map", br(), actionButton("reset_view", "Reset View to Scotland")),
#         tabPanel("Info", br(), "Additional information goes here.")
#       )
#     ),
#     mainPanel(
#       width = 9, # 3/4 width
#       fluidRow(
#         column(
#           width = 12,
#           leafletOutput("map", height = "600px") # 3/4 height
#         )
#       ),
#       fluidRow(
#         column(
#           width = 12,
#           htmlOutput("area_info") # Use htmlOutput instead of verbatimTextOutput
#         )
#       )
#     )
#   )
# )

#__________________________________
# 3. SHINY UI   v.II          #####
#__________________________________
# version 2
# ui <- fluidPage(
#   theme = shinytheme("cerulean"),
#   tags$head(
#     tags$style(HTML("
#       .well {
#         background-color: #f9f9f9;
#         border-radius: 5px;
#         padding: 15px;
#         margin-bottom: 15px;
#         box-shadow: 0 1px 3px rgba(0,0,0,0.1);
#       }
#       .info-panel {
#         background-color: #f0f8ff;
#         border-left: 4px solid #337ab7;
#       }
#       h3, h4, h5 {
#         margin-top: 0;
#         color: #2c3e50;
#       }
#       .table {
#         width: 100%;
#         margin-bottom: 15px;
#       }
#       .table td, .table th {
#         padding: 8px;
#         text-align: left;
#         border-bottom: 1px solid #ddd;
#       }
#       .positive-change {
#         color: #2ecc71;
#         font-weight: bold;
#       }
#       .negative-change {
#         color: #e74c3c;
#         font-weight: bold;
#       }
#       .testing-panel {
#         min-height: 300px;
#       }
#       .zscore-explanation {
#         background-color: #f8f9fa;
#         padding: 15px;
#         border-radius: 5px;
#         margin-top: 15px;
#         border-left: 4px solid #6c757d;
#       }
#       .comparison-good {
#         color: #2ecc71;
#         font-weight: bold;
#       }
#       .comparison-bad {
#         color: #e74c3c;
#         font-weight: bold;
#       }
#       .value-box {
#         border-radius: 5px;
#       }
#     "))
#   ),
#   titlePanel("Scotland Council Areas: Health Services Analysis"),
#   sidebarLayout(
#     sidebarPanel(
#       width = 3,
#       tabsetPanel(
#         tabPanel("Map Controls", 
#                  br(), 
#                  actionButton("reset_view", "Reset View", class = "btn-primary"),
#                  br(), br(),
#                  actionButton("toggle_density", "Toggle Population Density", class = "btn-info")
#         ),
#         tabPanel("Info", 
#                  br(), 
#                  p("This dashboard provides insights into:"),
#                  tags$ul(
#                    tags$li("Hospital distribution across Scottish council areas"),
#                    tags$li("Population health metrics"),
#                    tags$li("COVID-19 testing statistics")
#                  ),
#                  p("Click on any council area to view detailed information.")
#         )
#       )
#     ),
#     mainPanel(
#       width = 9,
#       fluidRow(
#         column(width = 12,
#                leafletOutput("map", height = "500px")
#         )
#       ),
#       fluidRow(
#         column(width = 12,
#                uiOutput("area_info")
#         )
#       )
#     )
#   )
# )

#__________________________________
# 4. SHINY UI   v.III         #####
#__________________________________
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  tags$head(
    tags$style(HTML("
      .well {
        background-color: #f9f9f9;
        border-radius: 5px;
        padding: 15px;
        margin-bottom: 15px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
      }
      .info-panel {
        background-color: #f0f8ff;
        border-left: 4px solid #337ab7;
      }
      h3, h4, h5 {
        margin-top: 0;
        color: #2c3e50;
      }
      .table {
        width: 100%;
        margin-bottom: 15px;
      }
      .table td, .table th {
        padding: 8px;
        text-align: left;
        border-bottom: 1px solid #ddd;
      }
      .positive-change {
        color: #2ecc71;
        font-weight: bold;
      }
      .negative-change {
        color: #e74c3c;
        font-weight: bold;
      }
      .testing-panel {
        min-height: 300px;
      }
      .zscore-explanation {
        background-color: #f8f9fa;
        padding: 15px;
        border-radius: 5px;
        margin-top: 15px;
        border-left: 4px solid #6c757d;
      }
      .comparison-good {
        color: #2ecc71;
        font-weight: bold;
      }
      .comparison-bad {
        color: #e74c3c;
        font-weight: bold;
      }
      .value-box {
        border-radius: 5px;
      }
      .tab-content {
        padding-top: 15px;
      }
      .plot-container {
        height: 500px;
      }
      #map { 
        height: 600px !important; 
      }
    "))
  ),
  titlePanel("Scotland COVID-19 Analysis"),
  tabsetPanel(
    id = "mainTabs",
    
    # First Tab - Council Area Map
    tabPanel("Council Area Map",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 tabsetPanel(
                   tabPanel("Map Controls", 
                            br(), 
                            actionButton("reset_view", "Reset View", class = "btn-primary"),
                            br(), br(),
                            actionButton("toggle_density", "Toggle Population Density", class = "btn-info")
                   ),
                   tabPanel("Info", 
                            br(), 
                            p("This dashboard provides insights into:"),
                            tags$ul(
                              tags$li("Hospital distribution across Scottish council areas"),
                              tags$li("Population health metrics"),
                              tags$li("COVID-19 testing statistics")
                            ),
                            p("Click on any council area to view detailed information.")
                   )
                 )
               ),
               mainPanel(
                 width = 9,
                 fluidRow(
                   column(width = 12,
                          leafletOutput("map")
                   )
                 ),
                 fluidRow(
                   column(width = 12,
                          uiOutput("area_info")
                   )
                 )
               )
             )
    ),
    
    # Second Tab - Time Series Analysis
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
                                min = as.Date("2020-02-28"),
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
                                       p(strong("Metric shown:"), textOutput("current_metric", inline = TRUE)),
                                       p(strong("Date range:"), 
                                         textOutput("current_date_range", inline = TRUE)),
                                       p("Peak values represent the highest recorded values during the selected period."),
                                       reactableOutput("ts_peaks")
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