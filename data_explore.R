# THIS FILE HAS MIGRATED TO "./data_exploration.Rmd"


# #__________________________________
# # 1. INSTALL & LOAD LIBRARIES #####
# #__________________________________
# if (!require(shiny)) install.packages("shiny", dependencies=TRUE)
# if (!require(leaflet)) install.packages("leaflet", dependencies=TRUE)
# if (!require(ggplot2)) install.packages("ggplot2", dependencies=TRUE) 
# if (!require(readr)) install.packages("readr", dependencies=TRUE)
# if (!require(dplyr)) install.packages("dplyr", dependencies=TRUE) 
# if (!require(naniar)) install.packages("naniar", dependencies=TRUE)
# if (!require(tidyverse)) install.packages("tidyverse", dependencies=TRUE)
# if (!require(janitor)) install.packages("janitor", dependencies=TRUE)
# if (!require(stringr)) install.packages("stringr", dependencies=TRUE)
# if (!require(here)) install.packages("here", dependencies=TRUE)
# if (!require(broom)) install.packages("broom", dependencies=TRUE)
# if (!require(slider)) install.packages("slider", dependencies=TRUE)
# if (!require(lubridate)) install.packages("lubridate", dependencies=TRUE)
# 
# library(shiny)
# library(leaflet)
# library(ggplot2)
# library(readr)
# library(dplyr)
# library(naniar)
# library(tidyverse)
# library(janitor)
# library(stringr)
# library(here)
# library(broom)
# library(slider)
# library(lubridate)
# 
# #________________________________
# # 2. DOWNLOAD & LOAD DATA   #####
# #________________________________
# # Ensure the `data_raw` directory exists
# if (!dir.exists("data_raw")) {
#   dir.create("data_raw")
# }
# 
# # Define the URLs for the CSV files
# urls <- list(
#   "total_cases_by_health_board" = "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/7fad90e5-6f19-455b-bc07-694a22f8d5dc/download/total_cases_by_hb_20231004.csv",
#   "total_cases_by_local_authority" = "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/e8454cf0-1152-4bcb-b9da-4343f625dfef/download/total_cases_by_la_20231004.csv",
#   "total_cases_by_age_sex" = "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/19646dce-d830-4ee0-a0a9-fcec79b5ac71/download/total_cases_agesex_20231004.csv", 
#   "total_cases_by_deprivation" = "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/a965ee86-0974-4c93-bbea-e839e27d7085/download/total_cases_simd_20231004.csv",
#   "daily_case_trends_by_health_board" = "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/2dd8534b-0a6f-4744-9253-9565d62f96c2/download/trend_hb_20231004.csv", 
#   "daily_case_trends_by_local_authority" = "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/427f9a25-db22-4014-a3bc-893b68243055/download/trend_ca_20231004.csv",
#   "daily_case_trends_by_age_sex" = "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/9393bd66-5012-4f01-9bc5-e7a10accacf4/download/trend_agesex_20231004.csv", 
#   "daily_case_trends_by_deprivation" = "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/a38a4c21-7c75-4ecd-a511-3f83e0e8f0c3/download/trend_simd_20231004.csv", 
#   "tests_by_health_board" = "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/8da654cd-293b-4286-96a4-b3ece86225f0/download/test_hb_20231004.csv", 
#   "tests_by_local_authority" = "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/3349540e-dc63-4d6d-a78b-00387b9aca50/download/test_ca_20231004.csv", 
#   "daily_covid_hospital_admissions" = "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/bb4d083b-7b92-4722-85a3-a9b58af1f794/download/daily_covid_admissions_20231004.csv", 
#   "ethnicity" = "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/07b3dff7-3254-4d26-946f-6cd9ff276a1d/download/ethnicity_20231004.csv"
#   # Locations seems to have been removed from the opendata archive- have an older version downloaded in 2021. 
# )
# 
# # Download the files
# for (name in names(urls)) {
#   download.file(urls[[name]], paste0("data_raw/", name, ".csv"))
# }
# 
# # Load the datasets
# total_cases_by_health_board <- read_csv("data_raw/total_cases_by_health_board.csv")
# total_cases_by_local_authority <- read_csv("data_raw/total_cases_by_local_authority.csv")
# total_cases_by_age_sex <- read_csv("data_raw/total_cases_by_age_sex.csv")
# total_cases_by_deprivation <- read_csv("data_raw/total_cases_by_deprivation.csv")
# daily_case_trends_by_health_board <- read_csv("data_raw/daily_case_trends_by_health_board.csv")
# daily_case_trends_by_local_authority <- read_csv("data_raw/daily_case_trends_by_local_authority.csv")
# daily_case_trends_by_age_sex <- read_csv("data_raw/daily_case_trends_by_age_sex.csv")
# daily_case_trends_by_deprivation <- read_csv("data_raw/daily_case_trends_by_deprivation.csv")
# tests_by_health_board <- read_csv("data_raw/tests_by_health_board.csv")
# tests_by_local_authority <- read_csv("data_raw/tests_by_local_authority.csv")
# daily_covid_hospital_admissions <- read_csv("data_raw/daily_covid_hospital_admissions.csv")
# ethnicity <- read_csv("data_raw/ethnicity.csv")
# hospital_location <- read_csv("data_raw/hospital_location.csv")
# 
# #____________________________________
# # 3. EXPLORE DATASETS - BASICS  #####
# #____________________________________
# # The following code can be copied and pasted and then adapted as I explore each dataset. These are the fundamental tools I use to explore a dataset.
# 
# # Initial look at dataset
# #________________________
# # Check the structure of the datasets
# 
# str(total_cases_by_local_authority)
# 
# 
# # Summary statistics
# summary(total_cases_by_local_authority)
# 
# # View the first few rows
# head(total_cases_by_local_authority)
# 
# 
# #     Missing Data
# #________________________
# # Visualize missing data
# gg_miss_var(total_cases_by_local_authority) %>% 
#   print()
# 
# 
# # Handle missing values by replacing with 0 (example)
# total_cases_by_local_authority <- total_cases_by_local_authority %>%
#   mutate(across(everything(), ~ifelse(is.na(.), 0, .)))
# 
# 
# 
# #___________________________________________________________
# # 4. DATA EXPLORATION - total_cases_by_health_board    #####
# #___________________________________________________________
# # Check the structure of the dataset
# str(total_cases_by_health_board)
# 
# # Summary statistics
# summary(total_cases_by_health_board)
# 
# 
# # View the first few rows
# head(total_cases_by_health_board)
# 
# # Visualize missing data
# gg_miss_var(total_cases_by_health_board)
# 
# # Handle missing values by replacing with 0 (example)
# total_cases_by_health_board <- total_cases_by_health_board %>%
#   mutate(across(everything(), ~ifelse(is.na(.), 0, .)))
# 
# #_______________________________________________________________________
# # 5. DATA WRANGLING & CLEANING  - total_cases_by_local_authority   #####
# #_______________________________________________________________________
# # Convert Date columns to Date type
# total_cases_by_health_board <- total_cases_by_health_board %>%
#   mutate(Date = as.Date(Date, format = "%Y-%m-%d"))
# 
# 
# 
# # Convert Date and create new variables for total_cases_by_health_board
# total_cases_by_health_board <- total_cases_by_health_board %>%
#   mutate(Date = ymd(Date),
#          Year = year(Date),
#          Month = month(Date, label = TRUE),
#          Day = day(Date),
#          Week = week(Date),
#          Weekday = wday(Date, label = TRUE))
# 
# 
# 
# 
# #_______________________________________________________________
# # 6. DATA WRANGLING & CLEANING  - total_cases_by_age_sex   #####
# #_______________________________________________________________
# 
# total_cases_by_age_sex <- total_cases_by_age_sex %>%
#   mutate(Date = as.Date(as.character(Date), format = "%Y%m%d"))
# 
# 
# 
# # Convert Date and create new variables for total_cases_by_age_sex
# total_cases_by_age_sex <- total_cases_by_age_sex %>%
#   mutate(Date = ymd(Date),
#          Year = year(Date),
#          Month = month(Date, label = TRUE),
#          Day = day(Date),
#          Week = week(Date),
#          Weekday = wday(Date, label = TRUE))
# 
# 
# #___________________________________________________________________
# # 7. DATA WRANGLING & CLEANING  - total_cases_by_deprivation   #####
# #___________________________________________________________________
# total_cases_by_deprivation <- total_cases_by_deprivation %>%
#   mutate(Date = as.Date(Date, format = "%Y-%m-%d"))
# 
# 
# 
# daily_case_trends_by_health_board <- daily_case_trends_by_health_board %>%
#   mutate(Date = as.Date(Date, format = "%Y-%m-%d"))
# 
# #_____________________________________________________________________________
# # 8. DATA WRANGLING & CLEANING  - daily_case_trends_by_local_authority   #####
# #_____________________________________________________________________________
# daily_case_trends_by_local_authority <- daily_case_trends_by_local_authority %>%
#   mutate(Date = as.Date(Date, format = "%Y-%m-%d"))
# 
# 
# # Convert Date and create new variables for daily_case_trends_by_local_authority
# daily_case_trends_by_local_authority <- daily_case_trends_by_local_authority %>%
#   mutate(Date = ymd(Date),
#          Year = year(Date),
#          Month = month(Date, label = TRUE),
#          Day = day(Date),
#          Week = week(Date),
#          Weekday = wday(Date, label = TRUE))
# 
# #______________________________________________________________________
# # 9. DATA WRANGLING & CLEANING  - daily_case_trends_by_age_sex    #####
# #______________________________________________________________________
# 
# daily_case_trends_by_age_sex <- daily_case_trends_by_age_sex %>%
#   mutate(Date = as.Date(as.character(Date), format = "%Y%m%d"))
# 
# 
# # Convert Date and create new variables for daily_case_trends_by_age_sex
# daily_case_trends_by_age_sex <- daily_case_trends_by_age_sex %>%
#   mutate(Date = ymd(Date),
#          Year = year(Date),
#          Month = month(Date, label = TRUE),
#          Day = day(Date),
#          Week = week(Date),
#          Weekday = wday(Date, label = TRUE))
# 
# 
# #___________________________________________________________________________
# # 10. DATA WRANGLING & CLEANING  - daily_case_trends_by_deprivation    #####
# #___________________________________________________________________________
# 
# daily_case_trends_by_deprivation <- daily_case_trends_by_deprivation %>%
#   mutate(Date = as.Date(Date, format = "%Y-%m-%d"))
# 
# 
# 
# # Convert Date and create new variables for daily_case_trends_by_deprivation
# daily_case_trends_by_deprivation <- daily_case_trends_by_deprivation %>%
#   mutate(Date = ymd(Date),
#          Year = year(Date),
#          Month = month(Date, label = TRUE),
#          Day = day(Date),
#          Week = week(Date),
#          Weekday = wday(Date, label = TRUE))
# 
# #_______________________________________________________________
# # 11. DATA WRANGLING & CLEANING  - tests_by_health_board   #####
# #_______________________________________________________________
# tests_by_health_board <- tests_by_health_board %>%
#   mutate(Date = as.Date(Date, format = "%Y-%m-%d"))
# 
# 
# # Convert Date and create new variables for tests_by_health_board
# tests_by_health_board <- tests_by_health_board %>%
#   mutate(Date = ymd(Date),
#          Year = year(Date),
#          Month = month(Date, label = TRUE),
#          Day = day(Date),
#          Week = week(Date),
#          Weekday = wday(Date, label = TRUE))
# 
# #__________________________________________________________________
# # 12. DATA WRANGLING & CLEANING  - tests_by_local_authority   #####
# #__________________________________________________________________
# 
# tests_by_local_authority <- tests_by_local_authority %>%
#   mutate(Date = as.Date(Date, format = "%Y-%m-%d"))
# 
# 
# 
# # Convert Date and create new variables for tests_by_local_authority
# tests_by_local_authority <- tests_by_local_authority %>%
#   mutate(Date = ymd(Date),
#          Year = year(Date),
#          Month = month(Date, label = TRUE),
#          Day = day(Date),
#          Week = week(Date),
#          Weekday = wday(Date, label = TRUE))
# 
# #_________________________________________________________________________
# # 13. DATA WRANGLING & CLEANING  - daily_covid_hospital_admissions   #####
# #_________________________________________________________________________
# 
# daily_covid_hospital_admissions <- daily_covid_hospital_admissions %>%
#   mutate(Date = as.Date(Date, format = "%Y-%m-%d"))
# 
# daily_covid_hospital_admissions <- daily_covid_hospital_admissions %>%
#   mutate(Date = ymd(Date),
#          Year = year(Date),
#          Month = month(Date, label = TRUE),
#          Day = day(Date),
#          Week = week(Date),
#          Weekday = wday(Date, label = TRUE))
# 
# #___________________________________________________
# # 14. DATA WRANGLING & CLEANING  - ethnicity   #####
# #___________________________________________________
# 
# 
# ethnicity <- ethnicity %>%
#   mutate(Date = ymd(Date),
#          Year = year(Date),
#          Month = month(Date, label = TRUE),
#          Day = day(Date),
#          Week = week(Date),
#          Weekday = wday(Date, label = TRUE))
# 
# 
# #___________________________________________________________
# # 15. DATA WRANGLING & CLEANING  - hospital_location   #####
# #___________________________________________________________