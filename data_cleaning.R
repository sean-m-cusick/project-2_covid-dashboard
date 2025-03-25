#__________________________________
# 1. INSTALL & LOAD LIBRARIES #####
#__________________________________
if (!require(shiny)) install.packages("shiny", dependencies=TRUE)
if (!require(leaflet)) install.packages("leaflet", dependencies=TRUE)
if (!require(ggplot2)) install.packages("ggplot2", dependencies=TRUE) 
if (!require(readr)) install.packages("readr", dependencies=TRUE)
if (!require(dplyr)) install.packages("dplyr", dependencies=TRUE) 
if (!require(naniar)) install.packages("naniar", dependencies=TRUE)
if (!require(tidyverse)) install.packages("tidyverse", dependencies=TRUE)
if (!require(janitor)) install.packages("janitor", dependencies=TRUE)
if (!require(stringr)) install.packages("stringr", dependencies=TRUE)
if (!require(here)) install.packages("here", dependencies=TRUE)
if (!require(broom)) install.packages("broom", dependencies=TRUE)
if (!require(slider)) install.packages("slider", dependencies=TRUE)
if (!require(lubridate)) install.packages("lubridate", dependencies=TRUE)

library(shiny)
library(leaflet)
library(ggplot2)
library(readr)
library(dplyr)
library(naniar)
library(tidyverse)
library(janitor)
library(stringr)
library(here)
library(broom)
library(slider)
library(lubridate)

#__________________________________
# 2. LOAD DATA                #####
#__________________________________
# Load the datasets
total_cases_by_health_board <- read_csv("data_raw/total_cases_by_health_board.csv")
total_cases_by_local_authority <- read_csv("data_raw/total_cases_by_local_authority.csv")
total_cases_by_age_sex <- read_csv("data_raw/total_cases_by_age_sex.csv")
total_cases_by_deprivation <- read_csv("data_raw/total_cases_by_deprivation.csv")
daily_case_trends_by_health_board <- read_csv("data_raw/daily_case_trends_by_health_board.csv")
daily_case_trends_by_local_authority <- read_csv("data_raw/daily_case_trends_by_local_authority.csv")
daily_case_trends_by_age_sex <- read_csv("data_raw/daily_case_trends_by_age_sex.csv")
daily_case_trends_by_deprivation <- read_csv("data_raw/daily_case_trends_by_deprivation.csv")
tests_by_health_board <- read_csv("data_raw/tests_by_health_board.csv")
tests_by_local_authority <- read_csv("data_raw/tests_by_local_authority.csv")
daily_covid_hospital_admissions <- read_csv("data_raw/daily_covid_hospital_admissions.csv")
ethnicity <- read_csv("data_raw/ethnicity.csv")
hospital_location <- read_csv("data_raw/hospital_location.csv")
council_area_2019 <- read_csv("data_raw/council_area_2019.csv")
# statistical_qualifiers <- read_csv("data_raw/statistical_qualifiers.csv")  # This is a dictionary and doesn't require wrangling or cleaning
ca_pop_est <- read_csv("data_raw/ca_pop_est.csv")
hscp_pop_est <- read_csv("data_raw/hscp_pop_est.csv")
hb_pop_est <- read_csv("data_raw/hb_pop_est.csv")
scotland_pop_proj <- read_csv("data_raw/scotland_pop_proj.csv")
ca_pop_proj <- read_csv("data_raw/ca_pop_proj.csv")
hscp_pop_proj <- read_csv("data_raw/hscp_pop_proj.csv")
hb_pop_proj <- read_csv("data_raw/hb_pop_proj.csv")

#______________________________________
# 3. GENERAL DATA CLEANING        #####
#______________________________________
# Functions
## clean text strings
clean_strings <- function(input) {
  input <- str_trim(input)
  
  # Insert underscore between lowercase and uppercase letters
  input <- str_replace_all(input, "([a-z])([A-Z])", "\\1_\\2")
  
  # Insert underscore between the last two capital letters in sequences of two or more capitals followed by lowercase letters
  input <- str_replace_all(input, "([A-Z]{2,})([A-Z][a-z])", "\\1_\\2")
  
  # Replace spaces and other whitespace characters with underscores
  input <- str_replace_all(input, "\\s+", "_")
  
  # Convert everything to lowercase
  input <- str_to_lower(input)
  
  # Check later - replace special characters with underscores
  # input <- str_replace_all(input, "[^a-z0-9_]", "_")
  
  return(input)
}


clean_data_frame <- function(df) {
  # Convert to tibble
  df <- as_tibble(df)
  
  # Clean column names
  colnames(df) <- clean_strings(colnames(df))
  
  
  # Apply clean_strings only to relevant character columns
  df <- df %>%
    mutate(across(where(is.character), 
                  ~ ifelse(is.na(.), ., 
                           ifelse(grepl("^S[0-9]+", .), ., clean_strings(.)))))
  
  return(df)
}


# create new tibbles incase cleaning breaks the dataframe
clean_total_cases_by_health_board <- clean_data_frame(total_cases_by_health_board)
clean_total_cases_by_local_authority <- clean_data_frame(total_cases_by_local_authority)
clean_total_cases_by_age_sex <- clean_data_frame(total_cases_by_age_sex)
clean_total_cases_by_deprivation <- clean_data_frame(total_cases_by_deprivation)
clean_daily_case_trends_by_health_board <- clean_data_frame(daily_case_trends_by_health_board)
clean_daily_case_trends_by_local_authority <- clean_data_frame(daily_case_trends_by_local_authority)
clean_daily_case_trends_by_age_sex <- clean_data_frame(daily_case_trends_by_age_sex)
clean_daily_case_trends_by_deprivation <- clean_data_frame(daily_case_trends_by_deprivation)
clean_tests_by_health_board <- clean_data_frame(tests_by_health_board)
clean_tests_by_local_authority <- clean_data_frame(tests_by_local_authority)
clean_daily_covid_hospital_admissions <- clean_data_frame(daily_covid_hospital_admissions)
clean_ethnicity <- clean_data_frame(ethnicity)
clean_hospital_location <- clean_data_frame(hospital_location)
clean_council_area_2019 <- clean_data_frame(council_area_2019)
clean_ca_pop_est <- clean_data_frame(ca_pop_est)
clean_hscp_pop_est <- clean_data_frame(hscp_pop_est)
clean_hb_pop_est <- clean_data_frame(hb_pop_est)
clean_scotland_pop_proj <- clean_data_frame(scotland_pop_proj)
clean_ca_pop_proj <- clean_data_frame(ca_pop_proj)
clean_hscp_pop_proj <- clean_data_frame(hscp_pop_proj)
clean_hb_pop_proj <- clean_data_frame(hb_pop_proj)

# debugging functions - 
# summary(clean_total_cases_by_health_board)
# str(clean_total_cases_by_health_board)

# Dataset looks ready for manipulation


#___________________________________________________________
# 4. DATA WRANGLING & CLEANING - total_cases_by_health_board    #####
#___________________________________________________________
# Convert Date columns to Date type
clean_total_cases_by_health_board <- clean_total_cases_by_health_board %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

# Not necessary to expand dates on totals as can't be used for time series

# Convert Date and create new variables for total_cases_by_health_board
# clean_total_cases_by_health_board <- clean_total_cases_by_health_board %>%
#   mutate(date = ymd(date),
#          year = year(date),
#          month = month(date, label = TRUE),
#          day = day(date),
#          week = week(date),
#          weekday = wday(date, label = TRUE))

# Dataset looks ready for manipulation

#_______________________________________________________________________
# 5. DATA WRANGLING & CLEANING  - total_cases_by_local_authority   #####
#_______________________________________________________________________
# Convert Date columns to Date type
clean_total_cases_by_health_board <- clean_total_cases_by_local_authority %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

# Not necessary to expand dates on totals as can't be used for time series

# Convert Date and create new variables for total_cases_by_health_board
# clean_total_cases_by_health_board <- clean_total_cases_by_health_board %>%
#   mutate(date = ymd(date),
#          year = year(date),
#          month = month(date, label = TRUE),
#          day = day(date),
#          week = week(date),
#          weekday = wday(date, label = TRUE))


# Dataset looks ready for manipulation


#_______________________________________________________________
# 6. DATA WRANGLING & CLEANING  - total_cases_by_age_sex   #####
#_______________________________________________________________
# date was in YYYYMMDD format so the date cleaning code
clean_total_cases_by_age_sex <- clean_total_cases_by_age_sex %>%
  mutate(date = as.Date(as.character(date), format = "%Y%m%d"))

# the sex_qf and age_group_qf shows the quality - and only has a value for the total, "d" for "derived"

# head(clean_total_cases_by_age_sex)

# Dataset looks ready for manipulation

#___________________________________________________________________
# 7. DATA WRANGLING & CLEANING  - total_cases_by_deprivation   #####
#___________________________________________________________________
# date was in YYYYMMDD format so the date cleaning code
clean_total_cases_by_deprivation <- clean_total_cases_by_deprivation %>%
  mutate(date = as.Date(as.character(date), format = "%Y%m%d"))


# Dataset looks ready for manipulation

#_____________________________________________________________________________
# 8. DATA WRANGLING & CLEANING  - daily_case_trends_by_local_authority   #####
#_____________________________________________________________________________
clean_daily_case_trends_by_local_authority <- clean_daily_case_trends_by_local_authority %>%
  mutate(date = as.Date(as.character(date), format = "%Y%m%d"))


# Convert Date and create new variables for daily_case_trends_by_local_authority
clean_daily_case_trends_by_local_authority <- clean_daily_case_trends_by_local_authority %>%
  mutate(date = ymd(date),
         year = year(date),
         month = month(date),
         day = day(date),
         week = week(date),
         weekday = wday(date, label = TRUE),
         season = case_when(
           month %in% c(12, 1, 2) ~ "winter",
           month %in% c(3, 4, 5) ~ "spring",
           month %in% c(6, 7, 8) ~ "summer",
           month %in% c(9, 10, 11) ~ "autumn",
           TRUE ~ NA_character_
         )
  )

# Dataset looks ready for manipulation
#______________________________________________________________________
# 9. DATA WRANGLING & CLEANING  - daily_case_trends_by_age_sex    #####
#______________________________________________________________________

clean_daily_case_trends_by_age_sex <- clean_daily_case_trends_by_age_sex %>%
  mutate(date = as.Date(as.character(date), format = "%Y%m%d"))


# Convert Date and create new variables for daily_case_trends_by_age_sex
clean_daily_case_trends_by_age_sex <- clean_daily_case_trends_by_age_sex %>%
  mutate(date = ymd(date),
         year = year(date),
         month = month(date),
         day = day(date),
         week = week(date),
         weekday = wday(date, label = TRUE),
         season = case_when(
           month %in% c(12, 1, 2) ~ "winter",
           month %in% c(3, 4, 5) ~ "spring",
           month %in% c(6, 7, 8) ~ "summer",
           month %in% c(9, 10, 11) ~ "autumn",
           TRUE ~ NA_character_
         )
  )

# Dataset looks ready for manipulation

#___________________________________________________________________________
# 10. DATA WRANGLING & CLEANING  - daily_case_trends_by_deprivation    #####
#___________________________________________________________________________

clean_daily_case_trends_by_deprivation <- clean_daily_case_trends_by_deprivation %>%
  mutate(date = as.Date(as.character(clean_daily_case_trends_by_deprivation$date), format = "%Y%m%d"))


# Convert Date and create new variables for daily_case_trends_by_deprivation
clean_daily_case_trends_by_deprivation <- clean_daily_case_trends_by_deprivation %>%
  mutate(date = ymd(clean_daily_case_trends_by_deprivation$date),
         year = year(clean_daily_case_trends_by_deprivation$date),
         month = month(clean_daily_case_trends_by_deprivation$date),
         day = day(clean_daily_case_trends_by_deprivation$date),
         week = week(clean_daily_case_trends_by_deprivation$date),
         weekday = wday(clean_daily_case_trends_by_deprivation$date, label = TRUE),
         season = case_when(
           month %in% c(12, 1, 2) ~ "winter",
           month %in% c(3, 4, 5) ~ "spring",
           month %in% c(6, 7, 8) ~ "summer",
           month %in% c(9, 10, 11) ~ "autumn",
           TRUE ~ NA_character_
           )
         )

# Dataset looks ready for manipulation

#_______________________________________________________________
# 11. DATA WRANGLING & CLEANING  - tests_by_health_board   #####
#_______________________________________________________________
clean_tests_by_health_board <- clean_tests_by_health_board %>%
  mutate(date = as.Date(as.character(date), format = "%Y%m%d"))


# Dataset looks ready for manipulation

#__________________________________________________________________
# 12. DATA WRANGLING & CLEANING  - tests_by_local_authority   #####
#__________________________________________________________________

clean_tests_by_local_authority <- clean_tests_by_local_authority %>%
  mutate(date = as.Date(as.character(date), format = "%Y%m%d"))

# Dataset looks ready for manipulation


#_________________________________________________________________________
# 13. DATA WRANGLING & CLEANING  - daily_covid_hospital_admissions   #####
#_________________________________________________________________________

clean_daily_covid_hospital_admissions <- clean_daily_covid_hospital_admissions %>%
  mutate(date = as.Date(as.character(date), format = "%Y%m%d"))

clean_daily_covid_hospital_admissions <- clean_daily_covid_hospital_admissions %>%
  mutate(date = ymd(date),
         year = year(date),
         month = month(date),
         day = day(date),
         week = week(date),
         weekday = wday(date, label = TRUE),
         season = case_when(
           month %in% c(12, 1, 2) ~ "winter",
           month %in% c(3, 4, 5) ~ "spring",
           month %in% c(6, 7, 8) ~ "summer",
           month %in% c(9, 10, 11) ~ "autumn",
           TRUE ~ NA_character_
         )
        )

# Dataset looks ready for manipulation

#___________________________________________________________
# 14. DATA WRANGLING & CLEANING  - ethnicity           #####
#___________________________________________________________


clean_ethnicity <- clean_ethnicity %>%
  mutate(date = as.Date(as.character(month), format = "%Y%m%d")) %>% 
  select(-month)

clean_ethnicity <- clean_ethnicity %>%  
  mutate(date = ymd(date),
         year = year(date),
         month = month(date),
         day = day(date),
         week = week(date),
         weekday = wday(date, label = TRUE),
         season = case_when(
           month %in% c(12, 1, 2) ~ "winter",
           month %in% c(3, 4, 5) ~ "spring",
           month %in% c(6, 7, 8) ~ "summer",
           month %in% c(9, 10, 11) ~ "autumn",
           TRUE ~ NA_character_
         )
  )


# Dataset looks ready for manipulation

#___________________________________________________________
# 15. DATA WRANGLING & CLEANING  - hospital_location   #####
#___________________________________________________________
str(clean_hospital_location)
summary(clean_hospital_location)

# Functions
clean_address <- function(df) {
  df <- df %>%
    mutate(
      # Split the address_line into parts based on commas
      split_address = str_split(address_line, ","),
      
      # Handle cases with two commas (three parts) or one comma (two parts)
      hospital = if_else(lengths(split_address) == 3, sapply(split_address, `[`, 1), NA_character_),
      road = if_else(lengths(split_address) == 3, sapply(split_address, `[`, 2), sapply(split_address, `[`, 1)),
      area = if_else(lengths(split_address) == 3, sapply(split_address, `[`, 3), sapply(split_address, `[`, 2))
    ) %>%
    # Remove any leading underscores from road and area
    mutate(
      road = str_replace_all(road, "^_", ""),
      area = str_replace_all(area, "^_", "")
    ) %>%
    # Remove the intermediate split_address column
    select(-split_address)
  
  return(df)
}

clean_coordinates <- function(df) {
  df <- df %>%
    mutate(
      # Rename columns x_coordinate and y_coordinate to lng and lat
      lng = as.numeric(x_coordinate),
      lat = as.numeric(y_coordinate)
    ) %>%
    # Remove the original x_coordinate and y_coordinate columns
    select(-x_coordinate, -y_coordinate)
  
  return(df)
}

clean_hospital_location <- clean_address(clean_hospital_location)
clean_hospital_location <- clean_coordinates(clean_hospital_location)


# Dataset looks ready for manipulation

#___________________________________________________________
# 16. DATA WRANGLING & CLEANING  - council_area_2019   #####
#___________________________________________________________
str(clean_council_area_2019)
summary(clean_council_area_2019)

# Function to clean dates for Council Area (ca)
clean_ca_dates <- function(df, prefix) {
  enacted_col <- paste0(prefix, "_date_enacted")
  archived_col <- paste0(prefix, "_date_archived")
  
  # Check if the year, month, and day columns already exist
  year_enacted_col <- paste0(prefix, "_year_enacted")
  month_enacted_col <- paste0(prefix, "_month_enacted")
  day_enacted_col <- paste0(prefix, "_day_enacted")
  
  year_archived_col <- paste0(prefix, "_year_archived")
  month_archived_col <- paste0(prefix, "_month_archived")
  day_archived_col <- paste0(prefix, "_day_archived")
  
  period_col <- paste0(prefix, "_period")
  
  # Only create new columns if they don't already exist
  if (!all(c(year_enacted_col, month_enacted_col, day_enacted_col, 
             year_archived_col, month_archived_col, day_archived_col, 
             period_col) %in% colnames(df))) {
    df <- df %>%
      mutate(
        # Convert numeric dates to Date objects
        !!enacted_col := ymd(!!sym(enacted_col)),
        !!archived_col := ymd(!!sym(archived_col)),
        
        # Extract year, month, and day
        !!year_enacted_col := year(!!sym(enacted_col)),
        !!month_enacted_col := month(!!sym(enacted_col)),
        !!day_enacted_col := day(!!sym(enacted_col)),
        
        !!year_archived_col := year(!!sym(archived_col)),
        !!month_archived_col := month(!!sym(archived_col)),
        !!day_archived_col := day(!!sym(archived_col)),
        
        # Calculate period
        !!period_col := if_else(
          is.na(!!sym(archived_col)),
          as.duration(interval(!!sym(enacted_col), today())), # If archived is NA, calculate until today
          as.duration(interval(!!sym(enacted_col), !!sym(archived_col))) # Otherwise, calculate between enacted and archived
        )
      )
    }
  
    return(df)
}


# Apply the function to ca, hscp, and hb date columns
clean_council_area_2019 <- clean_council_area_2019 %>%
  clean_ca_dates("ca") %>%
  clean_ca_dates("hscp") %>%
  clean_ca_dates("hb")

# View the updated data frame
print(council_area_2019)

# Dataset looks ready for manipulation

#___________________________________________________________
# 17. DATA WRANGLING & CLEANING  - ca_pop_est          #####
#___________________________________________________________

# Dataset looks ready for manipulation

#___________________________________________________________
# 18. DATA WRANGLING & CLEANING  - hscp_pop_est        #####
#___________________________________________________________


# Dataset looks ready for manipulation


#___________________________________________________________
# 19. DATA WRANGLING & CLEANING  - hb_pop_est          #####
#___________________________________________________________

# Dataset looks ready for manipulation

#___________________________________________________________
# 20. DATA WRANGLING & CLEANING  - scotland_pop_proj   #####
#___________________________________________________________


# Dataset looks ready for manipulation


#___________________________________________________________
# 21. DATA WRANGLING & CLEANING  - ca_pop_proj         #####
#___________________________________________________________

# Dataset looks ready for manipulation

#___________________________________________________________
# 22. DATA WRANGLING & CLEANING  - hscp_pop_proj       #####
#___________________________________________________________


# Dataset looks ready for manipulation


#___________________________________________________________
# 23. DATA WRANGLING & CLEANING  - hb_pop_proj         #####
#___________________________________________________________

# Dataset looks ready for manipulation

#______________________________________
# 24. CLEAN DATA EXPORT           #####
#______________________________________

# COMBINE LOCATIONS
# Join the datasets on health board code
combined_data <- clean_hospital_location %>%
  left_join(clean_council_area_2019, by = c("hb" = "hb"))

clean_combined_data <- clean_data_frame(combined_data) %>%
  clean_ca_dates("ca") %>%
  clean_ca_dates("hscp") %>%
  clean_ca_dates("hb")


# View the result
head(combined_data)

# NOT READY - NEEDS CLEANING ^^^^

# Create data_clean subdirectory if it doesn't exist
if (!dir.exists("data_clean")) {
  dir.create("data_clean")
}

# Export datasets to CSV function
export_to_csv <- function(df, name) {
  write_csv(df, paste0("data_clean/", name, ".csv"))
}


# Export
export_to_csv(clean_total_cases_by_health_board, "clean_total_cases_by_health_board")
export_to_csv(clean_total_cases_by_local_authority, "clean_total_cases_by_local_authority")
export_to_csv(clean_total_cases_by_age_sex, "clean_total_cases_by_age_sex")
export_to_csv(clean_total_cases_by_deprivation, "clean_total_cases_by_deprivation")
export_to_csv(clean_daily_case_trends_by_health_board, "clean_daily_case_trends_by_health_board")
export_to_csv(clean_daily_case_trends_by_local_authority, "clean_daily_case_trends_by_local_authority")
export_to_csv(clean_daily_case_trends_by_age_sex, "clean_daily_case_trends_by_age_sex")
export_to_csv(clean_daily_case_trends_by_deprivation, "clean_daily_case_trends_by_deprivation")
export_to_csv(clean_tests_by_health_board, "clean_tests_by_health_board")
export_to_csv(clean_tests_by_local_authority, "clean_tests_by_local_authority")
export_to_csv(clean_daily_covid_hospital_admissions, "clean_daily_covid_hospital_admissions")
export_to_csv(clean_ethnicity, "clean_ethnicity")
export_to_csv(clean_hospital_location, "clean_hospital_location")
export_to_csv(clean_council_area_2019, "clean_council_area_2019")
export_to_csv(clean_ca_pop_est, "clean_ca_pop_est")
export_to_csv(clean_hscp_pop_est, "clean_hscp_pop_est")
export_to_csv(clean_hb_pop_est, "clean_hb_pop_est")
export_to_csv(clean_scotland_pop_proj, "clean_scotland_pop_proj")
export_to_csv(clean_ca_pop_proj, "clean_ca_pop_proj")
export_to_csv(clean_hscp_pop_proj, "clean_hscp_pop_proj")
export_to_csv(clean_hb_pop_proj, "clean_hb_pop_proj")
export_to_csv(combined_data, "clean_combined_data")

