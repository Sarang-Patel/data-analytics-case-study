
library("tidyverse")
library("lubridate")
library("ggplot2")
library("dplyr")
library("stringr")

##### load the dataset

dataset <- read.csv("C:\\Users\\saran\\Desktop\\data analysis projects\\case study\\Cyclistic\\Raw data\\202506-divvy-tripdata.csv")
dataset

nrow(dataset)

######

#####checking for duplicates - 0 found

duplicates <- dataset[duplicated(dataset),]

duplicates

######

###### checking for naming errors - 0 found

n_distinct(dataset$rideable_type)
unique(dataset$rideable_type)


n_distinct(dataset$member_casual)
unique(dataset$member_casual)


######

###### trimming all data

dataset <- dataset %>%
  mutate(across(where(is.character), str_trim))

dataset

###### removing null or empty strings to we have complete data for geospatial viz later.

dataset <- dataset %>%
  filter(
    !(start_station_name == "" | is.na(start_station_name)),
    !(start_station_id == "" | is.na(start_station_id)),
    !(end_station_name == "" | is.na(end_station_name)),
    !(end_station_id == "" | is.na(end_station_id))
  ) %>%
  mutate(
    start_lat = round(as.numeric(start_lat), 6),
    start_lng = round(as.numeric(start_lng), 6),
    end_lat = round(as.numeric(end_lat), 6),
    end_lng = round(as.numeric(end_lng), 6)
  )

dataset

######

###### converting station ids to characters, removing any decimals and padding with leading zeros for consistent length.

dataset <- dataset %>%
  mutate(
    start_station_id = as.character(start_station_id),
    end_station_id = as.character(end_station_id),
    
    start_station_id = gsub("\\.0$", "", start_station_id),
    end_station_id = gsub("\\.0$", "", end_station_id),
    
    start_station_id = str_pad(start_station_id, width = 12, side = "left", pad = "0"),
    end_station_id = str_pad(end_station_id, width = 12, side = "left", pad = "0")
  )

dataset

###### 

###### separatng the started at ended at columns into separate date time and duration columns
nrow(dataset)
dataset <- dataset %>%
  mutate(
    started_at = ymd_hms(started_at),
    ended_at = ymd_hms(ended_at),
    
    start_date = as.Date(started_at),
    end_date = as.Date(ended_at),
    
    start_hour = hour(started_at),
    end_hour = hour(ended_at),
    
    ride_duration_secs = as.numeric(difftime(ended_at, started_at, units = "secs"))
  ) %>%
  filter(ride_duration_secs > 0) %>%
  mutate(
    ride_duration = sprintf(
      "%02d:%02d:%02d",
      as.integer(ride_duration_secs) %/% 3600,
      (as.integer(ride_duration_secs) %% 3600) %/% 60,
      as.integer(ride_duration_secs) %% 60
    )
  ) %>%
  select(-ride_duration_secs, -started_at, -ended_at)

nrow(dataset)
dataset

######

###### saving the cleaned file

write_csv(dataset, "data_cleaned/2025_06_cleaned.csv")

######



###### combining 12 months data quaterly


# Define file paths for each quarter
q1_files <- c("data_cleaned/2024_06_cleaned.csv",
              "data_cleaned/2024_07_cleaned.csv",
              "data_cleaned/2024_08_cleaned.csv")

q2_files <- c("data_cleaned/2024_09_cleaned.csv",
              "data_cleaned/2024_10_cleaned.csv",
              "data_cleaned/2024_11_cleaned.csv")

q3_files <- c("data_cleaned/2024_12_cleaned.csv",
              "data_cleaned/2025_01_cleaned.csv",
              "data_cleaned/2025_02_cleaned.csv")

q4_files <- c("data_cleaned/2025_03_cleaned.csv",
              "data_cleaned/2025_04_cleaned.csv",
              "data_cleaned/2025_05_cleaned.csv")

# Helper function to read and bind
combine_quarter <- function(file_list) {
  file_list %>%
    map_dfr(read_csv)
}

# Combine each quarter
q1 <- combine_quarter(q1_files)
q2 <- combine_quarter(q2_files)
q3 <- combine_quarter(q3_files)
q4 <- combine_quarter(q4_files)

# Optionally write them out
write_csv(q1, "data_quarters/Q1_2024_cleaned.csv")
write_csv(q2, "data_quarters/Q2_2024_cleaned.csv")
write_csv(q3, "data_quarters/Q3_2024_2025_cleaned.csv")
write_csv(q4, "data_quarters/Q4_2025_cleaned.csv")


######

