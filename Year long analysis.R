
# Define file paths for each quarter
q1_files <- c("data_quarters/Q1_2024_cleaned.csv",
              "data_quarters/Q2_2024_cleaned.csv",
              "data_quarters/Q3_2024_2025_cleaned.csv",
              "data_quarters/Q4_2025_cleaned.csv")


# Helper function to read and bind
combine_quarter <- function(file_list) {
  file_list %>%
    map_dfr(read_csv)
}

# Combine each quarter
year_data <- combine_quarter(q1_files)

# Optionally write them out
write_csv(year_data, "data_quarters/year_long_data.csv")
year_data <- read.csv("C:\\Users\\saran\\Desktop\\data analysis projects\\case study\\Cyclistic\\data_quarters\\year_long_data.csv")

year_data <- year_data[!(year_data$ride_duration > "00:50:00" | year_data$ride_duration < "00:02:00"), ]
year_data <- year_data %>%
  mutate(
    day_of_week = weekdays(as.Date(start_date)),  # Day of the week
    month = month(as.Date(start_date), label = TRUE)  # Month (abbreviated)
  )

year_data$day_of_week <- factor(
  year_data$day_of_week,
  levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
)

year_data$month <- factor(
  year_data$month,
  levels = month.name
)

# visaulizations

year_data %>%
  count(member_casual) %>%
  ggplot(aes(x = member_casual, y = n, fill = member_casual)) +
  geom_col() +
  labs(title = "Total Rides by Member Type", x = "User Type", y = "Total Rides") +
  theme_minimal()

year_data %>%
  count(rideable_type) %>%
  ggplot(aes(x = rideable_type, y = n, fill = rideable_type)) +
  geom_col() +
  labs(title = "Rides by Bike Type", x = "Bike Type", y = "Number of Rides") +
  theme_minimal()


year_data <- year_data %>%
  mutate(
    rideable_type = str_replace(rideable_type, "electric_scooter", "electric_bike")
  )

year_data %>%
  count(month, member_casual)%>%
  ggplot(aes(x=month, y = n, colour = member_casual, group = member_casual))+
  geom_line(size = 1.2)+
  geom_point()+
  labs(title = "Monthly ride trends for casual riders and member riders", x = "Month", y = "Num of Rides")+
  theme_minimal()


year_data %>%
  count(day_of_week, member_casual) %>%
  ggplot(aes(x = day_of_week, y = n, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Weekly Ride Patterns", x = "Day of Week", y = "Number of Rides") +
  theme_minimal()



year_data <- year_data %>%
  mutate(
    month = month(as.Date(start_date), label = TRUE)
  )
unique(year_data$month)


year_data %>%
  mutate(ride_sec = as.numeric(as.duration(hms(ride_duration)))) %>%
  mutate(ride_min = ride_sec / 60) %>% 
  group_by(day_of_week, member_casual)%>%
  summarise(avg_ride_min = mean(ride_min, na.rm = TRUE)) %>%
  ggplot(aes(x = day_of_week, y = avg_ride_min, fill = member_casual))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "Average Ride Duration by Day and Member Type",x = "Day of the Week", y = "Average Ride Duration (minutes)") +
  theme_minimal()


year_data %>%
  count(start_station_name) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(start_station_name, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Start Stations", x = "Station", y = "Number of Rides") +
  theme_minimal()


year_data_no <- read.csv("C:\\Users\\saran\\Desktop\\data analysis projects\\case study\\Cyclistic\\data_quarters\\year_long_data.csv")


year_data_no %>%
  mutate(ride_sec = as.numeric(as.duration(hms(ride_duration)))) %>%
  mutate(ride_min = ride_sec / 60) %>% 
  group_by(rideable_type)%>%
  summarise(avg_ride_min = mean(ride_min, na.rm = TRUE)) %>%
  ggplot(aes(x = rideable_type, y = avg_ride_min, fill = rideable_type))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "Average Ride Duration by Day and Member Type",x = "Day of the Week", y = "Average Ride Duration (minutes)") +
  theme_minimal()

unique(year_data_no$rideable_type)
unique(year_data_no$rideable_type)

year_data_no %>%
  mutate(ride_sec = as.numeric(as.duration(hms(ride_duration)))) %>%
  mutate(ride_min = ride_sec / 60) %>% 
  arrange(desc(ride_min))

top_stations <- year_data %>%
  count(start_station_name, member_casual) %>%
  group_by(start_station_name, member_casual) %>%
  top_n(10, n)

top_stations <- year_data %>%
  group_by(start_station_name, member_casual, start_lat, start_lng) %>%  # Group by station, member type, and coordinates
  tally() %>%  # Count the number of rides for each group
  ungroup() %>%  # Remove the grouping for further operations
  group_by(member_casual) %>%  # Group by member type for filtering the top stations
  arrange(desc(n)) %>%  # Arrange by count of rides in descending order
  slice(1:10)

library(sf)
library(ggspatial)

# Assuming `top_stations` dataframe with lat and lng
# Create a simple features (sf) object
stations_sf <- st_as_sf(top_stations, coords = c("start_lng", "start_lat"), crs = 4326)

# You can plot on a map using ggplot with an actual map layer
ggplot() +
  # Add map layer (you can replace with actual shapefile data or use basemap)
  geom_sf(data = stations_sf, aes(color = member_casual), size = 3, alpha = 0.7) +
  scale_color_manual(values = c("blue", "red")) +
  labs(
    title = "Top 10 Most Used Stations by Member Type",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

install.packages("leaflet")

library(leaflet)
leaflet(top_stations) %>%
  addProviderTiles("CartoDB.Positron") %>%  # Add a map tile layer
  addCircleMarkers(
    lat = ~start_lat, 
    lng = ~start_lng, 
    color = ~ifelse(member_casual == "member", "#00bfc4", "#f8766d"),  # Color based on member type
    radius = 6, 
    stroke = FALSE, 
    fillOpacity = 0.8, 
    popup = ~paste(
      "Station: ", start_station_name, "<br>",
      "Member Type: ", member_casual, "<br>",
      "Rides: ", n
    )
  ) %>%
  addLegend(
    position = "topright", 
    colors = c("#00bfc4", "#f8766d"), 
    labels = c("Member", "Casual"), 
    title = "Member Type"
  )



year_data %>%
  count(rideable_type, member_casual) %>%
  group_by(member_casual)%>%
  ggplot(aes(x = rideable_type, y = n, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Rides by Bike Type", x = "Bike Type", y = "Number of Rides") +
  theme_minimal()
  