library("tidyverse")
library("lubridate")
library("ggplot2")
library("dplyr")
library("stringr")

q1 <- read.csv("C:/Users/saran/Desktop/data analysis projects/case study/Cyclistic/data_quarters/Q1_2024_cleaned.csv")

q1_cleaned <- q1 %>%
  filter(ride_duration != "00:00:00")

q1_cleaned %>%
  arrange((ride_duration))

length(which(q1_cleaned$ride_duration > "00:50:00"))
length(which(q1_cleaned$ride_duration < "00:02:00"))

nrow(q1_cleaned)
q1_cleaned <- q1_cleaned[!(q1_cleaned$ride_duration > "00:50:00" | q1_cleaned$ride_duration < "00:02:00"), ]
nrow(q1_cleaned)
q1_cleaned %>%
  arrange(desc(ride_duration))

summary(q1_cleaned)

q1_cleaned$day_of_week <- weekdays(as.Date(q1_cleaned$start_date))

q1_cleaned

ggplot(q1_cleaned, mapping = aes(x = day_of_week, fill = day_of_week))+
  geom_bar()+
  labs(title = "Number of rides per weekday", x = "Weekday", y = "Count of rides")+
  theme_light()

q1_cleaned$day_of_week <- factor(
  q1_cleaned$day_of_week,
  levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
)

q1_cleaned %>%
  count(day_of_week, member_casual) %>%
  ggplot(aes(day_of_week, n, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(
    title = "Number of Rides per Weekday by Member Type",
    x = "Weekday",
    y = "Count of Rides",
    fill = "Member Type"
  ) +
  theme_light()


rides_summary <- q1_cleaned %>%
  count(day_of_week, member_casual)


rides_wide <- q1_cleaned %>%
  count(day_of_week, member_casual) %>%
  pivot_wider(
    names_from = member_casual,
    values_from = n,
    names_prefix = "rides_"
  )


# Order weekdays
rides_summary$day_of_week <- factor(
  rides_summary$day_of_week,
  levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
)

# Plot bars + line
ggplot(rides_summary, aes(x = day_of_week, y = n, group = member_casual, fill = member_casual, color = member_casual)) +
  geom_col(position = "dodge", alpha = 0.7) +  # bars
  geom_line(aes(group = member_casual), position = position_dodge(width = 0.9), size = 1.2) + # line over bars
  geom_point(position = position_dodge(width = 0.9), size = 2) + # points for clarity
  labs(
    title = "Number of Rides per Weekday with Flow Line",
    x = "Weekday",
    y = "Count of Rides",
    fill = "Member Type",
    color = "Member Type"
  ) +
  theme_light()



# 1. number of rides per member type

q1_cleaned %>%
  count(member_casual)

# 1. number of rides per bike type
q1_cleaned %>%
  count(rideable_type)

q1_cleaned %>%
  count(rideable_type, member_casual)

q1_cleaned %>%
  mutate(ride_duration_secs = as.numeric(hms(ride_duration))) %>%
  group_by(member_casual) %>%
  summarise(
    avg_ride_duration = mean(ride_duration_secs, na.rm = TRUE) / 60,
    median_ride_duration = median(ride_duration_secs, na.rm = TRUE) / 60
  )

q1_cleaned %>%
  group_by(day_of_week) %>%
  summarise(rides = n())

q1_cleaned %>%
  group_by(day_of_week, member_casual) %>%
  summarise(rides = n()) %>%
  pivot_wider(names_from = member_casual, values_from = rides) %>%
  rename_with(~ paste0(.x, "_rides"), -day_of_week)

q1_cleaned %>%
  group_by(start_hour) %>%
  summarise(rides = n()) %>%
  arrange(desc(rides))


q1_cleaned %>%
  group_by(start_hour) %>%
  summarise(rides = n()) %>%
  arrange(desc(rides))


q1_cleaned %>%
  group_by(start_station_name) %>%
  summarise(rides = n()) %>%
  arrange(desc(rides))

q1_cleaned %>%
  filter(member_casual == 'member')%>%
  group_by(start_station_name) %>%
  summarise(rides = n()) %>%
  arrange(desc(rides))

q1_cleaned %>%
  filter(member_casual == 'casual')%>%
  group_by(start_station_name) %>%
  summarise(rides = n()) %>%
  arrange(desc(rides))

