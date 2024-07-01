# Check current working directory if needed
getwd()

# Load necessary packages if not already loaded
library(tidyverse)

# Read in the CSV data (assuming you've already done this)
storm_data <- read_csv("StormEvents_details-ftp_v1.0_d1998_c20220425 (1).csv")

# Select specific columns
storm_data <- storm_data %>%
  select(BEGIN_YEARMONTH, EPISODE_ID, STATE, STATE_FIPS, CZ_NAME, CZ_TYPE, CZ_FIPS, EVENT_TYPE)

# Arrange the data by the state name (STATE)
storm_data <- storm_data %>%
  arrange(STATE)

# Change state and county names to title case
storm_data <- storm_data %>%
  mutate(STATE = str_to_title(STATE),
         CZ_NAME = str_to_title(CZ_NAME))

# Limit to events listed by county FIPS (CZ_TYPE of "C") and remove CZ_TYPE column
storm_data <- storm_data %>%
  filter(CZ_TYPE == "C") %>%
  select(-CZ_TYPE)

# Pad the STATE_FIPS and CZ_FIPS columns with "0" at the beginning
storm_data <- storm_data %>%
  mutate(STATE_FIPS = str_pad(STATE_FIPS, width = 2, side = "left", pad = "0"),
         CZ_FIPS = str_pad(CZ_FIPS, width = 3, side = "left", pad = "0"))

# Unite the STATE_FIPS and CZ_FIPS columns into one FIPS column
storm_data <- storm_data %>%
  unite("FIPS", STATE_FIPS, CZ_FIPS, sep = "")

# Change all column names to lowercase
storm_data <- storm_data %>%
  rename_all(tolower)

# Create dataframe with the number of events per state
events_per_state <- storm_data %>%
  group_by(state) %>%
  summarise(num_events = n())

# Merge with us_state_info dataframe
merged_data <- merge(events_per_state, us_state_info, by.x = "state", by.y = "state", all.x = TRUE)

# Remove states not in us_state_info
merged_data <- merged_data[!is.na(merged_data$region), ]

# View the merged dataframe
head(merged_data)

# Load necessary packages if not already loaded
library(ggplot2)

# Assuming `merged_data` contains the merged dataframe with state information
# Scatter plot
ggplot(merged_data, aes(x = area, y = num_events, label = region)) +
  geom_point() +  # Scatter plot points
  geom_text(vjust = -0.5) +  # Label adjustment
  labs(x = "Land Area (square miles)", y = "# of Storm Events in 1998", 
       title = "Storm Events vs. Land Area by Region") +  # Axis labels and title
  theme_minimal() +  # Minimalistic theme
  theme(plot.title = element_text(hjust = 0.5))  # Centered title

