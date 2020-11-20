# Assignment 3 -- Analyzing Incarceration data

#Set up 
library(tidyverse)
library(scales)
data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# ----------------Summary Info section----------------

# Get name of location with the largest Native American jail population in 2016
place_top_na_jail_pop_2016 <- data %>%
  mutate(location = paste(county_name, state, sep = ", ")) %>% 
  filter(year == 2016) %>% 
  filter(native_jail_pop == max(native_jail_pop, na.rm = T)) %>% 
  pull(location)

#Get population of Native Americans in jail in above location
top_native_jail_pop <- data %>% 
  filter(year == 2016) %>% 
  mutate(location = paste(county_name, state, sep = ", ")) %>% 
  filter(location == place_top_na_jail_pop_2016) %>% 
  pull(native_jail_pop)

# Get the year with the most prison admissions
year_most_prison_adm <- data %>% 
  group_by(year) %>% 
  summarize(num_prison_adm = sum(total_prison_adm, na.rm = T)) %>% 
  filter(num_prison_adm == max(num_prison_adm, nam.rm = T)) %>% 
  pull(year)

# Get the number of prison admissions in year with most admissions
num_prison_adm_largest_year <- data %>% 
  group_by(year) %>% 
  summarize(num_prison_adm = sum(total_prison_adm, na.rm = T)) %>% 
  filter(year == year_most_prison_adm) %>% 
  pull(num_prison_adm)

# Get the average black prison population (across all US counties) in 2016
avg_black_prison_pop_2016 <- data %>% 
  group_by(year) %>% 
  summarize(avg_black_prison_pop = mean(black_prison_pop, na.rm = T)) %>% 
  filter(year == 2016) %>% 
  pull(avg_black_prison_pop)

# Function takes year as input, returns the total U.S population for 
# the given year.
num_prison_pop_in_year <- function(yr) {
  result <- data %>% 
    group_by(year) %>% 
    summarize(num_prison_pop = sum(total_prison_pop, na.rm = T)) %>% 
    filter(year == yr) %>% 
    pull(num_prison_pop)
  result
}

# Get U.S prison population change between 2000 and 2010
prison_pop_change_2000_to_2010 <- round(num_prison_pop_in_year(2010) - 
  num_prison_pop_in_year(2000), 0)


# ----------------Trend Plot section----------------

# Get the top 5 Louisiana counties by avg jail population over the time period 
# of 1970-2018 (period of data collection)
top_5_counties_la <- data %>% 
  filter(state == "LA") %>% 
  group_by(county_name) %>% 
  summarize(avg_jail_pop = mean(total_jail_pop, na.rm = T)) %>% 
  slice_max(avg_jail_pop, n = 5) %>% 
  pull(county_name)

# return a data frame of the filtered Louisiana counties 
filtered_counties <- data %>% 
  filter(county_name %in% top_5_counties_la)

# Render a trend plot of jail incarceration rates over time 
trend_plot <- ggplot(filtered_counties, mapping = 
                aes(x = year, y = total_jail_pop_rate, color = county_name)) +
  geom_point() +
  geom_smooth() +
  labs(
    title = "Jail Incarceration Rate per Year of Top 5 Louisiana Counties",
    x = "Year",
    y = "Jail Incarceration Rate (per 100,000 residents age 15-64)",
    color = "County"
  )


# ----------------Variable Comparison Plot Section----------------

# Get desired incarceration data
filtered_data <- data %>% 
  filter(year == 2018) %>% #just get 2018 rows
  filter(urbanicity != "") %>% # remove rows with blank category urbanity
  filter(is.na(total_jail_pop) == F, is.na(jail_rated_capacity) == F)

# Render comparison scatter plot matrix
comparison_plot <- ggplot(filtered_data, ) +
  geom_point(
    mapping = aes(x = jail_rated_capacity, y = total_jail_pop,
                  color = urbanicity),
    alpha = 0.3 # opacity of points
  )  +
  xlim(0, 12500) + # manual limits -- very few data points beyond
  ylim(0, 8000) +
  labs(
    title = "Jail Capacity versus Total Jail Population (2018)",
    x = "Jail Capacity",
    y = "Total Jail Population",
    color = "Urbanity"
  )


# ----------------Map Plot Section----------------
  
# Code borrowed from Ch.16 of textbook 
# Define a minimalist theme for maps
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

# load incarceration data
data_2018 <- data %>%  #keep only 2018 data
  rename(county = county_name) %>% 
  mutate(location = paste(county, state, sep = ", ")) %>% # mutate for join
  filter(year == 2018) %>% 
  group_by(state) %>% 
  summarize(
    black_jail_prop = mean(black_jail_pop / total_jail_pop, na.rm = T)) 

# Load shapefile of U.S States
state_shape <- map_data("state") %>% 
  rename(state = region) %>% 
  mutate(state = str_to_title(state)) %>% 
  mutate(state = state.abb[match(state, state.name)]) %>% # mutate for join
  left_join(data_2018, by = "state")

# Create a blank map of U.S. states
map_plot <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, 
                  fill = black_jail_prop),
    color = "white", # show state outlines
    size = .1        # thinly stroked
  ) +
  coord_map() + # use a map-based coordinate system
  scale_fill_continuous(low = "#132B43", high = "Red", labels = percent) +
  labs(
    title = 
      "Proportion of Blacks in Jail Populations by State (2018)",
    fill = "% Black Jail Population") +
  blank_theme