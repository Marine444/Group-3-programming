install.packages(c("ggplot2", "dplyr", "sf", "tigris", "viridis"))

# Packages
library(readr)
library(dplyr)
library(ggplot2)
library(viridis)
library(tigris)
library(sf)

options(tigris_use_cache = TRUE)

# Data inlezen
merge_2 <- read_csv("merge_2.csv")

# mapping van state name naar STUSPS
state_abbr <- tibble::tibble(
  State = state.name,
  STUSPS = state.abb
)

# DC handmatig toevoegen
state_abbr <- bind_rows(state_abbr, tibble(State = "District of Columbia", STUSPS = "DC"))

#  Abbreviatie toevoegen aan je dataframe
merge_2 <- merge_2 %>%
  left_join(state_abbr, by = "State")


# US shapefile ophalen en filteren
states_sf <- states(cb = TRUE) %>%
  filter(!STUSPS %in% c("PR", "VI", "GU", "MP", "AS")) %>%
  st_transform(crs = 5070)

# Merge shapefile en je data (alle jaren)
map_data <- left_join(states_sf, merge_2, by = "STUSPS")

#setting the limits for the scale of the heat map
hpi_min <- min(map_data$HPI, na.rm = TRUE)
hpi_max <- max(map_data$HPI, na.rm = TRUE)

# Filter voor 2006
map_data_2006 <- map_data %>% filter(Year == 2006)

# Plot 2006
ggplot(map_data_2006) +
  geom_sf(aes(fill = HPI), color = "white", size = 0.2) +
  scale_fill_viridis(option = "plasma", na.value = "grey90", limits = c(hpi_min, hpi_max)) +
  theme_minimal(base_size = 14) +
  labs(
    title = "House Price $ Index per state - 2006",
    fill = "HPI (Index)"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12)
  )


ggsave("map_2006.png", width = 10, height = 8, dpi = 300)

map_data_2010 <- map_data %>% filter(Year == 2010)

# Plot 2010
ggplot(map_data_2010) +
  geom_sf(aes(fill = HPI), color = "white", size = 0.2) +
  scale_fill_viridis(option = "plasma", na.value = "grey90", limits = c(hpi_min, hpi_max)) +
  theme_minimal(base_size = 14) +
  labs(
    title = "House Price $ Index per state - 2010",
    fill = "HPI (Index)"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12)
  )


ggsave("map_2010.png", width = 10, height = 8, dpi = 300)
