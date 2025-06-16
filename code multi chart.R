install.packages("tidyverse")
library(tidyverse)

data <- read.csv("merge_2.csv")

# Herbenoem kolommen als dat nodig is
colnames(data) <- make.names(colnames(data))

# Zorg dat Year een getal is
data$Year <- as.integer(data$Year)

# Filter alleen de benodigde kolommen
plot_data <- data %>%
  select(State, Year, price_to_income_ratio)


ggplot(plot_data, aes(x = Year, y = price_to_income_ratio, color = State)) +
  geom_line() +
  labs(title = "Price-to-Income Ratio per State Over Time",
       x = "Jaar",
       y = "Price-to-Income Ratio") +
  theme_minimal() +
  theme(legend.position = "none")  # optioneel: verberg de legende als er te veel staten zijn

top_states <- c("California", "Texas", "New York", "Florida", "Illinois")
filtered_data <- plot_data %>% filter(State %in% top_states)

ggplot(filtered_data, aes(x = Year, y = price_to_income_ratio, color = State)) +
  geom_line(size = 1.2) +
  labs(title = "Price-to-Income Ratio per State",
       x = "Jaar", y = "Ratio") +
  theme_minimal()

# Bereken het gemiddelde van alle staten per jaar
average_data <- plot_data %>%
  group_by(Year) %>%
  summarise(avg_ratio = mean(price_to_income_ratio, na.rm = TRUE))

top_states <- c("California", "Texas", "New York", "Florida", "Illinois")

filtered_data <- plot_data %>%
  filter(State %in% top_states)

ggplot() +
  geom_line(data = filtered_data, aes(x = Year, y = price_to_income_ratio, color = State), size = 1.2) +
  geom_line(data = average_data, aes(x = Year, y = avg_ratio), color = "black", size = 1.3, linetype = "dashed") +
  labs(title = "Price-to-Income Ratio per State (met Gemiddelde)",
       x = "Jaar", y = "Price-to-Income Ratio") +
  theme_minimal()

ggplot() +
  geom_line(data = filtered_data, aes(x = Year, y = price_to_income_ratio, color = State), size = 1.2) +
  geom_line(data = average_data, aes(x = Year, y = avg_ratio, color = "Mean"), size = 1.3, linetype = "dashed") +
  scale_color_manual(values = c("California" = "blue", "Texas" = "green", "New York" = "red",
                                "Florida" = "purple", "Illinois" = "orange", "Mean" = "black")) +
  labs(title = "Price-to-Income Ratio per State with National mean",
       x = "year", y = "Ratio", color = "State") +
  theme_minimal()


ggsave("Temporal_Visualization.png", width = 10, height = 6)






