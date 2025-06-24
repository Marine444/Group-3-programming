install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

data = merge_2


colnames(data) <- make.names(colnames(data))


data$Year <- as.integer(data$Year)


plot_data <- data %>%
  select(State, Year, price_to_income_ratio)


average_data <- plot_data %>%
  group_by(Year) %>%
  summarise(avg_ratio = mean(price_to_income_ratio, na.rm = TRUE))


filtered_data <- plot_data %>%
  filter(State %in% top_states)


ggplot() +
  geom_line(data = filtered_data, aes(x = Year, y = price_to_income_ratio, color = State), size = 1.2) +
  geom_line(data = average_data, aes(x = Year, y = avg_ratio, color = "Mean"), size = 1.3, linetype = "dashed") +
  scale_color_manual(values = c("California" = "blue", "Texas" = "green", "New York" = "red",
                                "Florida" = "purple", "Illinois" = "orange", "Mean" = "black")) +
  labs(title = "Price-to-Income Ratio per State with National mean",
       x = "Year", y = "Ratio", color = "State") +
  theme_minimal() +
  geom_vline(xintercept = 2008, linetype = "solid", color = "black") +
  annotate("text" , x= 2008.7 , y = 22, size = 4.5, label = "Financial crisis\nhit")



# ggsave("Temporal_Visualization.png", width = 10, height = 6)






