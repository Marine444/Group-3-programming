
library(ggplot2)
library(dplyr)

# Make sure Year is numeric
merge_2 <- merge_2 %>%
  mutate(Year = as.numeric(Year))

# Filter for 2008
merge_2_filtered <- merge_2 %>%
  filter(Year == 2008)

# Summarize counts + compute percentages within each region
summary_data <- merge_2_filtered %>%
  group_by(region, link_price_to_income) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(region) %>%
  mutate(percent = count / sum(count) * 100) %>%
  ungroup()

# Plot
ggplot(summary_data, aes(x = region, y = percent, fill = link_price_to_income)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(
    title = "Income & HPI change categories by region (2008)",
    x = "region",
    y = "Percentage of cases",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# check if year is 2008

table(merge_2_filtered$Year)
