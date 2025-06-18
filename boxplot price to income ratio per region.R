merge_2 <- merge_2 %>%
  mutate(
    region = case_when(
      State %in% c(
        # Combine Northeast + Midwest als North
        "Maine", "New Hampshire", "Vermont", "Massachusetts", "Rhode Island", "Connecticut",
        "New York", "New Jersey", "Pennsylvania",
        "Ohio", "Indiana", "Illinois", "Michigan", "Wisconsin", "Minnesota", "Iowa", 
        "Missouri", "North Dakota", "South Dakota", "Nebraska", "Kansas"
      ) ~ "North",
      
      State %in% c(
        # South
        "Delaware", "Maryland", "District of Columbia", "Virginia", "West Virginia",
        "North Carolina", "South Carolina", "Georgia", "Florida", "Kentucky", "Tennessee",
        "Mississippi", "Alabama", "Texas", "Arkansas", "Louisiana", "Oklahoma"
      ) ~ "South",
      
      State %in% c(
        # West
        "Montana", "Idaho", "Wyoming", "Colorado", "New Mexico", "Arizona", "Utah", "Nevada",
        "California", "Oregon", "Washington", "Alaska", "Hawaii"
      ) ~ "West",
      
      TRUE ~ "Other"
    )
  )

ggplot(merge_2, aes(x = region, y = price_to_income_ratio)) +
  geom_boxplot()

ggplot(merge_2, aes(x = region, y = price_to_income_ratio)) +
  geom_boxplot() +
  labs(
    title = "Price-to-Income Ratio per Region",
    x = "Region",
    y = "Price-to-Income Ratio"
  )

write.csv(merge_2,"~/GitHub/Group-3-programming/merge_2.csv")
