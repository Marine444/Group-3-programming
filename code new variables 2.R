install.packages("tidyverse")
library("tidyverse")

#create variable 1: house prices to income ratio
merge_2 <- merge_2 %>%
  mutate(price_to_income_ratio = (HPI / Income)*1000)
merge_2

# create new variable of income_change and HPI_change

merge_2 <- merge_2 %>%
  arrange(State, Year) %>%  # ensure data is ordered
  group_by(State) %>%      # compute changes within each state
  mutate(
    income_change = Income - lag(Income),
    HPI_change = HPI - lag(HPI)
  ) %>%
  ungroup()


write.csv(merge_2, "merge_2.csv")


write.csv(merge_2,"merge_2.csv")

#create variable 2: link between income loss & housing price drop 

merge_2 <- merge_2 %>%
  mutate(
    link_price_to_income = case_when(
      income_change < 0 & `HPI_change` < 0  ~ "Both fell",
      income_change < 0 & `HPI_change` > 0 ~ "Income fell, HPI increased",
      income_change == 0 & `HPI_change` < 0 ~ "Income stable, HPI fell",
      income_change == 0 & `HPI_change` > 0 ~ "Income stable, HPI increased",
      income_change > 0 & `HPI_change` < 0 ~ "Income increased, HPI fell",
      income_change > 0 & `HPI_change` > 0 ~ "Income increased, HPI increased",
      TRUE ~ "Other"
    )
  )
merge_2

merge_2$link_price_to_income[merge_2$Year=="2006"] = "NA"

merge_2$link_price_to_income[merge_2$Year=="2006"] = "NA"


merge_2 <- read.csv("merged_dataset.csv")
