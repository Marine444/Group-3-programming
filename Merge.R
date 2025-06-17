library(tidyverse)
library(readxl)
library(stringr)

#Changing both data sets to data frame 
tabn025 = as.data.frame(tabn025)
hpi_at_state = as.data.frame(hpi_at_state)

#Filtering the HPI dataset for 20006-2010 
hpi_filtered = hpi_at_state %>%
  filter(Year %in% c(2006,2007,2008,2009,2010)) %>%
  select(State, Year, HPI)

#Filtering the Income data set for 2006-2010
income_to_use = select(tabn025,State,"2006","2007","2008","2009","2010")
#Formatting the income data set into a long format
income_long = income_to_use %>%
  pivot_longer(
    cols = -State,
    names_to = "Year",
    values_to = "Income"
  ) 
#Omitting the NA rows since we dont need 
income_long_no_NA = na.omit(income_long)
#Making the State titles the same 
income_long_no_NA$State = income_long_no_NA$State %>%
  str_replace_all("[.]", "") %>%
  str_squish() %>%
  str_trim()
#Making the Year the same numeric class 
hpi_filtered$Year = as.numeric(hpi_filtered$Year)
income_long_no_NA$Year = as.numeric(income_long_no_NA$Year)
#merging the 2 data sets
merge_4 = inner_join(hpi_filtered , income_long_no_NA, by = c("State","Year"))

