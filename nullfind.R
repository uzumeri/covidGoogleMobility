library(tidyverse)

gm <- read.csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", stringsAsFactors = FALSE) %>% 
  filter(country_region_code == "US")


null <- gm %>% filter(is.null(retail_and_recreation_percent_change_from_baseline))
nas <- gm %>% filter(is.na(retail_and_recreation_percent_change_from_baseline))
empty <- gm %>% filter(retail_and_recreation_percent_change_from_baseline == "")
