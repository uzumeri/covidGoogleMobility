library(tidyverse)
library(choroplethrMaps)
library(jsonlite)

setwd("G:/My Drive/Statistical Analysis/RCOVID/COVIDDataAssembly")

ct <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv",stringsAsFactors = FALSE) %>%
  rename(codeaths = deaths, cocases = cases) %>%
  select(date, fips, cocases, codeaths, county, state)
ct$date <- as.Date(ct$date, format="%Y-%m-%d")
ct$costate <- paste(ct$state, "," , ct$county, sep="")
write.csv(ct,"./data/ct.csv")

st <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv", stringsAsFactors = FALSE)
st$date <- as.Date(st$date, format="%Y-%m-%d")
st$state <- tolower(st$state)

write.csv(st,"./data/st.csv")

gm <- read.csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", stringsAsFactors = FALSE) %>% 
  filter(country_region_code == "US")

gmusa <- gm %>% filter(sub_region_1 == "")

gmstate <- gm %>% filter(sub_region_1 != "" & sub_region_2 == "") %>% rename(state = sub_region_1)
gmstate$state <- tolower(gmstate$state)
gmstate$date <- as.Date(gmstate$date, format = "%Y-%m-%d")

gmcounty <- gm %>% filter(sub_region_1 != "" & sub_region_2 != "") %>% rename(state = sub_region_1, county = sub_region_2)
gmcounty$state <- tolower(gmcounty$state)
gmcounty$county <- tolower(sub(" County", "", gmcounty$county, fixed  = TRUE))
gmcounty$county <- tolower(sub(" parish", "", gmcounty$county, fixed  = TRUE))

data(county.regions)
countyfips <- county.regions %>% 
  rename(fips = region, state = state.name, county = county.name) %>% 
  select(state, county, fips)

gmcounty <- gmcounty %>% 
  left_join(countyfips, by = c("state","county"))

write.csv(gmusa, "./data/gmusa.csv")
write.csv(gmstate, "./data/gmstate.csv")
write.csv(gmcounty, "./data/gmcounty.csv")
