library(tidyverse)
library(jsonlite)
library(choroplethr)
library(choroplethrMaps)
library(maps)
# ct <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv",stringsAsFactors = FALSE)  
# write.csv(ct,"./ct.csv")

setwd("G:/My Drive/Statistical Analysis/ShinyCovidTracking")

ct <- read.csv("ct.csv",stringsAsFactors = FALSE) %>%
  rename(codeaths = deaths,cocases = cases)
  ct$date <- as.Date(ct$date,format="%Y-%m-%d")
  ct$costate <- paste(ct$county, " ",ct$state)

stc <- fromJSON("https://covidtracking.com/api/v1/states.json") %>% rename(stpos = positive, stneg = negative)

data(df_county_demographics)
data(county.regions)
mcounty <- map_data("county")

countybig <- ct %>% 
          left_join(df_county_demographics, by=c("fips"="region")) %>% 
          right_join(county.regions,by=c("fips"="region")) %>% 
          left_join(stc, by=c("state.abb"="state")) %>% 
          rename(region = fips.x)

county <- countybig %>% 
          select(date,county,state,costate,region,cocases,codeaths,total_population,median_rent,median_age,percent_white,stpos,stneg,totalTestResults,hospitalizedCurrently,recovered) %>% 
          group_by(region) %>% arrange(region,date) %>% mutate(comav2 = (cocases + lag(cocases))/2)

rm(state.regions,df_county_demographics,countybig,stc,ct)

countydate <- county %>% filter(date =="2020-03-30" & !is.na(region)) %>% rename(value=comav2) %>% mutate(if_else(is.na(value),0,value))

chp <- county_choropleth(countydate,num_colors=9)
chp
