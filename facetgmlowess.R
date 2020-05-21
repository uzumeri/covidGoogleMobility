library(tidyverse)

fplot <- function(ds,v) {
  p <-ggplot(ds, aes(date,v)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(~ state)
  plot(p)
}
fplotns <- function(ds,v) {
  p <-ggplot(ds, aes(date,v)) +
    geom_point(color="Red") +
    facet_wrap(~ state)
  plot(p)
}

setwd("G:/My Drive/Statistical Analysis/RCOVID/googlemobility")

s <- st %>% group_by(date,state) %>% select(date, state, cases, deaths) %>% 
  filter(state %in% c("california","florida","georgia","michigan","missouri","mississippi","new york","texas","washington"))

g <- gmstate %>% group_by(date,state) %>% 
      rename(retail = retail_and_recreation_percent_change_from_baseline) %>% 
      rename(work = workplaces_percent_change_from_baseline)  %>% 
      rename(groceries = grocery_and_pharmacy_percent_change_from_baseline)  %>% 
      rename(home = residential_percent_change_from_baseline) %>% 
      rename(transit = transit_stations_percent_change_from_baseline) %>% 
      rename(parks = parks_percent_change_from_baseline) %>% 
      left_join(s, by=c("state","date")) %>% 
      filter(state %in% c("california","florida","georgia","michigan","missouri","mississippi","new york","texas","washington"))

# pdf("./graphs.pdf", width=11)
fplot(g, g$retail)
fplot(g, g$work)
fplot(g, g$groceries)
fplot(g, g$home)
fplot(g, g$parks)
fplot(g, g$transit)
fplotns(s, s$cases)
fplotns(s, s$deaths)
# dev.off()

