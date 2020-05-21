library(tidyverse)
library(jsonlite)
library(plotly)
library(choroplethr)
library(choroplethrMaps)
library(naniar)
library(USAboundaries)
library(RColorBrewer)
library(png)

setwd("G:/My Drive/Statistical Analysis/ShinyCovidTracking")

ct <- read.csv("ct.csv", stringsAsFactors = FALSE) 
ct <- ct %>% mutate(fips = ifelse(county == "New York City", 36061, fips))
ct$date <- as.Date(ct$date, format="%Y-%m-%d")

dates <- ct %>% distinct(date)

data(county.regions) 

us_map_ref <-  county.regions %>% mutate(state.fips = as.numeric(state.fips.character))

cmap <- map_data("county") %>% 
  rename(state.name = region, county.name = subregion) %>% 
  left_join(us_map_ref, by=c("state.name","county.name")) %>% 
  rename(state = state.name, county = county.name, fips = region)

stc <- read.csv("statedaily.csv", stringsAsFactors = FALSE) %>% 
  rename(stpos = positive, stneg = negative, ndate = date) %>% 
  select(state, stpos, stneg, ndate, fips, totalTestResults)
stc$date <- as.Date(as.character(stc$ndate),format="%Y%m%d")

data(df_county_demographics)
data(state.regions)

countybig <- ct %>% 
  left_join(df_county_demographics, by=c("fips"="region")) %>% 
  left_join(county.regions, by=c("fips"="region")) %>% 
  left_join(stc, by=c("state.abb"="state", "date")) %>%  
  rename(fips = fips.x) 
  countybig[countybig$county == "New York City","total_population"] <- 8398748

county <- countybig %>% 
  select(date,county,state,costate,fips,cocases,codeaths,total_population,median_rent,median_age,percent_white,stpos,stneg,totalTestResults) %>% 
  group_by(fips) %>% 
  arrange(fips,date) %>%
  mutate(comav2 = (cocases - lag(cocases,2))/2) %>% 
  mutate(prate = comav2/(total_population/100000)) %>% 
  mutate(dmav2 = (codeaths - lag(codeaths,2))/2) %>% 
  mutate(drate = dmav2/(total_population/100000))

replace_with_na(county, replace = list(county$prate < 0))

nyc <- county %>% filter(fips == 36061) %>% select(date,comav2,prate,dmav2,drate,total_population,cocases,codeaths) 
write.csv(nyc,"./outmaps/nyc.csv")

palc <- brewer.pal(n=7, name="Reds")
county$cbreaks <- cut(county$prate,
     breaks = c(0, 2, 10, 30, 75, 100, 150, Inf),
     labels = c("0-2", "2-10", "10-30", "30-75", "75-100", "100-150", "150+"))

pald <- brewer.pal(n=7, name="Greys")
county$dbreaks <- cut(county$drate,
                      breaks = c(0, 1, 3, 5, 8, 12, 18, Inf),
                      labels = c("0-1", "1-3", "3-5", "5-8", "8-12", "12-18", "18+"))

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Times", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      # panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#ffffff", color = NA), 
      panel.background = element_rect(fill = "#ffffff", color = NA), 
      legend.background = element_rect(fill = "#ffffff", color = NA),
      panel.border = element_blank(),
      plot.title = element_text("Times", color= "#22211d" ),
      ...
    )
}


mapcases <- county %>% 
  mutate(prate = replace_na(prate,0)) %>% 
  select(date, fips, prate, comav2, dmav2, drate, totalTestResults, cbreaks, dbreaks) %>% 
  right_join(cmap, by="fips")

pdf("./outmaps/usacases.pdf", width=11)
for (i in (99:nrow(dates))) {
  
  temp <- mapcases %>% filter(date == dates$date[i])

  dt <- format(dates$date[i], "%b-%d")
  ti <- "2-Day Moving Average of New Cases per 100,000 Population by County "
  src1 <- "Data from The New York Times, based on reports from state and local health organizations"
  src2 <- "Analysis and Graphics by Vic Uzumeri, PhD"

  p <- ggplot(temp) +
    theme_map() +
    geom_polygon(aes(long, lat, group=group, fill=cbreaks)) +
    scale_fill_manual("New Cases\nPer 100,000", drop=FALSE, values=palc) +
    annotate("text", x = -80, y = 48, size = 8, label=dt) +
    annotate("text", x = -100, y = 52, size = 6, label=ti) +
    annotate("text", x = -100, y = 51, size = 3, label=src1) +
    annotate("text", x = -120, y = 25, size = 3, label=src2) +
    # theme_map() +
    theme(legend.position=c(0.9,0.3)) +
    borders("state", color="Gray") 
  
  print(p)
}
dev.off()

mapdeaths <- county %>% 
  mutate(drate = replace_na(drate,0)) %>% 
  select(date, fips, drate, dmav2, totalTestResults, dbreaks) %>% 
  right_join(cmap, by="fips")

pdf("./outmaps/usadeaths.pdf", width=11)
for (i in (99:nrow(dates))) {
  
  temp <- mapdeaths %>% filter(date == dates$date[i])
  
  dt <- format(dates$date[i], "%b-%d")
  ti <- "2-Day Moving Average of Deaths per 100,000 Population by County "
  src1 <- "Data from The New York Times, based on reports from state and local health organizations"
  src2 <- "Analysis and Graphics by Vic Uzumeri, PhD"
  
  p <- ggplot(temp) +
    theme_map() +
    geom_polygon(aes(long, lat, group=group, fill=dbreaks)) +
    scale_fill_manual("Deaths\nPer 100,000", drop=FALSE, values=pald) +
    annotate("text", x = -80, y = 48, size = 8, label=dt) +
    annotate("text", x = -100, y = 52, size = 6, label=ti) +
    annotate("text", x = -100, y = 51, size = 3, label=src1) +
    annotate("text", x = -120, y = 25, size = 3, label=src2) +
    # theme_map() +
    theme(legend.position=c(0.9,0.3)) +
    borders("state", color="Gray") 
  
  print(p)
}
dev.off()

gmcounty <- read.csv("gmcounty.csv")
gmcounty$date <- as.Date(gmcounty$date, format="%Y-%m-%d")

gdates <- gmcounty %>% distinct(date)

gmcounty <- gmcounty %>% 
  rename(discretionary = retail_and_recreation_percent_change_from_baseline) %>% 
  mutate(discretionary = replace_na(discretionary,0)) %>% 
  select(date, fips, discretionary, state, county) 

pala <- brewer.pal(n=8, name="RdYlGn")
gmcounty$abreaks <- cut(gmcounty$discretionary,
                        breaks = c(-Inf,-60,-40, -20, 0, 20, 40, Inf),
                        labels = c("<-60", "-60 to -40", "-40 to -20", "-20 to 0", "0 to 20", "20 to 40", "40+"))

mapaction <- gmcounty %>% 
  right_join(cmap, by="fips")

pdf("./outmaps/usaactivity.pdf", width=11)
for (i in (1:nrow(gdates))) {
  
  temp <- mapaction %>% filter(date == gdates$date[i])
  
  dt <- format(gdates$date[i], "%a %b-%d")
  ti <- "Google Community Mobility Report - Change in Mobility to Retail and Recreation Destinations "
  src1 <- "Google LLC Google COVID-19 Community Mobility Reports: https://www.google.com/covid19/mobility/"
  src2 <- "Analysis and Graphics by Vic Uzumeri, PhD"
  
  p <- ggplot(temp) +
    theme_map() +
    geom_polygon(aes(long, lat, group=group, fill=abreaks)) +
    scale_fill_manual("Mobility Change (%)", drop=FALSE, values=pala, na.value="gray95") +
    annotate("text", x = -80, y = 48, size = 8, label=dt) +
    annotate("text", x = -95, y = 52, size = 6, label=ti) +
    annotate("text", x = -95, y = 51, size = 3, label=src1) +
    annotate("text", x = -120, y = 25, size = 3, label=src2) +
    # theme_map() +
    theme(legend.position=c(0.9,0.3)) +
    borders("state", color="Gray") 
  
  print(p)
}
dev.off()
