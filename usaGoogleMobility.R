library(tidyverse)
library(jsonlite)
library(plotly)
library(choroplethrMaps)
library(RColorBrewer)

setwd("G:/My Drive/Statistical Analysis/RCOVID/googlemobility")

data(county.regions)

us_map_ref <-  county.regions %>% mutate(state.fips = as.numeric(state.fips.character))

cmap <- map_data("county") %>% 
  rename(state.name = region, county.name = subregion) %>% 
  left_join(us_map_ref, by=c("state.name","county.name")) %>% 
  rename(state = state.name, county = county.name, fips = region) %>% 
  select(long, lat, group, order, state, county, fips, state.fips, state.abb)

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
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      # plot.background = element_rect(fill = "#404040", color = NA),
      panel.background = element_rect(fill = "#94a9d1", color = NA),
      # legend.background = element_rect(fill = "#ffffff", color = NA),
      legend.position=c(0.9,0.3),
      panel.border = element_blank(),
      plot.title = element_text("Times", color= "#22211d" ),
      ...
    )
}

gmcounty <- read.csv("./data/gmcounty.csv") 
gmcounty$date <- as.Date(gmcounty$date, format="%Y-%m-%d")

gdates <- gmcounty %>% distinct(date)

gmcounty <- gmcounty %>% 
  arrange(state, county, date) %>% 
  group_by(state, county) %>% 
  rename(discretionary = retail_and_recreation_percent_change_from_baseline) %>% 
  rename(worktrips = workplaces_percent_change_from_baseline) %>% 
  rename(stayhomes = residential_percent_change_from_baseline) %>% 
  mutate(d2rate = discretionary) %>% 
  mutate(w2rate = worktrips) %>% 
  mutate(s2rate = stayhomes) %>% 
  select(date, fips, discretionary, d2rate, w2rate, worktrips, stayhomes, s2rate, state, county) 

pala <- brewer.pal(n=8, name="RdYlGn")
pala[5] <- "#edf2da"

gmcounty$abreaks <- cut(gmcounty$d2rate, 
                        breaks = c(-Inf,-60,-40, -20, 0, 20, 40, Inf),
                        labels = c("<-60", "-60 to -40", "-40 to -20", "-20 to 0", "0 to 20", "20 to 40", "40+"))

gmcounty$wbreaks <- cut(gmcounty$w2rate,
                        breaks = c(-Inf,-60,-40, -20, 0, 20, 40, Inf),
                        labels = c("<-60", "-60 to -40", "-40 to -20", "-20 to 0", "0 to 20", "20 to 40", "40+"))

pals <- brewer.pal(n=8, name="RdYlGn")
pals[4] <- "#f0ffeb"

gmcounty$sbreaks <- cut(gmcounty$s2rate,
                        breaks = c(-Inf,-20,-10, 0, 10, 20, 30, Inf),
                        labels = c("<-20", "-20 to -0", "-10 to 0", "0 to 10", "10 to 20", "20 to 30", "30+"))

mapaction <- gmcounty %>% 
  right_join(cmap, by="fips")

pdf("./outmaps/usaactivityretail.pdf", width=11)
for (i in (1:nrow(gdates))) {
  
  temp <- mapaction %>% filter(date == gdates$date[i])
  
  dt <- format(gdates$date[i], "%a %b-%d")
  ti <- "Google Community Mobility Report - Change in Mobility to Retail and Recreation Destinations "
  src1 <- "Google LLC Google COVID-19 Community Mobility Reports: https://www.google.com/covid19/mobility/"
  src2 <- "Analysis and Graphics by Vic Uzumeri, PhD - 05/2020"
  chg <- "Change relative to Jan 3 - Feb 6, 2020 Baseline"
  
  p <- ggplot(temp) +
    theme_map() +
    geom_polygon(aes(long, lat, group=group, fill=abreaks)) +
    scale_fill_manual("Mobility\nChange (%)", drop=FALSE, values=rev(pala), na.value="#C0C0C0") +
    guides(fill=guide_legend(order=9)) +
    annotate("text", x = -79, y = 48, size = 8, label=dt, color="#ffffff") +
    annotate("text", x = -95, y = 52, size = 6, label=ti, color="#ffffff") +
    annotate("text", x = -95, y = 51, size = 3, label=src1, color="#ffffff") +
    annotate("text", x = -95, y = 50, size = 3, label=chg, color="#ffffff") +
    annotate("text", x = -115, y = 25, size = 3, label=src2, color="#ffffff") +
    borders("state", color="Gray") 
  
  print(p)
}
dev.off()

pdf("./outmaps/usaactivitywork.pdf", width=11)
for (i in (1:nrow(gdates))) {
  
  temp <- mapaction %>% filter(date == gdates$date[i])
  
  dt <- format(gdates$date[i], "%a %b-%d")
  ti <- "Google Community Mobility Report - Change in Mobility to Work Destinations "
  src1 <- "Google LLC Google COVID-19 Community Mobility Reports: https://www.google.com/covid19/mobility/"
  src2 <- "Analysis and Graphics by Vic Uzumeri, PhD - 05/2020"
  chg <- "Change relative to Jan 3 - Feb 6, 2020 Baseline"
  
  p <- ggplot(temp) +
    theme_map() +
    geom_polygon(aes(long, lat, group=group, fill=wbreaks)) +
    scale_fill_manual("Mobility\nChange (%)", drop=FALSE, values=rev(pala), na.value="#C0C0C0") +
    guides(fill=guide_legend(order=2)) +
    annotate("text", x = -79, y = 48, size = 8, label=dt, color="#ffffff") +
    annotate("text", x = -95, y = 52, size = 6, label=ti, color="#ffffff") +
    annotate("text", x = -95, y = 51, size = 3, label=src1, color="#ffffff") +
    annotate("text", x = -95, y = 50, size = 3, label=chg, color="#ffffff") +
    annotate("text", x = -115, y = 25, size = 3, label=src2, color="#ffffff") +
    borders("state", color="Gray") 
  
  print(p)
}
dev.off()

pdf("./outmaps/usaactivitystayhome.pdf", width=11)
for (i in (1:nrow(gdates))) {
  
  temp <- mapaction %>% filter(date == gdates$date[i])
  
  dt <- format(gdates$date[i], "%a %b-%d")
  ti <- "Google Community Mobility Report - Change in Mobility for Residences"
  src1 <- "Google LLC Google COVID-19 Community Mobility Reports: https://www.google.com/covid19/mobility/"
  src2 <- "Analysis and Graphics by Vic Uzumeri, PhD - 05/2020"
  chg <- "Change relative to Jan 3 - Feb 6, 2020 Baseline"
  
  p <- ggplot(temp) +
    theme_map() +
    geom_polygon(aes(long, lat, group=group, fill=sbreaks)) +
    scale_fill_manual("Mobility\nChange (%)", drop=FALSE, values=pals, na.value="#C0C0C0") +
    guides(fill=guide_legend(order=0)) +
    annotate("text", x = -79, y = 48, size = 8, label=dt, color="#ffffff") +
    annotate("text", x = -95, y = 52, size = 6, label=ti, color="#ffffff") +
    annotate("text", x = -95, y = 51, size = 3, label=src1, color="#ffffff") +
    annotate("text", x = -95, y = 50, size = 3, label=chg, color="#ffffff") +
    annotate("text", x = -115, y = 25, size = 3, label=src2, color="#ffffff") +
    borders("state", color="Gray") 
  
  print(p)
}
dev.off()

