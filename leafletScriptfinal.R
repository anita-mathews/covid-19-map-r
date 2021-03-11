library(leaflet)
library(rgeos)
library(dplyr)
library(viridis)
library(RColorBrewer)
library(httr)
library(geojsonio)
library(geojson)
library(raster)
library(ggplot2)
library(scales)
library(ggeasy)
#calculated centroids and merged it with the data frame
# trueCentroids = gCentroid(sids,byid=TRUE)
#load in the datasets
res_toronto <- geojson_read("https://opendata.arcgis.com/datasets/5fa0e4d94e3147b1b957b0d84641c7a1_0.geojson", what="sp")
spdf <- geojson_read("https://opendata.arcgis.com/datasets/2177d6c6562247319edede041fd439af_10.geojson", what="sp")
res_peel <- geojson_read("https://opendata.arcgis.com/datasets/1f7fe2d6295346cb9ad2dd408edba6e4_0.geojson", what="sp")
res_york <- geojson_read("https://opendata.arcgis.com/datasets/834537fee6d541a5a7e9a404d9da648a_1.geojson", what="sp")

res_york_groups = aggregate(res_york, by="NAME")


covid_cases_toronto <- read.csv("COVID19 cases.csv")
covid_cases_york <- read.csv("YR_CaseData.csv")

#find the centroids of the polygons
spdf_data <- as.data.frame(spdf)
peel_data <- as.data.frame(res_peel)

centroids <- gCentroid(spdf, byid=TRUE)
spdf_data$x = centroids$x
spdf_data$y = centroids$y

centroids_toronto <- gCentroid(res_toronto, byid=TRUE)
#need to update summary dataframe with centroids and number of cases
# per neighbourhood
toronto_df <- as.data.frame(res_toronto)
toronto_df$x <- centroids_toronto$x
toronto_df$y <- centroids_toronto$y

centroids_peel <- gCentroid(res_peel, byid=TRUE)
peel_data$x <- centroids_peel$x
peel_data$y <- centroids_peel$y

centroids_york <- gCentroid(res_york_groups, byid=TRUE)
york_df <- as.data.frame(res_york_groups)
york_df$x <- centroids_york$x
york_df$y <- centroids_york$y

toronto_df <- toronto_df[, c(1, 2, 4, 40, 41)]
# find the number of cases in each neighbourhood
toronto_summary <- covid_cases_toronto %>%
  group_by(Neighbourhood.Name) %>%
  summarise(n= n_distinct(X_id))

york_summary <- covid_cases_york %>%
  group_by(Municipality) %>%
  summarize(n = n())

toronto_infection <- covid_cases_toronto %>% count(Source.of.Infection)
york_infection <- covid_cases_york %>% count(Acquisition)

toronto_outcome <- covid_cases_toronto %>% count(Outcome)
york_outcome <- covid_cases_york %>% count(Status)

tor_inf_df <- data.frame(group=toronto_infection$Source.of.Infection,
                         value=toronto_infection$n) %>% mutate(pcent=round(value/sum(value)*100, digits=1))

tor_out_df <- data.frame(group=toronto_outcome$Outcome,
                         value=toronto_outcome$n) %>% mutate(pcent=round(value/sum(value)*100, digits=1))

yor_inf_df <- data.frame(group=york_infection$Acquisition,
                         value=york_infection$n) %>% mutate(pcent=round(value/sum(value)*100, digits=1))

yor_out_df <- data.frame(group=york_outcome$Status,
                         value=york_outcome$n) %>% mutate(pcent=round(value/sum(value)*100, digits=1))

merged_toronto <- merge(toronto_df, toronto_summary, by.x="Neighbourhood", by.y="Neighbourhood.Name")
merged_york <- merge(york_summary, york_df, by.x="Municipality", by.y="NAME")
#have multiple Georginas, need to group polygons together by name?

#only the regions that are actually have data
res_york_groups_req <- res_york_groups[res_york_groups$NAME %in% merged_york$Municipality, ]


