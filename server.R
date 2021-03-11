library(shiny)
library(shinydashboard)
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
library(ggrepel)
source("leafletScriptfinal.R")

shinyServer <- function(input, output, session) {
  state <- reactiveValues()
  
  observe ({
    state$obs_regions <- input$region
    count = 0
    data_values = c()
    for (i in input$region) {
      if (i == "Hamilton") {
        data_values <- append(data_values, spdf_data$NUMBER_OF_CASES)
      }
      
      else if (i == "Toronto") {
        data_values <- append(data_values, merged_toronto$n)
      }
      
      else if (i == "York") {
        data_values <- append(data_values, merged_york$n)
      }
      
      else{
        data_values <- append(data_values, peel_data$Total)
      }
      
    }
    
    if (length(data_values) == 0) {data_values <- c(0, 0)}
    
    pal_tor <- colorBin(
      palette = "Spectral",
      bins=10,
      reverse = TRUE,
      domain = c(min(data_values), max(data_values)))
    
    output$covidMap <- renderLeaflet({ leaflet(spdf) %>%  
        addTiles(group = "default") %>%
        addPolygons(data=spdf, weight=2, 
                    color="black",
                    fillColor = ~pal_tor(NUMBER_OF_CASES), 
                    fillOpacity = 0.7,
                    group = "Hamilton",
                    popup=~paste("<strong>Number of Cases:</strong> ",NUMBER_OF_CASES,"<br><strong>Population:</strong> ",POPULATION),
                    labelOptions = labelOptions(noHide = T, textsize = "20px"),
                    highlight = highlightOptions(
                      weight = 2,
                      color = "white",
                      bringToFront = TRUE
                    )) %>%
        addPolygons(data=res_toronto, weight=2, fillColor =~pal_tor(merged_toronto$n),
                    color = "black",
                    fillOpacity = 0.7,
                    group="Toronto",
                    popup=~paste("<strong>Number of Cases:</strong> ",merged_toronto$n,"<br><strong>Population:</strong> ",Total_Population),
                    labelOptions = labelOptions(noHide = T, textsize = "20px"),
                    highlight = highlightOptions(
                      weight = 2,
                      color = "white",
                      bringToFront = TRUE
                    )) %>%
        addPolygons(data=res_peel, weight=2, fillColor =~pal_tor(peel_data$Total), group="Peel",
                    fillOpacity = 0.7,
                    color="black",
                    labelOptions = labelOptions(noHide = T, textsize = "20px"),
                    popup=~paste("<strong>Number of Cases:</strong> ",peel_data$Total,"<br><strong>Population:</strong> ",Population),
                    highlight = highlightOptions(
                      weight = 2,
                      color = "white",
                      bringToFront = TRUE
                    )) %>%
        addPolygons(data=res_york_groups_req, weight=2, fillColor =~pal_tor(merged_york$n), group="York",
                    fillOpacity = 0.7,
                    color="black",
                    popup=~paste("<strong>Number of Cases:</strong> ",merged_york$n,"<br><strong>Municipality:</strong> ",merged_york$Municipality),
                    labelOptions = labelOptions(noHide = T, textsize = "20px"),
                    highlight = highlightOptions(
                      weight = 2,
                      color = "white",
                      bringToFront = TRUE
                    )) %>%
        addLegend(title = "Number of Cases", pal =pal_tor,
                  values = ~c(merged_toronto$n, peel_data$Total, spdf_data$NUMBER_OF_CASES,
                              merged_york$n), opacity = 1) %>%
        setView(lat = 43.6532, lng=-79.3832, zoom = 8.5)
      #layers control#
      #addLayersControl()
    })#end of leaflet
    
  })#end of observe
  
  observe({
    proxy <- leafletProxy("covidMap")
    
    #hide all groups - find a better way to do this
    proxy %>% hideGroup("Hamilton") %>%
      hideGroup("Toronto") %>%
      hideGroup("York") %>%
      hideGroup("Peel")
    
    proxy %>% showGroup("default")
    
    count = 0
    for (i in input$region) {
      proxy %>% showGroup(i)
      count = count + 1
    }
    
    if (count == 0) {
      proxy %>% clearControls()
    }
  })
  
  output$TorontoInfection <- renderPlot({ggplot(tor_inf_df, 
                                                aes(x="", y=value, fill=paste(group, pcent,"%") )) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) + theme_void() +
      scale_fill_brewer(palette="PiYG") +
      geom_text_repel(aes(x = 1.65, y=value, label=paste(pcent, "%",sep="")),
                      position = position_stack(vjust=0.5), box.padding = 0.1) +
      guides(fill = guide_legend(title = "Source of Infection"))
  })
  
  output$YorkInfection <- renderPlot({ggplot(yor_inf_df, 
                                             aes(x="", y=value, fill=paste(group,pcent,"%") )) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) + theme_void() +
      scale_fill_brewer(palette="Paired") +
      geom_text_repel(aes(x = 1.65, y=value, label=paste(pcent, "%",sep="")),
                      position = position_stack(vjust=0.5), box.padding = 0) +
      guides(fill = guide_legend(title = "Source of Infection"))
    
  })
  
  output$YorkOutcome <- renderPlot({ggplot(yor_out_df, 
                                           aes(x="", y=value, fill=paste(group, pcent,"%") )) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) + theme_void() +
      scale_fill_brewer(palette="Pastel2") +
      geom_text_repel(aes(x = 1.65, y=value, label=paste(pcent, "%",sep="")),
                      position = position_stack(vjust=0.5), box.padding = 0.1) +
      guides(fill = guide_legend(title = "Status"))
  })
  
  output$TorontoOutcome <- renderPlot({ggplot(tor_out_df, 
                                              aes(x="", y=value, fill=paste(group, pcent,"%") )) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) + theme_void() +
      scale_fill_brewer(palette="Pastel1") +
      geom_text_repel(aes(x = 1.65, y=value, label=paste(pcent, "%",sep="")),
                      position = position_stack(vjust=0.5), box.padding = 0.1) +
      guides(fill = guide_legend(title = "Status"))
    
    
    
    
  })
  
  
}