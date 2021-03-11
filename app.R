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

ui <- dashboardPage(
  dashboardHeader(title = "GTA Covid Cases"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Interactive Map", tabName = "map", icon = icon("map")),
      menuItem("Charts", tabName = "charts", icon = icon("chart-bar")),
      menuItem("About", tabName = "sources", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
    tabItem(tabName = "map",
    # Boxes need to be put in a row (or column)
    fluidRow(
      
      box(
        title = "Select a Region",
        checkboxGroupInput("region", "Region", choices=c("Hamilton", "Toronto",
                                                         "Peel", "York"), 
                           selected = c("Hamilton", "Toronto", "Peel", "York"),
        ), width=2
      ),
      
      box(leafletOutput("covidMap"), width = 10, title="Total Covid Cases since March 2020")
      
 
    )
    ),#end tab item1
    
    tabItem(tabName = "charts",
           h2("Source of Infections"),
           fluidRow (
             box(title = "Toronto",
                 plotOutput("TorontoInfection")),
             
             box(title = "York Region",
                 plotOutput("YorkInfection"))
           ),
           h2("Distribution of Cases"),
           fluidRow(
             box(title = "Toronto",
                 plotOutput("TorontoOutcome")),
             
             box(title = "York Region",
                 plotOutput("YorkOutcome"))             
           ),
           p("Note: Outcome and source of infection data were not present in the Hamilton & Peel datasets I used, though they may be available elsewhere. 
             Please see About for links to the data that were used.")
            ),#end of Charts
    tabItem(tabName = "sources", h2("Data Sources"),
            p("Open datasets were obtained from:"),
            tags$ul(
            tags$li(tags$a(href="https://open.hamilton.ca/datasets/2177d6c6562247319edede041fd439af_10?geometry=-81.706%2C42.892%2C-78.223%2C43.592", 
                      "Open Data Hamilton", target="_blank")),
            tags$li(tags$a(href = "https://data.peelregion.ca/datasets/covid-case-data-by-census-tract-region-of-peel",
                           "Region of Peel Data Portal", target="_blank")),
            tags$li(tags$a(href = "https://open.toronto.ca/dataset/covid-19-cases-in-toronto/",
                           "Open Data Toronto", target="_blank")),
            tags$li(tags$a(href = "https://opendata.arcgis.com/datasets/5fa0e4d94e3147b1b957b0d84641c7a1_0.geojson",
                           "Open Data: Toronto Census Tracts", target="_blank")),
            tags$li(tags$a(href = "https://www.york.ca/wps/portal/yorkhome/health/yr/covid-19/covid19inyorkregion/01covid19inyorkregion/!ut/p/z1/tZJLT-MwFIV_C4suI187SW0vTeg0CTQtjz7iTZVJ09RMk5SMKTC_fhxUJISgMGLshV-6Plfn80ESLZCss70qM62aOtuacyr7y0gMozA8h3jssQAEjEVMKIMBx2j-XAAfDAFIfuX9kQJ5XH6GJJK7XK1QStyCspwzh4KfO16W9x2Of67MxNc-5-uMd3JI5rXe6Q1Kn9pl3tS6qHUPnpr2lzn81krfP19smqowc5Ft9aYHebNXKwfzww5zVXcv2qI0mHoA-J1rFH_m3cAl7SgYlcZBpjeOqtcNWrz0OuzeiC7e72Wk1O3dnRTGXufpUaOFbX_zDuZrh8Nr5kE0i6mY4TF4kXsoIMTrhziAGMIxg-gHnfhnLMRwTg4FR_43NfmgH0K8Imi-V8UDmtZNW5m8Xv9jHMKXDpQFIhRDmMDNlMLlgHqsfzGaXFzhb3b4xIBledeqPAW78sSu_P-BE0cQYNHF3x24IEgUsFM3Zklil31il31il31iN_ez78LZVdNpxVx_WzLNo1u_rJZnp4mTxvs_R5eRODn5Cx0G6fA!/dz/d5/L2dBISEvZ0FBIS9nQSEh/?fbclid=IwAR1s7I6tHd_9J4PNYM8Rcn8GFThstopDEMRBiTNrw0hs0KJOLO4JWD-9LJA#.YEl-D_5yZH5",
                           "York Region Portal", target="_blank")),
            tags$li(tags$a(href = "https://opendata.arcgis.com/datasets/834537fee6d541a5a7e9a404d9da648a_1.geojson",
                           "York Region Municipalities", target="_blank"))
            
            
            
            
            )#end ul
            
            )#end About
    )#end tab items
  )#end dashboard body
)

server <- function(input, output) {
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

shinyApp(ui, server)