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
source("leafletScriptfinal.R")

dashboardPage(
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