pacman::p_load(shiny,
               sf,
               tmap,
               bslib,
               tidyverse,
               sfdep,
               shinydashboard,
               shinythemes)

rental_sf <- read_rds('data/rds/rental_sf.rds')
mpsz_sf <-
  #========================#
  ###### Shiny UI ######
#========================#

ui <- navbarPage(
  title = "Urbanscope",
  fluid = TRUE,
  theme = shinytheme("flatly"),
  id = "navbarID",
  
  tabPanel("EDA", sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "map_type",
        label = "Select Map Type",
        choices = c(
          "Average Rental Price" = "avg_price",
          "Count of Rental Flats" = "count"
        ),
        selected = "avg_price"
      ),
      selectInput(
        inputId = "flat_type",
        label = "Flat Type",
        choices = unique(rental_sf$flat_type),
        # Replace with your actual column name
        selected = '3-ROOM'
      ),
      selectInput(
        inputId = "month",
        label = "Month",
        choices = unique(month(rental_sf$rent_approval_date)),
        # You can customize this
        selected = month.abb[1]
      ),
      tmapOutput("mapPlot", width = "100%", height = 580)
    ),
    mainPanel(tmapOutput(
      "mapPlot", width = "100%", height = 580
    ))
  )),
  tabPanel(
    "Correlation Matrix", sidebarLayout(sidebarPanel(
      selectInput(
        inputId = "map_type",
        label = "Select Map Type",
        choices = c(
          "Average Rental Price" = "avg_price",
          "Count of Rental Flats" = "count"
        ),
        selected = "avg_price"),
      tmapOutput("mapPlot", width = "100%", height = 580)
    ),
    mainPanel(tmapOutput(
      "mapPlot", width = "100%", height = 580
    )))
  ),
  tabPanel(
    "GWR", sidebarLayout(sidebarPanel(
      selectInput(
        inputId = "map_type",
        label = "Select Map Type",
        choices = c(
          "Average Rental Price" = "avg_price",
          "Count of Rental Flats" = "count"
        ),
        selected = "avg_price"),
      tmapOutput("mapPlot", width = "100%", height = 580)
    ),
    mainPanel(tmapOutput(
      "mapPlot", width = "100%", height = 580
    )))
  ),
  tabPanel(
    "Predictive Model", sidebarLayout(sidebarPanel(
      selectInput(
        inputId = "map_type",
        label = "Select Map Type",
        choices = c(
          "Average Rental Price" = "avg_price",
          "Count of Rental Flats" = "count"
        ),
        selected = "avg_price"),
      tmapOutput("mapPlot", width = "100%", height = 580)
    ),
    mainPanel(tmapOutput(
      "mapPlot", width = "100%", height = 580
    )))
  ),
  tabPanel(
    "Data Table", sidebarLayout(sidebarPanel(
      selectInput(
        inputId = "map_type",
        label = "Select Map Type",
        choices = c(
          "Average Rental Price" = "avg_price",
          "Count of Rental Flats" = "count"
        ),
        selected = "avg_price"),
      tmapOutput("mapPlot", width = "100%", height = 580)
    ),
    mainPanel(tmapOutput(
      "mapPlot", width = "100%", height = 580
    )))
  )
  
)

#========================#
###### Shiny Server ######
#========================#

server <- function(input, output) {
  
}

shinyApp (ui = ui, server = server)