pacman::p_load(shiny,
               sf,
               tmap,
               bslib,
               tidyverse,
               sfdep,
               shinydashboard,
               shinythemes)

rental_sf <- read_rds('data/rds/rental_sf.rds')
mpsz_sf <- read_rds('data/rds/mpsz_grped_by_town.rds')
#========================#
###### Shiny UI ######
#========================#

ui <- navbarPage(
  title = "Urbanscope",
  fluid = TRUE,
  theme = shinytheme("flatly"),
  id = "navbarID",
  navbarMenu("EDA", #==========================================================
             # Histogram
             #==========================================================
             # tabPanel("Histogram"),
             #==========================================================
             # Chloropleth Map
             #==========================================================
             tabPanel(
               "Chloropleth Map", sidebarLayout(
                 sidebarPanel(
                   selectInput(
                     inputId = "plot_variable",
                     label = "Select Plot Variable",
                     choices = c(
                       "Median Rent" = "median_rent",
                       "Count of Rental Flats" = "rented_count"
                     ),
                     selected = "avg_price"
                   ),
                   selectInput(
                     inputId = "flat_type",
                     label = "Flat Type",
                     choices = sort(unique(rental_sf$flat_type)),
                   ),
                   selectInput(
                     inputId = "month",
                     label = "Month",
                     choices = c('2024 Jan to Sep' = '2024 Jan to Sep', unique(
                       format(rental_sf$rent_approval_date, "%Y %b")
                     ))
                   ),
                   tmapOutput("chloropleth", width = "100%", height = 580)
                 ),
                 mainPanel(tmapOutput(
                   "chloropleth", width = "100%", height = 580
                 ))
               )
             )),
  tabPanel(
    "Correlation Matrix",
    sidebarLayout(
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
        tmapOutput("mapPlot", width = "100%", height = 580)
      ),
      mainPanel(tmapOutput(
        "mapPlot", width = "100%", height = 580
      ))
    )
  ),
  tabPanel("GWR", sidebarLayout(
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
      tmapOutput("mapPlot", width = "100%", height = 580)
    ),
    mainPanel(tmapOutput(
      "mapPlot", width = "100%", height = 580
    ))
  )),
  tabPanel("Predictive Model", sidebarLayout(
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
      tmapOutput("mapPlot", width = "100%", height = 580)
    ),
    mainPanel(tmapOutput(
      "mapPlot", width = "100%", height = 580
    ))
  )),
  tabPanel("Data Table", sidebarLayout(
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
      tmapOutput("mapPlot", width = "100%", height = 580)
    ),
    mainPanel(tmapOutput(
      "mapPlot", width = "100%", height = 580
    ))
  ))
  
)

#========================#
###### Shiny Server ######
#========================#

server <- function(input, output) {
  #==========================================================
  # Chloropleth Map
  #==========================================================
  filtered_data <- reactive({
    req(input$flat_type, input$month)  # Ensure inputs are available
    rental_sf %>%
      mutate(
        town = if_else(town == 'KALLANG/WHAMPOA', 'KALLANG', town),
        year_month = format(rent_approval_date, "%Y %b")
      ) %>%
      filter(town != "CENTRAL") %>%
      filter (if (input$month == "2024 Jan to Sep") {
        TRUE  # No additional filtering by month
      } else {
        format(rent_approval_date, "%Y %b") == input$month  # Filter by selected year-month
      }) %>%
      filter(flat_type == input$flat_type)
  })
  
  calculate_chloropleth_data <- reactive({
    data <- filtered_data()
    
    if (input$plot_variable == "rented_count") {
      data_summary <- data %>%
        group_by(town) %>%
        summarize(rented_count = n(), .groups = 'drop') %>%
        st_drop_geometry()
    } else if (input$plot_variable == "median_rent") {
      data_summary <- data %>%
        group_by(town) %>%
        summarize(median_rent = mean(monthly_rent, na.rm = TRUE),
                  .groups = 'drop') %>%
        st_drop_geometry()
    }
    
    data_summary %>%
      right_join(mpsz_sf, by = c("town" = "PLN_AREA_N")) %>%
      st_as_sf()
  })
  
  
  output$chloropleth <- renderTmap({
    # Get chloropleth_data
    chloropleth_data <- calculate_chloropleth_data()
    
    if (input$plot_variable == "rented_count") {
      tmap_mode("view")
      qtm(chloropleth_data, fill = "rented_count")
    } else if (input$plot_variable == "median_rent") {
      tmap_mode("view")
      qtm(chloropleth_data, fill = "median_rent")
    }
  })
  
}

shinyApp (ui = ui, server = server)