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
  title = "UrbanScope",
  fluid = TRUE,
  theme = shinytheme("flatly"),
  id = "navbarID",
  navbarMenu("EDA", 
             #==========================================================
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
                     choices = c("Median Rent" = "median_rent", "Count of Rented Flats" = "rented_count"),
                     selected = "median_rent"
                   ),
                   selectInput(
                     inputId = "flat_type",
                     label = "Flat Type",
                     choices = sort(unique(rental_sf$flat_type)),
                     selected = sort(unique(rental_sf$flat_type))[1]
                   ),
                   selectInput(
                     inputId = "month",
                     label = "Month",
                     choices = c('2024 Jan to Sep' = '2024 Jan to Sep', unique(
                       format(rental_sf$rent_approval_date, "%Y %b")
                     )),
                     selected = '2024 Jan to Sep'
                   )  
                 ),
                 mainPanel(tmapOutput(
                   "chloropleth", width = "100%", height = 580
                 ),
                 tags$div(style = "margin-top: 20px;"),  # Add a div with margin
                 wellPanel(
                   tableOutput("statistics")
                 )
                 )
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
    
    
    data_summary <- data %>%
      group_by(town) %>%
      summarize(
        rented_count = n(),
        median_rent = median(monthly_rent, na.rm = TRUE),
        # Use median instead of mean for median_rent
        .groups = 'drop'
      ) %>%
      st_drop_geometry() %>%
      right_join(mpsz_sf, by = c("town" = "PLN_AREA_N")) %>%
      st_as_sf()
    
    return(data_summary)  # Return the data_summary
  })
  
  output$chloropleth <- renderTmap({
    # Get chloropleth_data
    chloropleth_data <- calculate_chloropleth_data()
    
    if (nrow(chloropleth_data) == 0) {
      return(NULL)  # Return NULL if there is no data to plot
    }
    
    if (input$plot_variable == "rented_count") {
      tmap_mode("view")
      qtm(chloropleth_data, fill = "rented_count")
    } else if (input$plot_variable == "median_rent") {
      tmap_mode("view")
      qtm(chloropleth_data, fill = "median_rent")
    }
  })
  
  summary_statistics <- reactive({
    data <- filtered_data()
    
    total_rentals <- nrow(data)
    avg_rent <- mean(data$monthly_rent, na.rm = TRUE)
    median_rent <- median(data$monthly_rent, na.rm = TRUE)
    max_rent <- max(data$monthly_rent, na.rm = TRUE)
    min_rent <- min(data$monthly_rent, na.rm = TRUE)
    
    stats <- data.frame(
      Total_Rentals = total_rentals,
      Average_Rent = round(avg_rent, 2),
      Median_Rent = round(median_rent, 2),
      Max_Rent = round(max_rent, 2),
      Min_Rent = round(min_rent, 2)
    )
    
    return(stats)
  })
  
  output$statistics <- renderTable({
    summary_statistics()
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
}

shinyApp (ui = ui, server = server)