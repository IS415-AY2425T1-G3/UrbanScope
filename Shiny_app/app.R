pacman::p_load(
  shiny,
  sf,
  tmap,
  bslib,
  tidyverse,
  sfdep,
  GWmodel,
  olsrr,
  performance,
  corrplot,
  ggstatsplot,
  shinydashboard,
  shinythemes,
  plotly,
  DT,
  shinycssloaders,
  shinyjs,
  shinyalert
)

rental_sf <- read_rds('data/rds/rental_sf.rds')
mpsz_sf <- read_rds('data/rds/mpsz_grped_by_town.rds')
gwr_model <- read_rds('data/rds/gwr_adaptive.rds')

categorical_cols <- c("town", "rent_approval_date", "flat_type", "region")

# Geospatial Data Import
kindergarten_sf <- read_rds('data/rds/geospatial/kindergarten_sf.rds')
childcare_sf <- read_rds('data/rds/geospatial/childcare_sf.rds')
hawker_sf <- read_rds('data/rds/geospatial/hawker_sf.rds')
busstop_sf <- read_rds('data/rds/geospatial/busstop_sf.rds')
shoppingmall_sf <- read_rds('data/rds/geospatial/shoppingmall_sf.rds')
mrt_sf <- read_rds('data/rds/geospatial/mrt_sf.rds')
primarysch_sf <- read_rds('data/rds/geospatial/primarysch_sf.rds')
cbd_sf <- read_rds('data/rds/geospatial/cbd_sf.rds')

# Define a global variable to store MLR results
mlr_model_result <- NULL
rental_fw_mlr <- NULL
rental_bw_mlr <- NULL
rental_bi_mlr <- NULL
#========================#
###### Shiny UI ######
#========================#

ui <- navbarPage(
  useShinyjs(), # Use shinyjs packages
  title = "UrbanScope",
  fluid = TRUE,
  theme = shinytheme("united"),
  id = "navbarID",
  navbarMenu(
    "EDA",
    #==========================================================
    # Histogram
    #==========================================================
    tabPanel(
      "Histogram/Barplot",
      sidebarLayout(
        sidebarPanel(
          tabsetPanel(
            id = "histogram_mode_selector",
            tabPanel(
              "Continuous",
              selectInput(
                inputId = "histo_plot_variable",
                label = "Select Plot Variable",
                choices = setdiff(colnames(rental_sf), c("geometry", categorical_cols)),
                selected = setdiff(colnames(rental_sf), c("geometry", categorical_cols))[1]
              ),
              selectInput(
                inputId = "histo_month",
                label = "Month",
                choices = c('2024 Jan to Sep' = '2024 Jan to Sep', unique(
                  format(rental_sf$rent_approval_date, "%Y %b")
                )),
                selected = '2024 Jan to Sep'
              ),
              radioButtons(
                inputId = "histo_flat_type",
                label = "Select Flat Type",
                choices = sort(unique(rental_sf$flat_type)),
                selected = sort(unique(rental_sf$flat_type))[1],
                inline = TRUE
              ),
              selectInput(
                inputId = "histo_town",
                label = "Town",
                choices = c("All of Singapore"='all',sort(unique(rental_sf$town))),
                selected = 'all'
              ),
              sliderInput(
                inputId = "histo_bin_number",
                label = "Number of Bins",
                min = 5,
                max = 20,
                value = c(12)
              ),
            ),
            tabPanel(
              "Categorical",
              selectInput(
                inputId = "histo_cat_plot_variable_x",
                label = "Select X Variable",
                choices = setdiff(categorical_cols, c('town'))
              ),
              selectInput(
                inputId = "histo_cat_plot_variable_y",
                label = "Select Y Variable",
                choices = c("Frequency" = "frequency", "Median Rent" = "median_rent") # You can add other variables as needed
              )
            )
          )
        ),
        mainPanel(
          plotlyOutput("histo_bar_plot"),
          tags$div(style = "margin-top: 20px;"),
          conditionalPanel(condition = "input.histogram_mode_selector == 'Continuous'", wellPanel(tableOutput("histo_statistics")))
        )
      )
    ),
    #==========================================================
    # Choropleth Map
    #==========================================================
    tabPanel("Choropleth Map", sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "choro_plot_variable",
          label = "Select Plot Variable",
          choices = c("Median Rent" = "median_rent", "Count of Rented Flats" = "rented_count"),
          selected = "median_rent"
        ),
        selectInput(
          inputId = "choro_month",
          label = "Month",
          choices = c('2024 Jan to Sep' = '2024 Jan to Sep', unique(
            format(rental_sf$rent_approval_date, "%Y %b")
          )),
          selected = '2024 Jan to Sep'
        ),
        radioButtons(
          inputId = "choro_flat_type",
          label = "Select Flat Type",
          choices = sort(unique(rental_sf$flat_type)),
          selected = sort(unique(rental_sf$flat_type))[1],
          inline = TRUE
        )
      ),
      mainPanel(
        tmapOutput("choropleth", width = "100%", height = "600px"),
        tags$div(style = "margin-top: 20px;"),
        wellPanel(
          uiOutput("choro_stats_title"),
          # Output for dynamic title
          tableOutput("choro_statistics")
        )
      )
    )),
    #==========================================================
    # Scatterplot
    #==========================================================
    tabPanel("Scatterplot", sidebarLayout(
      sidebarPanel(
        selectInput(
          "scatter_x",
          "Select X Variable",
          choices = setdiff(colnames(rental_sf), c("geometry", categorical_cols)),
          selected = setdiff(colnames(rental_sf), c("geometry", categorical_cols))[1]
        ),
        selectInput(
          "scatter_y",
          "Select Y Variable",
          choices = setdiff(colnames(rental_sf), c("geometry", categorical_cols)),
          selected = setdiff(colnames(rental_sf), c("geometry", categorical_cols))[2]  # Select a different column for Y by default
        ),
        selectInput(
          inputId = "scatter_month",
          label = "Month",
          choices = c('2024 Jan to Sep' = '2024 Jan to Sep', unique(
            format(rental_sf$rent_approval_date, "%Y %b")
          )),
          selected = '2024 Jan to Sep'
        ),
        selectInput(
          inputId = "scatter_town",
          label = "Town",
          choices = c("All of Singapore"='all',sort(unique(rental_sf$town))),
          selected = 'all'
        ),
        radioButtons(
          inputId = "scatter_flat_type",
          label = "Select Flat Type",
          choices = sort(unique(rental_sf$flat_type)),
          selected = sort(unique(rental_sf$flat_type))[1],
          inline = TRUE
        ),
        # UI
        selectInput("scatter_point_color", "Choose Point Color:", choices = colors(), selected = "blue"),
        sliderInput("scatter_point_size", "Point Size:", min = 1, max = 5, value = 2),
        checkboxInput("scatter_add_smooth", "Add Smoothing Line", value = FALSE)
      ),
      mainPanel(plotlyOutput("scatter_plot"))
    )),
    #==========================================================
    # Locations of Interest
    #==========================================================
    tabPanel("Locations of Interest", sidebarLayout(
      sidebarPanel(
        selectInput("dataset", "Select Dataset:",
                    choices = c("Kindergarten" = "kindergarten_sf",
                                "Childcare" = "childcare_sf",
                                "Hawker" = "hawker_sf",
                                "Bus Stop" = "busstop_sf",
                                "Shopping Mall" = "shoppingmall_sf",
                                "MRT" = "mrt_sf",
                                "Primary School" = "primarysch_sf",
                                "CBD" = "cbd_sf")),
        conditionalPanel(
          condition = "input.dataset != 'cbd_sf'",
          selectInput("locationTown", "Select Town:",
                      choices = c("All of Singapore"='all', sort(unique(mpsz_sf$PLN_AREA_N)))))
      ),
      mainPanel(tmapOutput('locationMap', width = "100%", height = "600px"))
    ))
  ),
  #==========================================================
  # Explanatory Model
  #==========================================================
  navbarMenu(
    "Explanatory Model",
    #==========================================================
    # Correlation Matrix
    #==========================================================
    tabPanel(
      "Correlation Matrix",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "cm_method",
            label = "Method",
            choices = c(
              "Circle" = "circle",
              "Square" = "square",
              "Ellipse" = "ellipse",
              "Number" = "number",
              "Pie" = "pie",
              "Shade" = "shade",
              "Color" = "color"
            ),
            selected = "circle"
          ),
          selectInput(
            inputId = "cm_type",
            label = "Type",
            choices = c(
              "Full" = "full",
              "Upper" = "upper",
              "Lower" = "lower"
            ),
            selected = "full"
          ),
          selectInput(
            inputId = "cm_order",
            label = "Order",
            choices = c(
              "Original" = "original",
              "Angular Order of the Eigenvectors Order (AOE)" = "AOE",
              "First Principal Component Order (FPC)" = "FPC",
              "Hierarchical Clustering Order" = "hclust",
              "Alphabetical Order" = "alphabet"
            ),
            selected = "original"
          ),
          conditionalPanel(
            condition = "input.cm_order == 'hclust'",
            selectInput(
              inputId = "cm_hclust_method",
              label = "Hierarchical Clustering Method",
              choices = c(
                "Ward's Method (D)" = "ward.D",
                "Ward's Method (D2)" = "ward.D2",
                "Single Linkage" = "single",
                "Complete Linkage" = "complete",
                "Average Linkage" = "average",
                "McQuitty Method" = "mcquitty",
                "Median Method" = "median",
                "Centroid Method" = "centroid"
              ),
              selected = "ward"
            )
          ),
          checkboxGroupInput(
            inputId = "selected_columns",
            label = "Select Independent Columns",
            choices = colnames(rental_sf)[7:19],
            selected = colnames(rental_sf)[7:19]
          ),
        ),
        
        mainPanel(
          plotOutput("correlationMatrixPlot", width = "100%", height = 580)
        )
      )
    ),
    #==========================================================
    # Multiple Linear Regression
    #==========================================================
    tabPanel("Multiple Linear Regression",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "mlr_dependent_variable",
                   label = "Dependent Variable",
                   choices = c("Monthly Rent" = "monthly_rent"),
                   selected = "CV"
                 ),
                 checkboxGroupInput(
                   inputId = "mlr_independent_variables",
                   label = "Select Independent Variables",
                   choices = colnames(rental_sf)[7:19],
                   selected = colnames(rental_sf)[7:19],
                   inline = FALSE
                 ),
                 conditionalPanel(
                   condition = "input.mlr_tabs == 'stepwise_tab'",
                   sliderInput(
                     inputId = "stepwise_p_value_threshold",
                     label = "Select p-value threshold for Stepwise Selection:",
                     min = 0.01,
                     max = 1,
                     value = 0.05,
                     step = 0.01
                   ),
                   selectInput(
                     inputId = "stepwise_method",
                     label = "Choose Stepwise Method:",
                     choices = c("Forward", "Backward", "Both"),
                     selected = "Forward",
                     multiple = TRUE
                   )
                 ),
                 actionButton(
                   inputId = "MLRSubmit",
                   label = "Submit",
                   style = "color: white; background-color: #007BFF; border-color: #007BFF"
                 )
               ),
               mainPanel(tabsetPanel(
                 id = "mlr_tabs",
                 tabPanel("Model Info", value = "mlr_model_info_tab", verbatimTextOutput("mlr_model_info")),
                 tabPanel(
                   "Publication Quality Table ",
                   value = "mlr_pqt_tab",
                   verbatimTextOutput("mlr_pqt_model_info")
                 ),
                 tabPanel(
                   "Stepwise Method",
                   value = "stepwise_tab",
                   conditionalPanel(
                     condition = "input.stepwise_method.includes('Forward')",
                     fluidRow(column(
                       width = 12,
                       h3("Model Information - Forward"),
                       verbatimTextOutput("stepwise_forward_info")
                     )),
                     fluidRow(column(
                       width = 12,
                       h3("Model Plot - Forward"),
                       plotOutput("stepwise_forward_plot", width = "100%", height = "600px")
                     )),
                     fluidRow(column(
                       width = 12,
                       h3("Model Parameters - Forward"),
                       plotOutput(
                         "stepwise_forward_parameters_plot",
                         width = "100%",
                         height = "600px"
                       )
                     )),
                   ),
                   
                   conditionalPanel(
                     condition = "input.stepwise_method.includes('Backward')",
                     fluidRow(column(
                       width = 12,
                       h3("Model Information - Backward"),
                       verbatimTextOutput("stepwise_backward_info")
                     )),
                     fluidRow(column(
                       width = 12,
                       h3("Model Plot - Backward"),
                       plotOutput(
                         "stepwise_backward_plot",
                         width = "100%",
                         height = "600px"
                       )
                     )),
                     fluidRow(column(
                       width = 12,
                       h3("Model Parameters - Backward"),
                       plotOutput(
                         "stepwise_backward_parameters_plot",
                         width = "100%",
                         height = "600px"
                       )
                     )),
                   ),
                   
                   conditionalPanel(
                     condition = "input.stepwise_method.includes('Both')",
                     fluidRow(column(
                       width = 12,
                       h3("Model Information - Both"),
                       verbatimTextOutput("stepwise_both_info")
                     )),
                     fluidRow(column(
                       width = 12,
                       h3("Model Plot - Both"),
                       plotOutput("stepwise_both_plot", width = "100%", height = "600px")
                     )),
                     fluidRow(column(
                       width = 12,
                       h3("Model Parameters - Both"),
                       plotOutput(
                         "stepwise_both_parameters_plot",
                         width = "100%",
                         height = "600px"
                       )
                     ))
                   )
                 ), 
                 tabPanel(
                   "Stepwise Comparison",
                   value = "stepwise_comparison_tab",
                   plotOutput("stepwise_comparison_plot", width = "100%", height = "600px")
                 ),
                 tabPanel(
                   "Regression Diagnostics (olsrr Package)",
                   value = "mlr_rd_olsrr_tab",
                   h4("Please wait while the plots are loading. Do not click on other tabs."),
                   verbatimTextOutput("olsrr_multicollinearity_text"),
                   withSpinner(plotOutput("olsrr_non_linearity_plot", width = "100%", height = "600px")),
                   withSpinner(plotOutput("olsrr_normality_plot", width = "100%", height = "600px")),
                   withSpinner(tmapOutput("olsrr_MLR_RES_plot", width = "100%", height = 580))
                 ),
                 tabPanel(
                   "Regression Diagnostics (Performance Package) - Forward",
                   value = "mlr_rd_performance_forward_tab",
                   h4("Please wait while the plots are loading. Do not click on other tabs."),
                   verbatimTextOutput("performance_forward_multicollinearity_text"),
                   withSpinner(plotOutput("performance_forward_multicollinearity_plot", width = "100%", height = "600px")),
                   withSpinner(plotOutput("performance_forward_non_linearity_plot", width = "100%", height = "600px")),
                   withSpinner(plotOutput("performance_forward_normality_plot", width = "100%", height = "600px")),
                   withSpinner(plotOutput("performance_forward_outlier_plot", width = "100%", height = "600px")),
                 ),
                 tabPanel(
                   "Regression Diagnostics (Performance Package) - Backward",
                   value = "mlr_rd_performance_backward_tab",
                   h4("Please wait while the plots are loading. Do not click on other tabs."),
                   verbatimTextOutput("performance_backward_multicollinearity_text"),
                   withSpinner(plotOutput("performance_backward_multicollinearity_plot", width = "100%", height = "600px")),
                   withSpinner(plotOutput("performance_backward_non_linearity_plot", width = "100%", height = "600px")),
                   withSpinner(plotOutput("performance_backward_normality_plot", width = "100%", height = "600px")),
                   withSpinner(plotOutput("performance_backward_outlier_plot", width = "100%", height = "600px")),
                 ),
                 tabPanel(
                   "Regression Diagnostics (Performance Package) - Both",
                   value = "mlr_rd_performance_both_tab",
                   h4("Please wait while the plots are loading. Do not click on other tabs."),
                   verbatimTextOutput("performance_both_multicollinearity_text"),
                   withSpinner(plotOutput("performance_both_multicollinearity_plot", width = "100%", height = "600px")),
                   withSpinner(plotOutput("performance_both_non_linearity_plot", width = "100%", height = "600px")),
                   withSpinner(plotOutput("performance_both_normality_plot", width = "100%", height = "600px")),
                   withSpinner(plotOutput("performance_both_outlier_plot", width = "100%", height = "600px")),
                 ),
               ))
             )), 
    #==========================================================
    # GWR
    #==========================================================
    tabPanel("GWR", sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "gwr_dependent_variable",
          label = "Dependent Variable",
          choices = c("Monthly Rent" = "monthly_rent"),
          selected = "CV"
        ),
        checkboxGroupInput(
          inputId = "gwr_independent_variables",
          label = "Select Independent Variables",
          choices = colnames(rental_sf)[7:19],
          selected = colnames(rental_sf)[7:19],
          inline = FALSE
        ),
        selectInput(
          inputId = "gwr_approach",
          label = "Bandwidth Approach",
          choices = c(
            "Cross-validation (CV)" = "CV",
            "Akaike Information Criterion corrected (AICc)" = "AIC"
          ),
          selected = "CV"
        ),
        selectInput(
          inputId = "gwr_adaptive",
          label = "Type of Bandwidth",
          choices = c("Adaptive" = "TRUE", "Fixed" = "FALSE"),
          selected = "TRUE"
        ),
        conditionalPanel(
          condition = "input.gwr_adaptive == 'TRUE'",
          sliderInput(
            inputId = "gwr_adaptive_bw",
            label = "Select Bandwidth:",
            min = 30,
            max = 200,
            value = 54,
            step = 1
          ),
          p("Recommended bandwidth: 54", style = "color: gray; font-size: 12px;")
        ), 
        conditionalPanel(
          condition = "input.gwr_adaptive == 'FALSE'",
          sliderInput(
            inputId = "gwr_fixed_bw",
            label = "Select Bandwidth:",
            min = 300,
            max = 1000,
            value = 450,
            step = 1
          ),
          p("Recommended bandwidth: 450, too low will casue error!", style = "color: gray; font-size: 12px;")
        ), 
        selectInput(
          inputId = "gwr_kernel",
          label = "Kernel",
          choices = c(
            "Gaussian" = "gaussian",
            "Exponential" = "exponential",
            "Bisquare" = "bisquare",
            "Tricube" = "tricube",
            "Boxcar" = "boxcar"
          ),
          selected = "gaussian"
        ),
        selectInput(
          inputId = "gwr_longlat",
          label = "Distance Measure",
          choices = c(
            "Great Circle Distance" = "TRUE",
            "Euclidean Distance" = "FALSE"
          ),
          selected = "FALSE"
        ),
        actionButton(
          inputId = "GWRSubmit",
          label = "Submit",
          style = "color: white; background-color: #007BFF; border-color: #007BFF"
        )
      ),
      mainPanel(tabsetPanel(
        id = "gwr_tabs",
        tabPanel("Model Info", value = "gwr_model_info_tab", withSpinner(verbatimTextOutput("gwr_model_info"))),
        tabPanel("Local R2 Map", value = "gwr_local_r2_tab", withSpinner(
          tmapOutput("gwr_local_r2_plot", width = "100%", height = 580)
        ))
      ))
    )),
  ), 
  #==========================================================
  # Predictive Model
  #==========================================================
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
    mainPanel(plotOutput(
      "mapPlot", width = "100%", height = 580
    ))
  )),
  #==========================================================
  # Predictive Model
  #==========================================================
  tabPanel("Data Table",
           dataTableOutput("data_table",height = "100%")
  )
  
)

#========================#
###### Shiny Server ######
#========================#

server <- function(input, output) {
  #==========================================================
  # Summary Statistics
  #==========================================================
  # Define a function that takes data and a column name as arguments
  calculate_summary_statistics <- function(data, column, original_data = NULL) {
    # Check if the column exists in the data
    if (!(column %in% colnames(data))) {
      stop("The specified column does not exist in the data.")
    }
    
    # Filter out non-numeric columns
    if (!is.numeric(data[[column]])) {
      stop("The specified column is not numeric.")
    }
    
    # Use the row count of original_data if provided, otherwise use the current data
    total_rows <- if (!is.null(original_data))
      nrow(original_data)
    else
      nrow(data)
    
    # Calculate summary statistics
    avg <- mean(data[[column]], na.rm = TRUE)
    median_val <- median(data[[column]], na.rm = TRUE)
    min_val <- min(data[[column]], na.rm = TRUE)
    max_val <- max(data[[column]], na.rm = TRUE)
    std_dev <- sd(data[[column]], na.rm = TRUE)
    iqr_val <- IQR(data[[column]], na.rm = TRUE)
    
    # Calculate Q1 and Q3
    q1_val <- quantile(data[[column]], 0.25, na.rm = TRUE)
    q3_val <- quantile(data[[column]], 0.75, na.rm = TRUE)
    
    # Create a summary statistics data frame
    stats <- data.frame(
      Total_Rows = total_rows,
      Average = round(avg, 2),
      Median = round(median_val, 2),
      Min = round(min_val, 2),
      Max = round(max_val, 2),
      Std_Dev = round(std_dev, 2),
      Q1 = round(q1_val, 2),
      Q3 = round(q3_val, 2),
      IQR = round(iqr_val, 2)
      
    )
    
    return(stats)
  }
  
  
  # FOR CHLORO SUMMARY STAT
  choro_summary_statistics <- reactive({
    data <- calculate_choropleth_data()  # Get grouped data
    filtered_data <- choro_filtered_data()  # Get filtered data
    calculate_summary_statistics(data, input$choro_plot_variable, filtered_data)  # Call the function with the filtered data
  })
  
  output$choro_statistics <- renderTable({
    choro_summary_statistics()  # This remains unchanged
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$choro_stats_title <- renderUI({
    req(input$choro_plot_variable, input$choro_flat_type, input$choro_month)  # Ensure all inputs are available
    
    # Construct the dynamic title with smaller font for Flat Type and Month
    title_text <- paste(
      "Summary Statistics for:",
      input$choro_plot_variable,
      "<br><span style='font-size: 0.85em; color: #555;'><strong>Flat Type:</strong> ",
      input$choro_flat_type,
      "| <strong>Month:</strong> ",
      input$choro_month,
      "</span>"
    )
    
    tags$h4(HTML(title_text))  # Render the title with HTML
  })
  
  # FOR HISTO SUMMARY STAT
  histo_summary_statistics <- reactive({
    if (input$histogram_mode_selector == "Continuous") {
      data <- histo_filtered_data()  # Get filtered data
      calculate_summary_statistics(data, input$histo_plot_variable)
    } else {
      return(NULL)  # Return NULL or an empty data frame if mode is not "Continuous"
    }
  })
  
  output$histo_statistics <- renderTable({
    histo_summary_statistics()  # This remains unchanged
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  
  #==========================================================
  # Histogram / Barplot
  #==========================================================
  # Only for continuous mode
  histo_filtered_data <- reactive({
    req(input$histo_flat_type, input$histo_month, input$histo_town)  # Ensure inputs are available
    rental_sf %>%
      mutate(year_month = format(rent_approval_date, "%Y %b")) %>%
      filter (if (input$histo_month == "2024 Jan to Sep") {
        TRUE  # No additional filtering by month
      } else {
        format(rent_approval_date, "%Y %b") == input$histo_month  # Filter by selected year-month
      }) %>%
      filter(flat_type == input$histo_flat_type)%>%
      filter(if (input$histo_town == 'all') TRUE else town == input$histo_town) # Don't filter if town is set to all
    
  })
  
  # Reactive function to calculate median rent based on input variables
  calculate_barplot_y <- reactive({
    rental_sf %>%
      group_by(!!sym(input$histo_cat_plot_variable_x)) %>% # Convert input to column name
      summarize(
        rented_count = n(),
        median_rent = median(monthly_rent, na.rm = TRUE),
        .groups = 'drop'
      )
    
  })
  
  # Generate plot based on selected tab
  output$histo_bar_plot <- renderPlotly({
    req(input$histogram_mode_selector)
    if (input$histogram_mode_selector == "Continuous") {
      # Continuous plot logic
      p <- ggplot(histo_filtered_data(),
                  aes_string(x = input$histo_plot_variable)) +
        geom_histogram(
          bins = input$histo_bin_number,
          fill = "blue",
          color = "black"
        ) +
        labs(
          title = paste("Histogram of", input$histo_plot_variable),
          x = input$histo_plot_variable,
          y = "Frequency"
        )
    } else if (input$histogram_mode_selector == "Categorical") {
      # Categorical plot logic
      if (input$histo_cat_plot_variable_y == "frequency") {
        # Frequency plot using count
        p <- ggplot(rental_sf,
                    aes_string(x = input$histo_cat_plot_variable_x)) +
          geom_bar(fill = "blue", color = "black") +
          labs(
            title = paste("Bar Plot of", input$histo_cat_plot_variable_x),
            x = input$histo_cat_plot_variable_x,
            y = "Frequency"
          )
      } else if (input$histo_cat_plot_variable_y == "median_rent") {
        # Use calculated data for median rent
        barplot_data <- calculate_barplot_y()
        p <- ggplot(
          barplot_data,
          aes_string(
            x = input$histo_cat_plot_variable_x,
            y = "median_rent"
          )
        ) +
          geom_bar(stat = "identity",
                   fill = "blue",
                   color = "black") +
          labs(
            title = paste(
              "Bar Plot of",
              input$histo_cat_plot_variable_x,
              "vs Median Rent"
            ),
            x = input$histo_cat_plot_variable_x,
            y = "Median Rent"
          )
      }
    }
    
    # Convert ggplot object to an interactive plotly object
    ggplotly(p)
  })
  
  
  
  #==========================================================
  # Choropleth Map
  #==========================================================
  choro_filtered_data <- reactive({
    req(input$choro_flat_type, input$choro_month)  # Ensure inputs are available
    rental_sf %>%
      mutate(
        town = if_else(town == 'KALLANG/WHAMPOA', 'KALLANG', town),
        year_month = format(rent_approval_date, "%Y %b")
      ) %>%
      filter(town != "CENTRAL") %>%
      filter (if (input$choro_month == "2024 Jan to Sep") {
        TRUE  # No additional filtering by choro_month
      } else {
        format(rent_approval_date, "%Y %b") == input$choro_month  # Filter by selected year-month
      }) %>%
      filter(flat_type == input$choro_flat_type)
  })
  
  calculate_choropleth_data <- reactive({
    data <- choro_filtered_data()
    
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
  
  output$choropleth <- renderTmap({
    # Get choropleth_data
    choropleth_data <- calculate_choropleth_data()
    
    if (nrow(choropleth_data) == 0) {
      return(NULL)  # Return NULL if there is no data to plot
    }
    
    if (input$choro_plot_variable == "rented_count") {
      tmap_mode("view")
      qtm(choropleth_data, fill = "rented_count")
    } else if (input$choro_plot_variable == "median_rent") {
      tmap_mode("view")
      qtm(choropleth_data, fill = "median_rent")
    }
  })
  #==========================================================
  # Scatterplot
  #==========================================================
  
  scatter_filtered_data <- reactive({
    req(input$scatter_flat_type, input$scatter_month, input$scatter_town)  # Ensure inputs are available
    rental_sf %>%
      mutate(year_month = format(rent_approval_date, "%Y %b")) %>%
      filter (if (input$scatter_month == "2024 Jan to Sep") {
        TRUE  # No additional filtering by month
      } else {
        format(rent_approval_date, "%Y %b") == input$scatter_month  # Filter by selected year-month
      }) %>%
      filter(flat_type == input$scatter_flat_type)%>%
      filter(if (input$scatter_town == 'all') TRUE else town == input$scatter_town) # Don't filter if town is set to all
    
  })
  
  output$scatter_plot <- renderPlotly({
    req(input$scatter_x, input$scatter_y, input$scatter_flat_type, input$scatter_month, input$scatter_point_color, input$scatter_point_size)
    

    # Build the scatter plot
    p <- ggplot(scatter_filtered_data(),
                aes_string(x = input$scatter_x, y = input$scatter_y)) +
      geom_point(color = input$scatter_point_color, size = input$scatter_point_size) +
      labs(
        title = paste("Scatterplot of", input$scatter_x, "vs", input$scatter_y),
        x = input$scatter_x,
        y = input$scatter_y
      ) +
      theme_minimal()  # Optional, for cleaner aesthetics
    
    if (input$scatter_add_smooth) {
      p <- p + geom_smooth(method = "lm", color = "red", se = FALSE)  # Linear regression line
    }
    
    ggplotly(p, height = 600)
    
    })
  
  #==========================================================
  # Locations of Interest
  #==========================================================
  
  output$locationMap <- renderTmap({
    req(input$dataset, input$locationTown)  # Ensure a dataset is selected
    
    # Dynamically select the dataset based on input
    data_to_plot <- switch(input$dataset,
                           "kindergarten_sf" = kindergarten_sf,
                           "childcare_sf" = childcare_sf,
                           "hawker_sf" = hawker_sf,
                           "busstop_sf" = busstop_sf,
                           "shoppingmall_sf" = shoppingmall_sf,
                           "mrt_sf" = mrt_sf,
                           "primarysch_sf" = primarysch_sf,
                           "cbd_sf" = cbd_sf)
    # Filter for town boundaries in mpsz_sf
    if(input$locationTown != 'all' && input$dataset != "cbd_sf"){
      specific_town_sf <- mpsz_sf %>% filter(PLN_AREA_N == input$locationTown)
      
      # Performing a spatial intersection
      data_to_plot <- st_intersection(data_to_plot, specific_town_sf)
    }
    tmap_options(check.and.fix = TRUE)

    # Render the map with the selected dataset
    tmap_mode("view")
    tm_shape(data_to_plot) +
      tm_dots(col = "red")
  })

  
  #==========================================================
  # Correlation Matrix
  #==========================================================
  
  # Extract independent columns
  independent_columns <- reactive({
    req(input$selected_columns)  # Ensure at least one column is selected
    rental_sf %>%
      select(all_of(input$selected_columns)) %>%  # Use all_of to select based on user input
      st_drop_geometry()
  })
  
  # Generate correlation matrix plot based on user inputs
  correlationMatrixResults <- reactive({
    # Calculate the correlation matrix
    correlation_matrix <- cor(independent_columns(), use = "pairwise.complete.obs")
    
    # Set tl.pos based on cm_type
    tl_pos <- switch(
      input$cm_type,
      "full" = "full",
      "upper" = "td",
      "lower" = "ld"
    )
    
    if (input$cm_order == "hclust") {
      corrplot(
        correlation_matrix,
        diag = FALSE,
        order = input$cm_order,
        method = input$cm_method,
        type = input$cm_type,
        hclust.method = input$cm_hclust_method,
        tl.pos = tl_pos,
        tl.cex = 0.8
      )
    } else {
      corrplot(
        correlation_matrix,
        diag = FALSE,
        order = input$cm_order,
        method = input$cm_method,
        type = input$cm_type,
        tl.pos = tl_pos,
        tl.cex = 0.8
      )
    }
    
  })
  
  # Render the correlation matrix plot in the main panel
  output$correlationMatrixPlot <- renderPlot({
    correlationMatrixResults()
  })
  
  #==========================================================
  # Multiple Linear Regression
  #==========================================================
  mlrResult <- eventReactive(input$MLRSubmit, {
    # Create the formula based on selected variables
    formula_input <- paste(
      input$mlr_dependent_variable,
      "~",
      paste(input$mlr_independent_variables, collapse = " + ")
    )
    formula <- as.formula(formula_input)
    # Run GWR with user-specified parameters
    output <- lm(
      formula = formula,
      data = rental_sf
    )
    mlr_model_result <<- output # overwrite the gwr_model
    return(output)
  })
  
  
  # Function to update the MLR button and output
  updateMLRModelResult <- function(active_tab) {
    if (active_tab == "mlr_model_info_tab") {
      # Calculate the result and update the output
      output$mlr_model_info <- renderText({
        result <- paste(capture.output(
          summary(mlrResult())
          ), collapse = "\n")
        return(result)
      })
    }else if (active_tab == "mlr_pqt_tab") {
      output$mlr_pqt_model_info <- renderText({
        result <- paste(capture.output(
          ols_regress(mlrResult())
          ), collapse = "\n")
        return(result)
      })
    } else if (active_tab == "stepwise_tab") {
      result <- mlrResult()
      # to get the user input of the stepwise selection
      if ("Forward" %in% input$stepwise_method) {
        rental_fw_mlr <<- ols_step_forward_p(
          result,
          p_val = input$stepwise_p_value_threshold,
          details = FALSE
        )
        # output the stepwise forward info
        output$stepwise_forward_info <- renderText({
          result <- paste(capture.output(
            rental_fw_mlr
          ), collapse = "\n")
          return(result)
        })
        output$stepwise_forward_plot <- renderPlot({
          plot(rental_fw_mlr)
        })
        output$stepwise_forward_parameters_plot <- renderPlot({
          ggcoefstats(rental_fw_mlr$model, sort = "ascending")
        })
      }
      # For backward
      if ("Backward" %in% input$stepwise_method) {
        rental_bw_mlr <<- ols_step_backward_p(
          result,
          p_val = input$stepwise_p_value_threshold,
          details = FALSE
        )
        # output the stepwise backward info
        output$stepwise_backward_info <- renderText({
          result <- paste(capture.output(
            rental_bw_mlr
          ), collapse = "\n")
          return(result)
        })
        output$stepwise_backward_plot <- renderPlot({
          plot(rental_bw_mlr)
        })
        output$stepwise_backward_parameters_plot <- renderPlot({
          ggcoefstats(rental_bw_mlr$model, sort = "ascending")
        })
        
      }
      #For Both
      if ("Both" %in% input$stepwise_method) {
        rental_bi_mlr <<- ols_step_both_p(
          result,
          p_val = input$stepwise_p_value_threshold,
          details = FALSE
        )
        # output the stepwise backward info
        output$stepwise_both_info <- renderText({
          result <- paste(capture.output(
            rental_bi_mlr
          ), collapse = "\n")
          return(result)
        })
        output$stepwise_both_plot <- renderPlot({
          plot(rental_bi_mlr)
        })
        output$stepwise_both_parameters_plot <- renderPlot({
          ggcoefstats(rental_bi_mlr$model, sort = "ascending")
        })
      }
    }
  }
  
  #==============================
  # Observe Event
  #==============================
  # Function to update the MLR model after the button is click 
  observeEvent(input$MLRSubmit, {
    # Detect the active tab
    active_tab <- input$mlr_tabs
    if (length(input$stepwise_method) == 0 && active_tab == "stepwise_tab") {
      shinyalert(
        title = "Selection Required",
        text = "You must select at least one stepwise method.",
        type = "warning"
      )
    }
    updateMLRModelResult(active_tab) # Method to process the model
  })
  
  
  #=======================
  # Observe GWR Tab change
  #=======================
  
  # reload the output everytime
  observeEvent(input$mlr_tabs, {
    # re-enable the submit button
    if(input$mlr_tabs != "stepwise_comparison_tab" && input$mlr_tabs != "mlr_rd_olsrr_tab" ) {
      shinyjs::enable("MLRSubmit")  # Re-enable the button after processing
      # Change button color and text back to original
      shinyjs::runjs("$('#MLRSubmit').css('background-color', '#007BFF');")  # Original color
      shinyjs::runjs("$('#MLRSubmit').css('border-color', '#007BFF');")  # Original border color
    }
    # To check which tab currently on
    if (input$mlr_tabs == "mlr_model_info_tab") {
      output$mlr_model_info <- renderText({
        if (is.null(mlr_model_result)) {
          return("Please build the MLR model first.")
        } else {
          result <- paste(capture.output(
            summary(mlr_model_result)
            ), collapse = "\n")
          return(result)
        }
      })
    }else if(input$mlr_tabs == "mlr_pqt_tab") {
      output$mlr_pqt_model_info <- renderText({
        if (is.null(mlr_model_result)) {
          return("Please build the MLR model first.")
        } else {
          result <- paste(capture.output(
            ols_regress(mlr_model_result)
          ), collapse = "\n")
          return(result)
        }
        
      })
    } else if(input$mlr_tabs == "stepwise_comparison_tab") {
      # Disable the button while processing
      shinyjs::disable("MLRSubmit")  # Disable the button
      shinyjs::runjs("$('#MLRSubmit').css('background-color', '#6c757d');")
      shinyjs::runjs("$('#MLRSubmit').css('border-color', '#6c757d');")
      # Check if any model is NULL
      if (is.null(mlr_model_result) || 
          (is.null(rental_fw_mlr$model) && 
           is.null(rental_bw_mlr$model) && 
           is.null(rental_bi_mlr$model))) {
        # Display a message to ask the user to run the models first
        shinyalert(
          title = "Models Not Run",
          text = "Please at least run the Stepwise Method once before comparing performance.",
          type = "warning"
        )
      }
      else {# load the output
        # Create a list to hold valid models
        models_to_compare <- list(MLR = mlr_model_result)
        # Check if each stepwise model is not NULL and add to the comparison list
        if (!is.null(rental_fw_mlr$model)) {
          models_to_compare <- append(models_to_compare, list(Forward = rental_fw_mlr$model))
        }
        
        if (!is.null(rental_bw_mlr$model)) {
          models_to_compare <- append(models_to_compare, list(Backward = rental_bw_mlr$model))
        }
        
        if (!is.null(rental_bi_mlr$model)) {
          models_to_compare <- append(models_to_compare, list(Both = rental_bi_mlr$model))
        }
        # If all models are available, proceed with comparing performance
        metric <- compare_performance(models_to_compare)
        output$stepwise_comparison_plot <- renderPlot({
          # Plot the comparison metric
          plot(metric)
        })
      }
    }else if (input$mlr_tabs == "mlr_rd_olsrr_tab" ) {
      # Disable the button while processing
      shinyjs::disable("MLRSubmit")  # Disable the button
      shinyjs::runjs("$('#MLRSubmit').css('background-color', '#6c757d');")
      shinyjs::runjs("$('#MLRSubmit').css('border-color', '#6c757d');")
      # Check if any model is NULL
      if (is.null(mlr_model_result)) {
        # Display a message to ask the user to run the models first
        shinyalert(title = "Models Not Run",
                   text = "Please ensure you run the MLR model at least once, either in the Model Info Tab or the Publication Quality Table Tab.",
                   type = "warning")
      }else { # if is not NULL, display the output
        output$olsrr_multicollinearity_text <- renderText({
          result <-  paste(capture.output(ols_vif_tol(mlr_model_result)), collapse = "\n")
          return(result)
        })
        
        output$olsrr_non_linearity_plot <- renderPlot({
          ols_plot_resid_fit(mlr_model_result)
        })
        
        output$olsrr_normality_plot <- renderPlot({
          ols_plot_resid_hist(mlr_model_result)
        })
        
        mlr.output <- as.data.frame(mlr_model_result$residuals)
        rental_res_sf <- cbind(rental_sf, 
                               mlr_model_result$residuals) %>%
          rename(`MLR_RES` = `mlr_model_result.residuals`)
        
        output$olsrr_MLR_RES_plot <- renderTmap({
          tmap_mode("view")
          tm_shape(mpsz_sf)+
            tmap_options(check.and.fix = TRUE) +
            tm_polygons(alpha = 0.4) +
            tm_shape(rental_res_sf) +  
            tm_dots(col = "MLR_RES",
                    alpha = 0.6,
                    style="quantile") +
            tm_view(set.zoom.limits = c(11,14))
        })
      }
    }
    else if (input$mlr_tabs == "mlr_rd_performance_forward_tab" ) {
      # Disable the button while processing
      shinyjs::disable("MLRSubmit")  # Disable the button
      shinyjs::runjs("$('#MLRSubmit').css('background-color', '#6c757d');")
      shinyjs::runjs("$('#MLRSubmit').css('border-color', '#6c757d');")
      
      # Check if the data needed is null or not
      if (is.null(rental_fw_mlr)) {
        # Display a message to ask the user to run the models first
        shinyalert(title = "Models Not Run",
                   text = "Please ensure you run the Stepwise Forware model at least once, in the Stepwise Method Tab.",
                   type = "warning")
      }else { # if is not NULL, display the output
        output$performance_forward_multicollinearity_text <- renderText({
          result <-  paste(capture.output(check_collinearity(rental_fw_mlr$model)), collapse = "\n")
          return(result)
        })
        
        output$performance_forward_multicollinearity_plot <- renderPlot({
          plot(check_collinearity(rental_fw_mlr$model)) +
            # theme is used to make the display the column name more friendly
            theme(axis.text.x = element_text (
              angle = 45, hjust = 1
            ))
        })
        
        output$performance_forward_non_linearity_plot <- renderPlot({
          out <- plot(check_model(rental_fw_mlr$model,
                                  panel = FALSE))
          out[[2]] # have 6 plot
        })
        output$performance_forward_normality_plot <- renderPlot({
          plot(check_normality(rental_fw_mlr$model))
        })
        output$performance_forward_outlier_plot <- renderPlot({
          plot(check_outliers(rental_fw_mlr$model,
                              method = "pareto"))
        })
        
      }
    }
    else if (input$mlr_tabs == "mlr_rd_performance_backward_tab" ) {
      # Disable the button while processing
      shinyjs::disable("MLRSubmit")  # Disable the button
      shinyjs::runjs("$('#MLRSubmit').css('background-color', '#6c757d');")
      shinyjs::runjs("$('#MLRSubmit').css('border-color', '#6c757d');")
      
      # Check if the data needed is null or not
      if (is.null(rental_bw_mlr)) {
        # Display a message to ask the user to run the models first
        shinyalert(title = "Models Not Run",
                   text = "Please ensure you run the Stepwise Backward model at least once, in the Stepwise Method Tab.",
                   type = "warning")
      }else { # if is not NULL, display the output
        output$performance_backward_multicollinearity_text <- renderText({
          result <-  paste(capture.output(check_collinearity(rental_bw_mlr$model)), collapse = "\n")
          return(result)
        })
        
        output$performance_backward_multicollinearity_plot <- renderPlot({
          plot(check_collinearity(rental_bw_mlr$model)) +
            # theme is used to make the display the column name more friendly
            theme(axis.text.x = element_text (
              angle = 45, hjust = 1
            ))
        })
        
        output$performance_backward_non_linearity_plot <- renderPlot({
          out <- plot(check_model(rental_bw_mlr$model,
                                  panel = FALSE))
          out[[2]] # have 6 plot
        })
        output$performance_backward_normality_plot <- renderPlot({
          plot(check_normality(rental_bw_mlr$model))
        })
        output$performance_backward_outlier_plot <- renderPlot({
          plot(check_outliers(rental_bw_mlr$model,
                              method = "pareto"))
        })
        
      }
    }
    else if (input$mlr_tabs == "mlr_rd_performance_both_tab" ) {
      # Disable the button while processing
      shinyjs::disable("MLRSubmit")  # Disable the button
      shinyjs::runjs("$('#MLRSubmit').css('background-color', '#6c757d');")
      shinyjs::runjs("$('#MLRSubmit').css('border-color', '#6c757d');")
      
      # Check if the data needed is null or not
      if (is.null(rental_bi_mlr)) {
        # Display a message to ask the user to run the models first
        shinyalert(title = "Models Not Run",
                   text = "Please ensure you run the Stepwise Both model at least once, in the Stepwise Method Tab.",
                   type = "warning")
      }else { # if is not NULL, display the output
        output$performance_both_multicollinearity_text <- renderText({
          result <-  paste(capture.output(check_collinearity(rental_bi_mlr$model)), collapse = "\n")
          return(result)
        })
        
        output$performance_both_multicollinearity_plot <- renderPlot({
          plot(check_collinearity(rental_bi_mlr$model)) +
            # theme is used to make the display the column name more friendly
            theme(axis.text.x = element_text (
              angle = 45, hjust = 1
            ))
        })
        
        output$performance_both_non_linearity_plot <- renderPlot({
          out <- plot(check_model(rental_bi_mlr$model,
                                  panel = FALSE))
          out[[2]] # have 6 plot
        })
        output$performance_both_normality_plot <- renderPlot({
          plot(check_normality(rental_bi_mlr$model))
        })
        output$performance_both_outlier_plot <- renderPlot({
          plot(check_outliers(rental_bi_mlr$model,
                              method = "pareto"))
        })
        
      }
    }
  })
  
  
  #==========================================================
  # GWR
  #==========================================================
  
  loadLocalR2Tmap <- function(input_model) {
    rental_sf_adaptive <- st_as_sf(input_model$SDF) %>%
      st_transform(crs = 3414)
    
    gwr_adaptive_output <- as.data.frame(input_model$SDF)
    
    rental_sf_adaptive <- cbind(rental_sf, gwr_adaptive_output)
    
    # Create the tmap visualization
    tm_shape(mpsz_sf) +
      tmap_options(check.and.fix = TRUE) +
      tm_polygons(alpha = 0.1) +
      tm_shape(rental_sf_adaptive) +
      tm_dots(col = "Local_R2",
              border.col = "gray60",
              border.lwd = 1) +
      tm_view(set.zoom.limits = c(11, 14))
  }
  
  #=======================
  # Based on User input
  #=======================
  gwrResult <- eventReactive(input$GWRSubmit, {
    # Disable the button while processing
    shinyjs::disable("GWRSubmit")  # Disable the button
    shinyjs::runjs("$('#GWRSubmit').css('background-color', '#6c757d');")  # Change to gray
    shinyjs::runjs("$('#GWRSubmit').css('border-color', '#6c757d');")  # Change border color
    shinyjs::runjs("$('#GWRSubmit').text('Processing...');")  # Change text to 'Processing...'
    # Create the formula based on selected variables
    formula_input <- paste(
      input$gwr_dependent_variable,
      "~",
      paste(input$gwr_independent_variables, collapse = " + ")
    )
    formula <- as.formula(formula_input)
    # Run GWR with user-specified parameters
    output <- gwr.basic(
      formula = formula,
      data = rental_sf,
      bw = if (as.logical(input$gwr_adaptive)) input$gwr_adaptive_bw else  input$gwr_fixed_bw,
      kernel = input$gwr_kernel,
      adaptive = as.logical(input$gwr_adaptive),
      longlat = as.logical(input$gwr_longlat)
    )
    gwr_model <<- output # overwrite the gwr_model
    return(output)
  })
  
  # Function to update the GWR button and output
  updateModelResult <- function(active_tab) {
    if (active_tab == "gwr_model_info_tab") {
      # Calculate the result and update the output
      output$gwr_model_info <- renderText({
        result <- paste(capture.output(gwrResult()), collapse = "\n")
        shinyjs::enable("GWRSubmit")  # Re-enable the button after processing
        # Change button color and text back to original
        shinyjs::runjs("$('#GWRSubmit').css('background-color', '#007BFF');")  # Original color
        shinyjs::runjs("$('#GWRSubmit').css('border-color', '#007BFF');")  # Original border color
        shinyjs::runjs("$('#GWRSubmit').text('Submit');")  # Reset text to 'Submit'
        return(result)
      })
    } else {
      output$gwr_local_r2_plot <- renderTmap({
        gwrResult()
        loadLocalR2Tmap(gwr_model)
        shinyjs::enable("GWRSubmit")  # Re-enable the button after processing
        # Change button color and text back to original
        shinyjs::runjs("$('#GWRSubmit').css('background-color', '#007BFF');")  # Original color
        shinyjs::runjs("$('#GWRSubmit').css('border-color', '#007BFF');")  # Original border color
        shinyjs::runjs("$('#GWRSubmit').text('Submit');")  # Reset text to 'Submit'
      })
    }
  }
  
  # Based on the current active tab, build the model and load the output
  observeEvent(input$GWRSubmit, {
    # Detect the active tab
    active_tab <- input$gwr_tabs
    shinyalert(
      title = "Confirm Submission",
      text = "Warning: The GWR model may take 4-5 minutes to process, and other pages will also be blocked during this time. Do you want to proceed?",
      type = "warning",
      showCancelButton = TRUE,
      confirmButtonText = "Yes, proceed",
      cancelButtonText = "No, cancel",
      callbackR = function(confirm) {
        if (confirm) {
          updateModelResult(active_tab) # Method to process the model
        }
      }
    )
  })
  
  #=======================
  # Observe GWR Tab change
  #=======================
  # reload the ooutput everytime
  observeEvent(input$gwr_tabs, {
    if (input$gwr_tabs == "gwr_model_info_tab") {
      output$gwr_model_info <- renderText({
        result <-  paste(capture.output(gwr_model), collapse = "\n")
        return(result)
      })
    }else {
      output$gwr_local_r2_plot <- renderTmap({
        loadLocalR2Tmap(gwr_model)
      })
    }
  })
  
  
  
  
  #==========================================================
  # Predictive Model
  #==========================================================
  
  
  
  #==========================================================
  # Data Table
  #==========================================================
  data <- data.frame(
    Name = c("Singapore Rental Flat Prices (Jan-17 to Sep-24)",
             "Master Plan 2014 Subzone Boundary (Web)", 
             "Hawker Centres Dataset",
             "Kindergarten",
             "Childcare",
             "Primary School",
             "Bus Stops Location",
             "MRT/ LRT Locations",
             "Shopping Malls Coordinates"
             ),
    Source = c("data.gov.sg", 
               "data.gov.sg",
               "data.gov.sg",
               "OneMap API",
               "OneMap API",
               "OneMap API",
               "LTA Data Mall",
               "LTA Data Mall",
               "Through Wikipedia and webscraping with the coordinates retrieved through OneMap API"
               ),
    URL = c("https://data.gov.sg/datasets/d_c9f57187485a850908655db0e8cfe651/view", 
            "https://data.gov.sg/datasets/d_d14da225fccf921049ab64238ff473d9/view", 
            "https://data.gov.sg/datasets?formats=GEOJSON%7CKML%7CSHP%7CKMZ&sort=relevancy&page=1&resultId=d_4a086da0a5553be1d89383cd90d07ecd",
            "https://www.onemap.gov.sg/apidocs/",
            "https://www.onemap.gov.sg/apidocs/",
            "https://www.onemap.gov.sg/apidocs/",
            "https://datamall.lta.gov.sg/content/dam/datamall/datasets/Geospatial/BusStopLocation_Jul2024.zip",
            "https://datamall.lta.gov.sg/content/dam/datamall/datasets/Geospatial/TrainStation_Jul2024.zip",
            "Manual Data-scrapping"
            )
  )
  # Convert URLs to clickable links
  data$URL <- ifelse(
    grepl("^http", data$URL), 
    paste0('<a href="', data$URL, '" target="_blank">', data$URL, '</a>'), 
    data$URL
  )
  output$data_table <- renderDT({
    datatable(data, escape = FALSE, options = list(pageLength = 10, searchHighlight = TRUE), )
  })
  
  
  
}

shinyApp (ui = ui, server = server)