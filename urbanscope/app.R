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
  shinycssloaders,
  shinyjs,
  shinyalert
)

rental_sf <- read_rds('data/rds/rental_sf.rds')
mpsz_sf <- read_rds('data/rds/mpsz_grped_by_town.rds')
gwr_model <- read_rds('data/rds/gwr_adaptive.rds')

categorical_cols <- c("town", "rent_approval_date", "flat_type", "region")

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
                inputId = "month",
                label = "Month",
                choices = c('2024 Jan to Sep' = '2024 Jan to Sep', unique(
                  format(rental_sf$rent_approval_date, "%Y %b")
                )),
                selected = '2024 Jan to Sep'
              ),
              selectInput(
                inputId = "flat_type",
                label = "Flat Type",
                choices = sort(unique(rental_sf$flat_type)),
                selected = sort(unique(rental_sf$flat_type))[1]
              ),
              selectInput(
                inputId = "town",
                label = "Town",
                choices = sort(unique(rental_sf$town)),
                selected = sort(unique(rental_sf$town))[1]
              ),
              sliderInput(
                inputId = "bin_number",
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
              # radioButtons(
              #   inputId = "flat_type_radio",
              #  label = "Select Flat Type",
              # choices = sort(unique(rental_sf$flat_type)),
              #  selected = sort(unique(rental_sf$flat_type))[1],
              # inline = TRUE
              #)
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
    # Chloropleth Map
    #==========================================================
    tabPanel("Chloropleth Map", sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "chloro_plot_variable",
          label = "Select Plot Variable",
          choices = c("Median Rent" = "median_rent", "Count of Rented Flats" = "rented_count"),
          selected = "median_rent"
        ),
        selectInput(
          inputId = "month",
          label = "Month",
          choices = c('2024 Jan to Sep' = '2024 Jan to Sep', unique(
            format(rental_sf$rent_approval_date, "%Y %b")
          )),
          selected = '2024 Jan to Sep'
        ),
        selectInput(
          inputId = "flat_type",
          label = "Flat Type",
          choices = sort(unique(rental_sf$flat_type)),
          selected = sort(unique(rental_sf$flat_type))[1]
        )
      ),
      mainPanel(
        tmapOutput("chloropleth", width = "100%", height = 580),
        tags$div(style = "margin-top: 20px;"),
        wellPanel(
          uiOutput("chloro_stats_title"),
          # Output for dynamic title
          tableOutput("chloro_statistics")
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
        )
      ),
      mainPanel(plotlyOutput("scatter_plot"))
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
  chloro_summary_statistics <- reactive({
    data <- calculate_chloropleth_data()  # Get grouped data
    filtered_data <- chloro_filtered_data()  # Get filtered data
    calculate_summary_statistics(data, input$chloro_plot_variable, filtered_data)  # Call the function with the filtered data
  })
  
  output$chloro_statistics <- renderTable({
    chloro_summary_statistics()  # This remains unchanged
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$chloro_stats_title <- renderUI({
    req(input$chloro_plot_variable, input$flat_type, input$month)  # Ensure all inputs are available
    
    # Construct the dynamic title with smaller font for Flat Type and Month
    title_text <- paste(
      "Summary Statistics for:",
      input$chloro_plot_variable,
      "<br><span style='font-size: 0.85em; color: #555;'><strong>Flat Type:</strong> ",
      input$flat_type,
      "| <strong>Month:</strong> ",
      input$month,
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
    req(input$flat_type, input$month, input$town)  # Ensure inputs are available
    rental_sf %>%
      mutate(year_month = format(rent_approval_date, "%Y %b")) %>%
      filter (if (input$month == "2024 Jan to Sep") {
        TRUE  # No additional filtering by month
      } else {
        format(rent_approval_date, "%Y %b") == input$month  # Filter by selected year-month
      }) %>%
      filter(flat_type == input$flat_type) %>%
      filter(town == input$town)
    
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
          bins = input$bin_number,
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
  # Chloropleth Map
  #==========================================================
  chloro_filtered_data <- reactive({
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
    data <- chloro_filtered_data()
    
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
    
    if (input$chloro_plot_variable == "rented_count") {
      tmap_mode("view")
      qtm(chloropleth_data, fill = "rented_count")
    } else if (input$chloro_plot_variable == "median_rent") {
      tmap_mode("view")
      qtm(chloropleth_data, fill = "median_rent")
    }
  })
  #==========================================================
  # Scatterplot: TBD
  #==========================================================
  output$scatter_plot <- renderPlotly({
    req(input$scatter_x, input$scatter_y)  # Ensure x and y inputs are selected
    
    # Build the scatter plot
    p <- ggplot(rental_sf,
                aes_string(x = input$scatter_x, y = input$scatter_y)) +
      geom_point(color = "blue", size = 2) +
      labs(
        title = paste("Scatterplot of", input$scatter_x, "vs", input$scatter_y),
        x = input$scatter_x,
        y = input$scatter_y
      ) +
      theme_minimal()  # Optional, for cleaner aesthetics
    
    ggplotly(p)  # Convert ggplot to interactive Plotly plot
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
    if(input$mlr_tabs != "stepwise_comparison_tab") {
      shinyjs::enable("MLRSubmit")  # Re-enable the button after processing
      # Change button color and text back to original
      shinyjs::runjs("$('#MLRSubmit').css('background-color', '#007BFF');")  # Original color
      shinyjs::runjs("$('#MLRSubmit').css('border-color', '#007BFF');")  # Original border color
    }
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
      } else {
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
      bw = if (as.logical(input$gwr_adaptive)) 54 else 450,    # 54 is for adaptive, 450 for Fix
      # Based on Take Home Ex 03 for Adaptive
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
  
  
  
  
}

shinyApp (ui = ui, server = server)