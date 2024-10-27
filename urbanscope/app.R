pacman::p_load(
  shiny,
  sf,
  tmap,
  bslib,
  tidyverse,
  sfdep,
  GWmodel,
  corrplot,
  shinydashboard,
  shinythemes,
  plotly
)

rental_sf <- read_rds('data/rds/rental_sf.rds')
mpsz_sf <- read_rds('data/rds/mpsz_grped_by_town.rds')

categorical_cols <- c("town", "rent_approval_date", "flat_type", "region")

#========================#
###### Shiny UI ######
#========================#

ui <- navbarPage(
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
          # Update to the range of your independent columns
          selected = colnames(rental_sf)[7:19]  # Default to all columns or any specific default
        ),
      ),
      
      mainPanel(
        plotOutput("correlationMatrixPlot", width = "100%", height = 580)
      )
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
  # GWR
  #==========================================================
  
  
  
  
  
  
  #==========================================================
  # Predictive Model
  #==========================================================
  
  
  
  #==========================================================
  # Data Table
  #==========================================================
  
  
  
  
}

shinyApp (ui = ui, server = server)