library(shiny)
library(plotly)

source("util.R")
max_num_studies = 1000

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Clinical Trials Query"),

  # feature 2: Data Summary Table
  wellPanel(
    h3("Data Summary"),
    verbatimTextOutput("data_summary") # Output for the data summary
  ),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("brief_title_kw", "Brief title keywords"),
      
      dateInput("start_date", "Start Date", value = NULL, max = Sys.Date()),
      dateInput("end_date", "End Date", value = Sys.Date(), max = Sys.Date()),
      
      # feature 1: Download CSV button
      downloadButton("downloadData", "Download CSV"),
      
      # we add a check box here for sponsor type of studies
      checkboxGroupInput("source_class", 
                         label = h3("Sponsor type"), 
                         choices = list("Federal" = "FED", 
                                        "Individual" = "INDIV", 
                                        "Industry" = "INDUSTRY",
                                        "Network" = "NETWORK",
                                        "NIH" = "NIH",
                                        "Other" = "OTHER",
                                        "Unknown" = "UNknown")),
      
      # feature 5: outcomes drop-down menu
      selectInput("source_outcomes", 
                  label = h3("Select Outcome Type"),
                  choices = c("Primary", "Secondary", 
                              "Other Pre-specified", "Post-Hoc")),
      
      # feature 6: study_type check box
      checkboxGroupInput("source_type", 
                         label = h3("Study type"), 
                         choices = list("Expanded Access" = "Expanded Access", 
                                        "Interventional" = "Interventional", 
                                        "Observational" = "Observational",
                                        "Observational [Patient Registry]" = "Observational [Patient Registry]",
                                        "NA" = "NA"))
   ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
         type = "tabs",
         # Change this in ui.R
         tabPanel("Phase", plotlyOutput("phase_plot")),
         tabPanel("Concurrent", plotlyOutput("concurrent_plot"))
       ),
      dataTableOutput("trial_table")
    )
  )
)


# Define server logic required to draw a histogram

plot_phase_histogram = function(studies_df) {
  studies_df <- studies_df %>%
    mutate(phase = factor(phase, levels = all_phases)) %>%
    count(phase) %>%
    complete(phase = all_phases, fill = list(n = 0)) # Ensure all phases are included
  
  # Create ggplot object
  p <- ggplot(studies_df, aes(x = phase, y = n)) +
    geom_bar(stat = "identity") + # geom_col is an alias for geom_bar(stat="identity")
    scale_x_discrete(limits = all_phases) + # Make sure the x-axis is uniform
    theme_bw() +
    xlab("Phase") +
    ylab("Count") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) # Rotate the x-axis labels for better legibility
  
  # Convert to plotly object
  ggplotly(p)
}

server <- function(input, output) {
  options(warn = -1)
  
  #reactive()---allows for automatic re-evaluation and updating of outputs when inputs change.
  #When the user changes the input, 
  #the reactive expression automatically re-evaluates, and the displayed result updates accordingly.
  get_studies = reactive({
    ret = studies
    if (input$brief_title_kw != "") {
      si = input$brief_title_kw |>
        strsplit(",") |>
        unlist() |>
        trimws()
      ret = query_kwds(ret, si, "brief_title", match_all = TRUE)
    }
    
    if (!is.null(input$source_type)) {
      ret = ret |> filter(study_type %in% !!input$source_type)
    }
    

    if (!is.null(input$source_class)) {
      ret = ret |> filter(source_class %in% !!input$source_class)
    }
    
    if(!is.null(input$source_outcomes)){
      ret = ret |> 
        inner_join(outcomes, by = "nct_id") |>
        filter(outcome_type %in% !!input$source_outcomes)
    }
    
    
    if (!is.null(input$start_date) && !is.null(input$end_date)) {
      ret = ret |> 
        filter(start_date >= !!input$start_date & completion_date <= !!input$end_date)
    }
    
    ret |> 
      head(max_num_studies) |> 
      collect()
  })
  

  # phase plot
  output$phase_plot = renderPlotly({
    gg <- get_studies() |>
      plot_phase_histogram()
    ggplotly(gg) # Converts ggplot2 to plotly object
  })

  # concurrent_plot
  output$concurrent_plot = renderPlotly({
    # Try to generate the ggplot object inside a tryCatch to catch any errors
    concurrent_plot <- tryCatch({
      get_studies() %>%
        select(start_date, completion_date) %>%
        get_concurrent_trials() %>%
        ggplot(aes(x = date, y = count)) +
        geom_line() +
        xlab("Date") +
        ylab("Count") +
        theme_bw()
    }, error = function(e) {
      # This will print the error to the console
      print(e)
      return(NULL)  # Return NULL if there was an error
    })
    
    # Only convert to Plotly if concurrent_plot is not NULL
    if (!is.null(concurrent_plot)) {
      ggplotly(concurrent_plot)
    }
  })
  
  
  # trial table
  output$trial_table = renderDataTable({
    get_studies() |> 
      select(nct_id, brief_title, start_date, completion_date) |>
      rename(`NCT ID` = nct_id, `Brief Title` = brief_title,
             `Start Date` = start_date, `Completion Date` = completion_date)
  })
  
  # feature 1: Downloadable Reports
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("clinical-trials-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Extract the data as a data frame from the reactive expression
      data_to_download <- get_studies()
      # Write to a CSV file
      write.csv(data_to_download, file, row.names = FALSE)
    }
  )
  
  # feature 2: Data Summary Table
  output$data_summary <- renderText({
    studies_df <- get_studies()
    
    # Calculate summary metrics
    total_num_trials <- nrow(studies_df)
    average_duration <- mean(studies_df$completion_date - studies_df$start_date, na.rm = TRUE)
    num_trials_per_phase <- table(factor(studies_df$phase, levels = all_phases))
    
    # Create the summary text
    summary_text <- paste(
      "Total number of trials: ", total_num_trials, "\n",
      "Average trial duration: ", round(average_duration), " days\n",
      "Number of trials per phase:\n", 
      paste(names(num_trials_per_phase), num_trials_per_phase, sep = ": ", collapse = "\n"),
      sep = ""
    )
    
    return(summary_text)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

