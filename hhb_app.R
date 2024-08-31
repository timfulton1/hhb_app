# Built by Tim Fulton, June 1, 2024

source("utils.R")

demo_data <- read_excel("data/hhb_demo_data.xlsx", col_names = TRUE)

# Define UI ----
ui <- page_fillable(
  padding = 15,
  gap = 15,
  titlePanel(HTML("<b>Deoxyhemoglobin Kinetics Analysis</b>"), "HHb Kinetics"),
  style = "background-color: #D0D9E5;",
  tags$style(".progress-bar{background-color:#426FAC;}"),
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar, .js-irs-0 .irs-handle, js-irs-0 .irs-handle {background: #426FAC} .js-irs-0 .irs-handle:hover {background: #426FAC}")),
  tags$style(HTML(".popover {max-width: 1000px;width: 950px} .popover-title {font-size: 1.5em}")),
  card(
    fill = FALSE,
    max_height = 85,
    style = "border-radius: 2px",
    layout_columns(
      col_widths = c(2, 4, 3, 3),
      actionButton(inputId = "info_button", label = "Instructions", width = 150, gap = 15) %>% 
        popover(
          HTML(
            "<strong>Background</strong> <br>
            This app performs kinetic modeling of deoxyhemoglobin (HHb) data obtained using near-infrared spectroscopy. 
            The metrics that are calculated provide estimates of microvascular function during the transition from rest to exercise. <br>
            <br>
            <strong>Methods</strong> <br>
            The data are fit using a monoexponential model with a time delay according to the equation below, <br>
            <br>"),
          withMathJax("$$HHb = {HHb}_{baseline} + \\Delta{HHb} \\cdot (1-e^{[-(t-TD) / tau]})$$"),
          HTML("
            <br>
            where HHb is the HHb at any time t, baseline HHb is the average HHb during the 30 seconds of rest prior to exercise, delta HHb is the difference between the baseline and the steady state amplitude at 60 seconds,
            t is the time, TD is the time delay, and tau is the time taken to reach 63% of the steady state amplitude. Three additional variables that are calcuated are the (1) response time -
            the sum of the time delay and the tau, (2) overshoot - the difference between the amplitude and the average HHb over the final 15 seconds of exercise, and (3) RMSE - the root mean squared error of the fit.
            The timespan of the data used will range from the first fitting point to 60 seconds. <br>
            <br>
            <strong>Usage</strong> <br>
            Data can be uploaded using the browse button (visitors can use the demo data). The first fitting point defaults to 6 seconds, but it should be adjusted to the first data point after time zero (exercise start) that is higher than the baseline. The first fitting point can be adjusted using the slider on the right."),  
          title = "Instructions"),
      fileInput(inputId = "upload", label = NULL, placeholder = "Upload Excel",  multiple = FALSE, accept = ".xlsx", width = 400),
      actionButton(inputId = "load_demo", label = "Load demo data", width = 190, gap = 15),
      sliderInput("time_start", label = NULL, min = 3, max = 12, value = 6, step = 3, ticks = FALSE, post = " s")
    )
  ),
  layout_columns(
    fill = FALSE,
    gap = 15,
    height = 900,
    col_widths = c(9, 3),
    layout_column_wrap(
      gap = 15, 
      width = 1, 
      card(
        max_height = 500, 
        style = "border-radius: 2px", 
        withSpinner(
          plotlyOutput("model_plot"), 
          color = '#041E42',
          type = 5,
          size = 0.5
        )
      ),
      layout_columns(
        col_widths = c(8, 4),
        gap = 15, 
        card(max_height = 360, style = "border-radius: 2px", plotlyOutput("residual_plot")), 
        card(max_height = 360, style = "border-radius: 2px", tableOutput("metrics_table"))
        )
      ),
    layout_columns(
      fill = FALSE, 
      height = 900, 
      card(
        max_height = 817, 
        style = "border-radius: 2px", 
        tableOutput("overall_table"),
        downloadButton("export_model_data", label = "Download Data")))
  )
)

# Define Server ----
server <- function(input, output) {
  
  # Define a reactive value to store the selected dataset
  selected_df <- reactiveVal(NULL)
  
  # Load the uploaded or demo dataset based on user action
  observeEvent(input$upload, {
    req(input$upload)
    selected_df(input$upload$datapath)
  })
  
  observeEvent(input$load_demo, {
    selected_df("data/hhb_demo_data.xlsx")  # Assuming demo_df contains your demo dataset
  })
  
  # Clean the selected data frame
  upload_df <- reactive({
    req(selected_df())
    # Perform any necessary cleaning/transformation on the selected dataset
    load_and_process_data(selected_df())
  })

  
  # Create the model fit data frame
  final_data <- reactive({
    
    # assign the uploaded data to a data frame
    hhb_data <- upload_df()
    
    # error control
    validate(
      need(hhb_data$hhb_normal[which(hhb_data$time == input$time_start)] > 0, "Please select a time point higher than the baseline")
    )
    
    ## Model Fitting ##
    # average of baseline values
    hhb_baseline = round((mean(hhb_data$hhb[1:10])), 2)
    
    # subset to only include positive exercise HHb values
    hhb_model_df <- hhb_data %>% 
      filter(hhb_normal > 0 & time >= input$time_start & time < 63)
    
    # reset time to 0 
    hhb_model_df$time <- seq(0, (60 - input$time_start), 3)
    
    # run model
    hhb_model_fit <- nls(hhb ~  hhb_baseline + hhb_delta *(1-exp(-(time/hhb_tau))), 
                         data = hhb_model_df, 
                         start = list(hhb_delta = 10, hhb_tau = 3))
    
    # assign parameter values to variables
    fit_delta <- round((summary(hhb_model_fit)$parameters[1,1]), 2)
    fit_tau <- round((summary(hhb_model_fit)$parameters[2,1]), 2)
    
    # calculate the time delay
    time_delay_temp <- (log(1-(hhb_baseline/(fit_delta + hhb_baseline))))*(-fit_tau) 
    hhb_time_delay <- round(input$time_start - time_delay_temp, 2)
    
    # new data frame for graphing
    kinetic_fit_df <- data.frame("time" = seq(hhb_time_delay, 60, by=0.1))
    
    # add model values to DF
    kinetic_fit_df$model_fit <- round(hhb_baseline + fit_delta *(1-exp(-((kinetic_fit_df$time-hhb_time_delay)/fit_tau))), 2)
    
    # update the column names
    colnames(kinetic_fit_df) <- c("Time", "HHb")
    
    
    ## Create data frame for residuals ##
    hhb_residuals <- data.frame("Time" = round(seq(input$time_start, 60, by=3)))
    hhb_residuals$HHb <- round(hhb_model_df$hhb, 2)
    hhb_residuals$Model <- round(hhb_baseline + fit_delta *(1-exp(-((hhb_residuals$Time-hhb_time_delay)/fit_tau))), 2)
    hhb_residuals$Residual <- round((hhb_residuals$HHb - hhb_residuals$Model), 2)
    
  
    ## Create table of metrics ##
    # calculate rse on time delay model fit
    hhb_rmse <- round((sqrt(sum(hhb_residuals$Residual*hhb_residuals$Residual)/(count(hhb_residuals)-2))),2)
    
    # average of last 5 contractions
    hhb_final_five = round(mean(hhb_data$hhb[86:90]), 2)
    
    #calculate mean response time
    hhb_response_time <- round((fit_tau + hhb_time_delay), 2)
    
    #calculate max
    hhb_max <- round((hhb_baseline + fit_delta), 2)
    
    #calculate overshoot
    hhb_overshoot <- round((fit_delta + hhb_baseline - hhb_final_five), 2)
    
    metrics_df <- data.frame(Variable = c("Baseline", "Delta", "Amplitude", "Time Delay", "Tau", "Response Time", "Overshoot", "RMSE"), 
                                      Value =  as.numeric(c(hhb_baseline, fit_delta, hhb_max, hhb_time_delay, fit_tau, hhb_response_time, hhb_overshoot, hhb_rmse)))
    
    ## Output final data
    final_data <- list(kinetic_fit_df, hhb_residuals, metrics_df)
    
  })
  
  ## Render the plot with raw data and kinetic fit data
  output$model_plot <- renderPlotly({
    
    temp_data <- upload_df()
    
    colnames(temp_data) <- c("Time", "HHb")
    
    fitted_data <- final_data()[[1]]
    
    ggplotly(
      ggplot(data = temp_data, mapping = aes(x = Time, y = HHb)) +
        theme_pubr() +
        #theme(panel.grid.major.y = element_line(color = "gray", linetype = "dotted")) + 
        labs(
          #title = "Raw Data and Model Fit",
          x = "Time (s)",
          y = "HHb (% physiological range)"
        ) +
        scale_x_continuous(breaks=seq(-30, 240, by = 30)) +
        scale_y_continuous(breaks=seq(0, 100, by = 20)) +
        expand_limits(y = c(0, 100)) +
        geom_point(
          shape = 21,
          alpha = 1.0,
          fill = "white",
          color = "black",
          size = 3
          ) +
        geom_line(
          data = fitted_data, 
          aes(x = Time , y = HHb), 
          color = "#DB4128", 
          linewidth = 0.8
        )
    )
  })
  
  
  ## Render the plot with raw data and kinetic fit data
  output$residual_plot <- renderPlotly({
    
    # assign residual data to new data frame
    residual_data <- final_data()[[2]]
    
    # plot
    ggplotly(
      ggplot(residual_data, aes(x = Time, y = Residual)) +
        theme_pubr() +
        labs(
          x = "Time (s)",
          y = "Residual"
        ) +
        scale_x_continuous(breaks=seq(0, 60, by = 6)) +
        expand_limits(x = c(0, 60)) +
        expand_limits(y = c(-10, 10)) +
        geom_line(
          y = 0, 
          color = "black", 
          linetype = "dashed"
        ) +
        geom_point(
          shape = 21,
          alpha = 1.0,
          fill = "gray75",
          color = "black",
          size = 2
        ) +
        geom_line()
    )
    
  })
  
  
  ## Render table for metrics
  output$metrics_table <- renderTable(
    final_data()[[3]],
    hover = TRUE,
    align = c("lr")
  )
  
  
  ## Render table for HHb and Model
  output$overall_table <- renderTable({
    data <- final_data()[[2]]
    # Convert the first column to integers
    data[, 1] <- as.integer(data[, 1])
    return(data)
  }, hover = TRUE, align = c("rccc"))
  
  
  ## Output Raw and Model Fit Table for Download ##
  output$export_model_data <- downloadHandler(
    
    filename = function() {
      if (!is.null(input$upload)) {
        # For user-uploaded file
        uploaded_filename <- input$upload$name
      } else {
        # For demo data file
        uploaded_filename <- "demo.xlsx"
      }

      # Remove the file extension
      filename_without_extension <- sub("\\.xlsx$", "", uploaded_filename)
      
      paste0(basename(filename_without_extension), "_model_data.xlsx")
    },
    
    content = function(file) {
      write.xlsx(final_data()[[2]], file)
    }
  )
  
}


# Run the app ----
shinyApp(ui = ui, server = server)
