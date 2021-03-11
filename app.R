library(dygraphs)
library(dplyr)
library(xts)
library(shiny)
library(shinyBS)
library(dualR)

PYTHON_DEPENDENCIES = c('sweat')

source("utils.R")

options(shiny.maxRequestSize=30*1024^2) 

ui <- fluidPage(
  mainPanel(h1("Dual Power Comparison"),
            br(),
            p("This application accepts one or two (overlapping) fit files and creates a comparison report."),
            p("dualR is being provided as a service to the community. It is free and open source."),
            p(HTML('See <a href="https://github.com/dblodgett-cycling/dualR" target="_blank">the github repository</a> for more information or to ask questions.')),
            p(HTML(paste0("Three input files can be supplied: <strong>Primary, verification, and validation</strong>. ", 
                          "The expectation is that <strong>'primary'</strong> comes from a virtual cycling platform, ", 
                          "<strong>'verification'</strong> is a second recording of the same physical power source, ", 
                          "and <strong>'validation'</strong> is a second physical power source."))),
            fixedRow(column(width = 5,
                            fileInput("f1", 
                                      label = "Primary", 
                                      multiple = FALSE, 
                                      accept = c(".fit", ".zip")),
                            textInput(inputId = "f1_label", label = "File 1 Label",
                                      width = "200px",
                                      placeholder = "e.g. Race Recording"),
                            checkboxInput("extraf1", "Add verification of primary", value = FALSE),
                            conditionalPanel(condition = "input.extraf1",
                                             fileInput("f1_2", 
                                                       label = "Verification", 
                                                       multiple = FALSE, 
                                                       accept = c(".fit", ".zip")),
                                             textInput(inputId = "f1_2_label", label = "Verification Label",
                                                       width = "200px",
                                                       placeholder = "e.g. Secondary Trianer Recording"))),
                     column(width = 5,
                            fileInput("f2", "Validation", multiple = FALSE, accept = c(".fit", ".zip")),
                            textInput(inputId = "f2_label", label = "Validation Label",
                                      width = "200px",
                                      placeholder = "e.g. Power Meter Verification")
                            ),
                     column(width = 2, br(),
                            p("", 
                              bsButton("q1", label = "", 
                                       icon = icon("info"), 
                                       style = "info", 
                                       size = "extra-small")),
                            bsPopover(id = "q1", title = "Upload Help",
                                      content = "If upload fails, try zipping your fit file and upload again.",
                                      placement = "right", 
                                      trigger = "focus", 
                                      options = list(container = "body")))),
            checkboxInput("demo", "Demo: Check to use demo files."),
            conditionalPanel(condition = "output.show",
                             checkboxInput("trim", label = "Trim timeseries to overlapping parts.", value = FALSE),
                             checkboxInput("offset", label = "Show timeseries offset controls."), value = FALSE),
            conditionalPanel(condition = "input.offset && output.show",
                             dygraphOutput("summary_dygraph"),
                             fixedRow(
                               column(width = 5,
                                      numericInput("f1_offset", "Primary Offset",
                                                   value = 0, step = 1),
                                      conditionalPanel(condition = "input.extraf1",
                                                       numericInput("f1_2_offset", "Verification Offset",
                                                                    value = 0, step = 1))
                               ),
                               column(width = 5,
                                      numericInput("f2_offset", "Validation Offset", 
                                                   value = 0, step = 1)
                               ),
                               column(width = 2, br(),
                                      p("", 
                                        bsButton("q2", label = "", 
                                                 icon = icon("info"), 
                                                 style = "info", 
                                                 size = "extra-small")),
                                      bsPopover(id = "q2", title = "Offset Help",
                                                content = "Enter desired shift in integer seconds.",
                                                placement = "right", 
                                                trigger = "focus", 
                                                options = list(container = "body")))
                             )
            ),
            actionButton(inputId = "generate", label = "Run/re-run Report", width = "200px"),
            br(),
            conditionalPanel(condition = "output.show",
                             downloadButton("run_report", label = "Download Report"),
                             h3("Power Source Metadata"),
                             checkboxInput("showdetails", label = "Show Full Device Tables", value = FALSE),
                             htmlOutput("f1_meta"),
                             h4("Connected Device Metadata"),
                             tableOutput("fit_1_ds"),
                             conditionalPanel(condition = "input.showdetails",
                                              tableOutput("fit_1_devices")
                             ),
                             br(), br(),
                             htmlOutput("f1_2_meta"),
                             htmlOutput("fit_1_2_device_heading"),
                             tableOutput("fit_1_2_ds"),
                             conditionalPanel(condition = "input.showdetails",
                                              tableOutput("fit_1_2_devices")
                             ),
                             br(), br(),
                             htmlOutput("f2_meta"),
                             htmlOutput("fit_2_device_heading"),
                             tableOutput("fit_2_ds"),
                             conditionalPanel(condition = "input.showdetails",
                                              tableOutput("fit_2_devices")
                             ),
                             br(),
                             h3("Critical Power Comparison"),
                             tableOutput("powerCurve_table"),
                             plotOutput("powerCurve_plot"),
                             br(),
                             p("Double click for zoom full."),
                             dygraphOutput("power_dygraph"),
                             dygraphOutput("hr_dygraph"),
                             dygraphOutput("ele_dygraph"),
                             dygraphOutput("cad_dygraph"))
  )
  
)

server <- function(input, output, session) {
  
  if(Sys.getenv("FITCSVTOOL") == "") {
    # ------------------ App virtualenv setup (Do not edit) ------------------- #
    
    virtualenv_dir = Sys.getenv('VIRTUALENV_NAME')
    python_path = Sys.getenv('PYTHON_PATH')
    
    app_env$ready <- FALSE
    
    env_ready <- FALSE
    
    try({
      reticulate::use_virtualenv(virtualenv_dir, required = TRUE)
      sys <- reticulate::import("sys")
      env_ready <- all(PYTHON_DEPENDENCIES %in% names(sys$modules))
    }, silent = TRUE)
    
    if(!env_ready) {
      
      reticulate::virtualenv_create(envname = virtualenv_dir, python = python_path, 
                                    system_site_packages = TRUE)
      reticulate::virtualenv_install(virtualenv_dir, packages = c(PYTHON_DEPENDENCIES),
                                     ignore_installed=FALSE)
      
      reticulate::use_virtualenv(virtualenv_dir, required = TRUE)
      
    }
  }
  
  observeEvent(input$generate, {
    
    output$status_message <- renderUI(paste("Starting Process", conf$f1$label))
    
    showModal(modalDialog(title = "Loading...", 
                          "Sit tight, this won't take long."))
    
    on.exit(removeModal())
    
    error_condition <- FALSE
    
    conf <- get_conf(input)
    
    if(is.null(conf$f1$f)) return()
    
    fit <- get_fit_data(conf, input$trim)
    
    output$f1_meta <- renderUI(HTML(fit$f1$m))
    
    output$fit_1_devices <- function() get_devices_table(fit$f1$d)
    
    output$fit_1_ds <- function() get_device_summary_table(fit$f1$s)
    
    if(!is.null(fit$f1_2$m)) {
      output$f1_2_meta <- renderUI(HTML(fit$f1_2$m))
    
      output$fit_1_2_devices <- function() get_devices_table(fit$f1_2$d)
      
      output$fit_1_2_ds <- function() get_device_summary_table(fit$f1_2$s)
      
      output$fit_1_2_device_heading <- renderUI(HTML("<h4>Connected Device Metadata</h4>"))
    }
    
    if(!is.null(fit$f2$m)) {
    
      output$f2_meta <- renderUI(HTML(fit$f2$m))
    
      output$fit_2_devices <- function() get_devices_table(fit$f2$d)
    
      output$fit_2_ds <- function() get_device_summary_table(fit$f2$s)
      
      output$fit_2_device_heading <- renderUI(HTML("<h4>Connected Device Metadata</h4>"))
    }
    
    tryCatch({
      
      maxes <- list(m1 = get_maxes(fit$f1$f),
                    m1_2 = NULL, m2 = NULL)

      if(!is.null(fit$f1_2$f)) maxes$m1_2 <- get_maxes(fit$f1_2$f)
      
      if(!is.null(fit$f2$f)) maxes$m2 <- get_maxes(fit$f2$f)
      
    }, error = function(e) {
      showNotification(paste("Error calculating critical powers. Original error was:", e), 
                       type = "error", duration = NULL)
      return()
    })
    
      output$powerCurve_table <- function() {
        get_powercurve_table(maxes, conf)
      }
      
      output$powerCurve_plot <- renderPlot(powercurve_plot(maxes, conf))
      
      dat <- get_dygraph_data(fit, conf)
      
      app_env$rmd_params$fit <- fit
      app_env$rmd_params$conf <- conf
      app_env$rmd_params$maxes <- maxes
      app_env$rmd_params$d <- dat
      
      output$summary_dygraph <- renderDygraph({
        get_dygraph(dat$power)
      })
      
      output$power_dygraph <- renderDygraph({
        get_dygraph(dat$power)
      })
      
      if("elevation" %in% names(dat)) {
        output$ele_dygraph <- renderDygraph({
          get_dygraph(dat$elevation)
        })
      }
      
      if("heartrate" %in% names(dat)) {
        output$hr_dygraph <- renderDygraph({
          get_dygraph(dat$heartrate)
        })
      }
      
      if("cadence" %in% names(dat)) {
        output$cad_dygraph <- renderDygraph({
          get_dygraph(dat$cadence)
        })
      }
      
      app_env$ready <- TRUE
  })
  
  output$show <- reactive({
    app_env$ready
  })
  
  outputOptions(output, 'show', suspendWhenHidden = FALSE)
  
  output$run_report <- downloadHandler(
    filename = "dualReport.html",
    content = function(file) {
      
      params <- list(fit = app_env$rmd_params$fit,
                     conf = app_env$rmd_params$conf,
                     maxes = app_env$rmd_params$maxes,
                     dat = app_env$rmd_params$d)
      
      rmarkdown::render("compare.Rmd", output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
}

app_env <- reactiveValues(rmd_params = list(),
                          ready = FALSE)

shinyApp(ui, server)
