library(dygraphs)
library(dplyr)
library(xts)
library(shiny)
library(shinyBS)
library(dualR)

PYTHON_DEPENDENCIES = c('sweat')

source("utils.R")

ui <- fluidPage(
  mainPanel(h1("Dual Power Comparison"),
            br(),
            p("This application accepts one or two (overlapping) fit files and creates a comparison report."),
            p("dualR is being provided as a service to the community. It is free and open source."),
            p(HTML('See <a href="https://github.com/dblodgett-cycling/dualR" target="_blank">the github repository</a> for more information or to ask questions.')),
            fixedRow(column(width = 5,
                            fileInput("f1", 
                                      label = "Fit 1", 
                                      multiple = FALSE, 
                                      accept = c(".fit", ".zip")),
                            textInput(inputId = "f1_label", label = "File 1 Label",
                                      width = "200px",
                                      placeholder = "e.g. Power Meter")),
                     column(width = 5,
                            fileInput("f2", "Fit 2", multiple = FALSE, accept = c(".fit", ".zip")),
                            textInput(inputId = "f2_label", label = "File 2 Label",
                                      width = "200px",
                                      placeholder = "e.g. Trainer")),
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
                             checkboxInput("offset", label = "Show timeseries offset controls."),),
            conditionalPanel(condition = "input.offset && output.show",
                             dygraphOutput("summary_dygraph", height = "100px"),
                             fixedRow(
                               column(width = 5,
                                      numericInput("f1_offset", "Fit 1 Offset",
                                                   value = 0, step = 1)
                               ),
                               column(width = 5,
                                      numericInput("f2_offset", "Fit 2 Offset", 
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
                             htmlOutput("f1_meta"),
                             h4("Connected Device Metadata"),
                             tableOutput("fit_1_devices"),
                             br(), br(),
                             htmlOutput("f2_meta"),
                             htmlOutput("fit_2_device_heading"),
                             tableOutput("fit_2_devices"),
                             br(),
                             h3("Critical Power Comparison"),
                             fixedRow(column(width = 3,
                                             br(),
                                             br(),
                                             tableOutput("powerCurve_table")),
                                      column(width = 9,
                                             plotOutput("powerCurve_plot"))),
                             p("NOTE: Critical powers are calculated on the overlapping portion of inputs only."),
                             br(),
                             p("Double click for zoom full."),
                             dygraphOutput("power_dygraph"),
                             dygraphOutput("hr_dygraph"),
                             dygraphOutput("ele_dygraph"),
                             dygraphOutput("cad_dygraph"))
  )
  
)


server <- function(input, output, session) {
  
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
    
    reticulate::virtualenv_create(envname = virtualenv_dir, python = python_path)
    reticulate::virtualenv_install(virtualenv_dir, packages = c(PYTHON_DEPENDENCIES),
                                   ignore_installed=FALSE)
    
    reticulate::use_virtualenv(virtualenv_dir, required = TRUE)
    
  }
  
  reticulate::source_python("inst/python/read_fit.py")
  reticulate::source_python("inst/python/fit_meta.py")
  
  observeEvent(input$generate, {
    
    error_condition <- FALSE
    
    if(input$demo) {
      in_fit_1 <- "inst/fit/fit1.fit"
      in_fit_1_label <- "Demo Source 1"
      f1_offset <- input$f1_offset
      in_fit_2 <- "inst/fit/fit2.fit"
      in_fit_2_label <- "Demo Source 2"
      f2_offset <- input$f2_offset
    } else {
      
      if(is.null(input$f1)) {
        
        showNotification("Upload fit file first.", type = "error")
        return()
        
      } else {
        
        if(input$f1_label == "") {
          showNotification("Provide label for first file.", type = "error")
          return()
        }
        
        in_fit_1 <- check_fit(input$f1$datapath)
        
        in_fit_1_label <- input$f1_label
        
        f1_offset <- input$f1_offset
        
      }
      
      if(!is.null(input$f2)) {
        
        if(is.null(input$f2_label)) {
          showNotification("Provide label for secton file.", type = "error")
          return()
        }
        
        in_fit_2 <- check_fit(input$f2$datapath)
        
        in_fit_2_label <- input$f2_label
        
        f2_offset <- input$f2_offset
      }
      
    }
    
    tryCatch({
      
      fit_1 <- read_fit_file(in_fit_1)
      
      if(abs(f1_offset) > 0) {
        fit_1$datetime <- as.POSIXct(fit_1$datetime, tz = "GMT")
        
        fit_1$datetime <- fit_1$datetime + f1_offset
      }
    },
    error = function(e) {
      showNotification(paste("Error reading fit file 1: \n", e), type = "error", duration = NULL)
      return()
    })
    
    f1_meta <- pretty_fm(get_fit_meta(in_fit_1), in_fit_1_label)
    output$f1_meta <- renderUI(HTML(f1_meta))
    
    f1_devices <- get_device_meta(in_fit_1)
    output$fit_1_devices <- function() get_devices_table(f1_devices)
    
    if(exists("in_fit_2")) {
      
      tryCatch({
        fit_2 <- read_fit_file(check_fit(in_fit_2))
        
        if(abs(f2_offset) > 0) {
          fit_2$datetime <- as.POSIXct(fit_2$datetime, tz = "GMT")
          
          fit_2$datetime <- fit_2$datetime + f2_offset
        }
        
        if(input$trim) {
          fit_1 <- get_overlapping(fit_1, fit_2)
          fit_2 <- get_overlapping(fit_2, fit_1)
        }
      },
      error = function(e) {
        showNotification(paste("Error reading fit file 2: \n", e), type = "error", duration = NULL)
        return()
      })
      
      f2_meta <- pretty_fm(get_fit_meta(in_fit_2), in_fit_2_label)
      output$f2_meta <- renderUI(HTML(f2_meta))
      
      f2_devices <- get_device_meta(in_fit_2)
      output$fit_2_devices <- function() get_devices_table(f2_devices)
      output$fit_2_device_heading <- renderUI(HTML("<h4>Connected Device Metadata</h4>"))
      
    } else {
      
      fit_2 <- NULL
      f2_meta <- NULL
      in_fit_2_label <- NULL
      f2_devices <- NULL
      
    }
    
    tryCatch({
      if(!is.null(fit_2)) {
        
        max_1 <- get_maxes(fit_1, fit_2)
        max_2 <- get_maxes(fit_2, fit_1)
        
      } else {
        
        max_1 <- get_maxes(fit_1)
        max_2 <- NULL
        
      }
    }, error = function(e) {
      showNotification(paste("Error calculating critical powers. Original error was:", e), 
                       type = "error", duration = NULL)
      return()
    })
    
    if(exists("max_1")) {
      output$powerCurve_table <- function() {
        get_powercurve_table(max_1, max_2, in_fit_1_label, in_fit_2_label)
      }
      
      output$powerCurve_plot <- renderPlot(powercurve_plot(max_1, max_2, in_fit_1_label, in_fit_2_label))
      
      dat <- tryCatch({
        get_dygraph_data(fit_1, fit_2, in_fit_1_label, in_fit_2_label)
      },
      error = function(e) {
        showNotification(paste("Error getting timeseries data for plot: \n", e), 
                         type = "error", duration = NULL)
        NULL
      })
      
      app_env$rmd_params$l1 <- in_fit_1_label
      app_env$rmd_params$m1 <- max_1
      app_env$rmd_params$f1 <- f1_meta
      app_env$rmd_params$d1 <- f1_devices
      app_env$rmd_params$d <- dat
      app_env$ready <- TRUE
      
      app_env$rmd_params$l2 <- in_fit_2_label
      app_env$rmd_params$m2 <- max_2
      app_env$rmd_params$f2 <- f2_meta
      app_env$rmd_params$d2 <- f2_devices
      
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
    }
  })
  
  output$show <- reactive({
    app_env$ready
  })
  
  outputOptions(output, 'show', suspendWhenHidden = FALSE)
  
  output$run_report <- downloadHandler(
    filename = "dualReport.html",
    content = function(file) {
      
      params <- list(f1_meta = app_env$rmd_params$f1,
                     f2_meta = app_env$rmd_params$f2,
                     f1_devices = app_env$rmd_params$d1,
                     f2_devices = app_env$rmd_params$d2,
                     in_fit_1_label = app_env$rmd_params$l1,
                     in_fit_2_label = app_env$rmd_params$l2,
                     max_1 = app_env$rmd_params$m1,
                     max_2 = app_env$rmd_params$m2,
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
