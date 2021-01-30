library(dygraphs)
library(dplyr)
library(xts)
library(shiny)

PYTHON_DEPENDENCIES = c('sweat')

source("R/profile.R")
source("R/utils.R")

app_env <- reactiveValues(rmd_params = list())

ui <- fluidPage(
  mainPanel(h1("Dual Power Comparison"),
            br(),
            p("This application accepts one or two (overlapping) fit files and creates a comparison report."),
            fixedRow(column(width = 6,
                            fileInput("f1", "Fit 1", multiple = FALSE, accept = ".fit"),
                            textInput(inputId = "f1_label", label = "File 1 Label",
                                      width = "200px",
                                      placeholder = "e.g. Power Meter")),
                     column(width = 6,
                            fileInput("f2", "Fit 2", multiple = FALSE, accept = ".fit"),
                            textInput(inputId = "f2_label", label = "File 2 Label",
                                      width = "200px",
                                      placeholder = "e.g. Trainer"))),
            fixedRow(column(width = 6,
                            actionButton(inputId = "generate", label = "Run Report", width = "200px")),
                     column(width = 6,
                            shiny::checkboxInput("demo", "Demo: Check to use demo files."))),
            br(),
            h3("Power Source Metadata"),
            fixedRow(column(width = 6,
                            htmlOutput("f1_meta")),
                     column(width = 6,
                            htmlOutput("f2_meta"))),
            br(),
            shiny::h3("Critical Power Comparison"),
            fixedRow(column(width = 3,
                            br(),
                            br(),
                            tableOutput("powerCurve_table")),
                     column(width = 9,
                            plotOutput("powerCurve_plot"))),
            br(),
            downloadButton("run_report", label = "Download Report"),
            dygraphOutput("power_dygraph"),
            dygraphOutput("hr_dygraph"),
            dygraphOutput("cad_dygraph"),
            dygraphOutput("ele_dygraph"))
)

server <- function(input, output, session) {

  # ------------------ App virtualenv setup (Do not edit) ------------------- #

  virtualenv_dir = Sys.getenv('VIRTUALENV_NAME')
  python_path = Sys.getenv('PYTHON_PATH')

  reticulate::virtualenv_create(envname = virtualenv_dir, python = python_path)
  reticulate::virtualenv_install(virtualenv_dir, packages = c(PYTHON_DEPENDENCIES),
                                 ignore_installed=FALSE)
  reticulate::use_virtualenv(virtualenv_dir, required = T)

  reticulate::source_python("python/read_fit.py")
  reticulate::source_python("python/fit_meta.py")

  observeEvent(input$generate, {

    if(input$demo) {
      in_fit_1 <- "fit/fit1.fit"
      in_fit_1_label <- "Demo Source 1"
      in_fit_2 <- "fit/fit2.fit"
      in_fit_2_label <- "Demo Source 2"
    } else {

      if(is.null(input$f1)) {

        showNotification("Upload fit file first.")
        return()

      } else {

        if(input$f1_label == "") {
          showNotification("Provide label for first file.")
          return()
        }

        in_fit_1 <- input$f1$datapath

        in_fit_1_label <- input$f1_label

      }

      if(!is.null(input$f2)) {

        if(is.null(input$f2_label)) {
          showNotification("Provide label for secton file.")
          return()
        }

        in_fit_2 <- input$f2$datapath

        in_fit_2_label <- input$f2_label
      }

    }

    fit_1 <- read_fit_file(in_fit_1)

    f1_meta <- get_fit_meta(in_fit_1)

    output$f1_meta <- renderUI(HTML(pretty_fm(f1_meta)))

    if(exists("in_fit_2")) {

      fit_2 <- read_fit_file(in_fit_2)

      f2_meta <- get_fit_meta(in_fit_2)
      output$f2_meta <- renderUI(HTML(pretty_fm(f2_meta)))

    } else {

      fit_2 <- NULL

    }

    if(!is.null(fit_2)) {

      max_1 <- get_maxes(fit_1, fit_2)
      max_2 <- get_maxes(fit_2, fit_1)

    } else {

      max_1 <- get_maxes(fit_1)
      max_2 <- NULL

    }

    output$powerCurve_table <- function() {
      get_powercurve_table(max_1, max_2, in_fit_1_label, in_fit_2_label)
    }

    output$powerCurve_plot <- renderPlot(powercurve_plot(max_1, max_2, in_fit_1_label, in_fit_2_label))

    dat <- get_hygraph_data(fit_1, fit_2, in_fit_1_label, in_fit_2_label)

    app_env$rmd_params$l1 <- in_fit_1_label
    app_env$rmd_params$l2 <- in_fit_2_label
    app_env$rmd_params$m1 <- max_1
    app_env$rmd_params$m2 <- max_2
    app_env$rmd_params$f1 <- pretty_fm(f1_meta)
    app_env$rmd_params$f2 <- pretty_fm(f2_meta)
    app_env$rmd_params$d <- dat

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
  })

  output$run_report <- downloadHandler(
    filename = "dualReport.html",
    content = function(file) {

      params <- list(f1_meta = app_env$rmd_params$f2,
                     f2_meta= app_env$rmd_params$f2,
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

shinyApp(ui, server)
