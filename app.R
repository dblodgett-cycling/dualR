library(dygraphs)
library(dplyr)
library(xts)
library(shiny)

reticulate::source_python("python/read_fit.py")
reticulate::source_python("python/fit_meta.py")

source("R/profile.R")
source("R/utils.R")

ui <- fluidPage(
  mainPanel(p(""),
            br(),
            p(""),
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
            actionButton(inputId = "generate", label = "Run Report", width = "200px"),
            fixedRow(column(width = 6,
                            htmlOutput("f1_meta")),
                     column(width = 6,
                            htmlOutput("f2_meta"))),
            fixedRow(column(width = 3,
                            tableOutput("powerCurve_table")),
                     column(width = 9,
                            plotOutput("powerCurve_plot"))),
            dygraphOutput("power_dygraph"),
            dygraphOutput("hr_dygraph"),
            dygraphOutput("cad_dygraph"),
            dygraphOutput("ele_dygraph"))
)

server <- function(input, output, session) {
  observeEvent(input$generate, {

    if(is.null(input$f1)) {

      showNotification("Upload fit file first.")
      return()

    } else {

      if(input$f1_label == "") {
        showNotification("Provide label for first file.")
        return()
      }

      fit_1 <- read_fit_file(input$f1$datapath)

      output$f1_meta <- renderUI(HTML(pretty_fm(get_fit_meta(input$f1$datapath))))
    }

    if(!is.null(input$f2)) {

      if(is.null(input$f2_label)) {
        showNotification("Provide label for secton file.")
        return()
      }

      fit_2 <- read_fit_file(input$f2$datapath)

      output$f2_meta <- renderUI(HTML(pretty_fm(get_fit_meta(input$f2$datapath))))

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
      get_powercurve_table(input, max_1, max_2)
    }

    output$powerCurve_plot <- renderPlot(powercurve_plot(input, max_1, max_2))

    dat <- get_hygraph_data(input, fit_1, fit_2)

    output$power_dygraph <- renderDygraph({
      dygraph(dat$power)
    })

    if("elevation" %in% names(dat)) {
      output$ele_dygraph <- renderDygraph({
        dygraph(dat$elevation)
      })
    }

    if("heartrate" %in% names(dat)) {
      output$hr_dygraph <- renderDygraph({
        dygraph(dat$heartrate)
      })
    }

    if("cadence" %in% names(dat)) {
      output$cad_dygraph <- renderDygraph({
        dygraph(dat$cadence)
      })
    }
  })

}

shinyApp(ui, server)
