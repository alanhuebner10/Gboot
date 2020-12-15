#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# library(shiny)
# library(Gboot)

# Define UI for application that draws a histogram
ui <- navbarPage("Gboot",
                 tabPanel("Crossed Simulation",
                          sidebarLayout(
                              sidebarPanel(
                                  numericInput("np",
                                               "np:", value = 100),
                                  numericInput("ni",
                                               "ni:", value = 20),
                                  numericInput("nr",
                                               "nr:", value = 2),
                                  numericInput("sig_p",
                                               "sig_p:", value = 4),
                                  numericInput("sig_i",
                                               "sig_i:", value = 2),
                                  numericInput("sig_r",
                                               "sig_r:", value = 1),
                                  numericInput("sig_pi",
                                               "sig_pi:", value = 8),
                                  numericInput("sig_pr",
                                               "sig_pr:", value = 2),
                                  numericInput("sig_ir",
                                               "sig_ir:", value = 3),
                                  numericInput("sig_pir",
                                               "sig_pir:", value = 12),
                                  numericInput("boots", "Number of bootstraps",
                                               value = 1000),
                                  selectInput("type", "Bootstrap Type:",
                                              c("p" = "p", "i" = "i",
                                                "o" = "o", "pi" = "pi",
                                                "po" = "po", "io" = "io",
                                                "pio" = "pio")),
                                  actionButton("crossedRun", "Run")),
                              mainPanel(
                                  plotOutput("crossedPlot")
                              )
                          )
                 ),
                 tabPanel("Nested Simulation",
                          sidebarLayout(
                              sidebarPanel(
                                  numericInput("np",
                                               "np:", value = 100),
                                  numericInput("ni",
                                               "ni:", value = 20),
                                  numericInput("nr",
                                               "nr:", value = 2),
                                  numericInput("sig_p",
                                               "sig_p:", value = 4),
                                  numericInput("sig_i",
                                               "sig_i:", value = 2),
                                  numericInput("sig_r",
                                               "sig_r:", value = 1),
                                  numericInput("sig_pr",
                                               "sig_pr:", value = 8),
                                  numericInput("sig_i.r",
                                               "sig_i.r:", value = 2),
                                  numericInput("sig_pi.r",
                                               "sig_pi.r:", value = 3),
                                  numericInput("boots", "Number of bootstraps",
                                               value = 1000),
                                  selectInput("type", "Bootstrap Type:",
                                              c("p" = "p",
                                                "o" = "o",
                                                "po" = "po",
                                                "io" = "io",
                                                "pio" = "pio")),
                                  actionButton("nestedRun", "Run")),
                              mainPanel(
                                  plotOutput("nestedPlot")
                              )
                          )
                 ))

# Define server logic required to draw a histogram
server <- function(input, output) {

    crossedData <- eventReactive(input$crossedRun, {
        dat <- sim_dat_2crossed(np = input$np, ni = input$ni, nr = input$nr,
                         sig_p = input$sig_p, sig_i =  input$sig_i,
                         sig_r = input$sig_r, sig_pi = input$sig_pi,
                         sig_pr = sqrt(input$sig_pr),
                         sig_ir = sqrt(input$sig_ir), sig_pir = input$sig_pir)

        inputType <- input$type
        boots <- input$boots
        return(list(dat = dat, inputType = inputType, boots = boots))
    })

    nestedData <- eventReactive(input$nestedRun, {
        dat <- sim_dat_2nested(np = 100, ni = 20, nr = 2,
                        sig_p = 4, sig_r = 1,
                        sig_pr = sqrt(2), sig_i.r = sqrt(7), sig_pi.r = sqrt(208))

        inputType <- input$type
        boots <- input$boots
        return(list(dat = dat, inputType = inputType, boots = boots))
    })

    output$crossedPlot <- renderPlot({
        simulatedBootstrappedCI <- CalcGTheoryCI(Data = crossedData()$dat,
                                                 B = crossedData()$boots,
                                                 type = crossedData()$inputType)

        bootViz(simulatedBootstrappedCI)
    })

    output$nestedPlot <- renderPlot({
        simulatedBootstrappedCI <- CalcGTheoryCINested(Data = nestedData()$dat,
                                                 B = nestedData()$boots,
                                                 type = nestedData()$inputType)

        bootViz(simulatedBootstrappedCI)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
