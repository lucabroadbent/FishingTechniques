
library(shiny)
library(mizer)

# Load the mizer model
celticsim <- readRDS("Celtic_16_untuned.rds")

# Define the UI
ui <- fluidPage(
    titlePanel("Mizer Model App"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("effort", "Fishing Effort", min = 0, max = 1, value = 0.5, step = 0.1),
            fluidRow(
                column(2, actionButton("button1", "1")),
                column(2, actionButton("button2", "3")),
                column(2, actionButton("button3", "5")),
                column(2, actionButton("button4", "10")),
                column(2, actionButton("button5", "20"))
            )
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Yield", plotOutput("yieldPlot")),
                tabPanel("Spectra", plotOutput("spectrumPlot"))
            )
        )
    )
)

# Define the server
server <- function(input, output) {
    # Load the mizer model
    celticsim <- readRDS("Celtic_16_untuned.rds")
    
    # Reactive function to calculate the abundance size spectrum and yield size spectrum based on the fishing effort
    spectra <- reactive({
        effort <- input$effort
        time_range <- as.numeric(switch(input$button, "button1" = 1, "button2" = 3, "button3" = 5, "button4" = 10, "button5" = 20))
        projection <- project(celticsim, effort = effort)
        list(
            spectrum = plotSpectra(projection, effort = effort, time_range = time_range),
            yield = plotYield(projection)
        )
    })
    
    # Render the abundance size spectrum plot
    output$spectrumPlot <- renderPlot({
        plot(spectra()$spectrum)
    })
    
    # Render the yield size spectrum plot
    output$yieldPlot <- renderPlot({
        plot(spectra()$yield)
    })
}

shinyApp(ui = ui, server = server)

# Define the server
server <- function(input, output) {
    # Reactive function to calculate the abundance size spectrum and yield size spectrum based on the fishing effort
    spectra <- reactive({
        effort <- input$effort
        projection <- project(celticsim, effort = effort)
        list(
            spectrum = plotSpectra(projection, effort = effort, time_range = input$year),
            yield = plotYield(projection)
        )
    })
    
    # Render the abundance size spectrum plot
    output$spectrumPlot <- renderPlot({
        plot(spectra()$spectrum)
    })
    
    # Render the yield size spectrum plot
    output$yieldPlot <- renderPlot({
        plot(spectra()$yield)
    })
}

# Run the app
shinyApp(ui = ui, server = server)


