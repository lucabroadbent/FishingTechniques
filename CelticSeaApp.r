
library(shiny)
library(mizer)
library(ggplot2)
library(dplyr)
library(plotly)
# Load the mizer model
celticsim <- readRDS("Celtic_16_untuned.rds")

# Define the UI
ui <- fluidPage(
    titlePanel("Mizer Model App"),
    tabsetPanel(
        tabPanel("Species",
            sidebarLayout(
                sidebarPanel(
                    sliderInput("species", "Herring", min = 0, max = 1, value = 1, step = 0.01),
                    sliderInput("year", "Time Range", min = 0, max = 100, value = c(1, 2), step = 1)
                ),
                mainPanel(
                    tabsetPanel(
                    tabPanel("Change in Species",plotOutput("speciesPlot")),
                    tabPanel("Change in Size",plotOutput("sizePlot"))
                ))
            )
        ), # Add closing parenthesis for tabPanel("Species")
        tabPanel("Fishing",
            sidebarLayout(
                sidebarPanel(
                    sliderInput("year", "Time Range", min = 0, max = 100, value = c(1, 2), step = 1),
                    sliderInput("industrial", "Commercial", min = 0, max = 1, value = 0.5, step = 0.1),
                    sliderInput("pelagic", "Pelagic", min = 0, max = 1, value = 0.5, step = 0.1),
                    sliderInput("beam", "Beam", min = 0, max = 1, value = 0.5, step = 0.1),
                    sliderInput("otter", "Otter", min = 0, max = 1, value = 0.5, step = 0.1)
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel("Yield", plotOutput("yieldPlot")),
                        tabPanel("Spectra", plotOutput("spectrumPlot"))
                    )
                )
            )
        )
    ) # Add closing brace for ui <- fluidPage()
)



# Define the server
server <- function(input, output) {
    # Reactive function to calculate the abundance size spectrum and yield size spectrum based on the fishing effort
    spectra <- reactive({
        effort <- effort <- c(commercial = input$industrial, pelagic = input$pelagic, beam = input$beam, otter = input$otter)
        projection <- project(celticsim, effort = effort)
        list(
            spectrum = plotSpectra(projection, time_range = input$year),
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


    #this is for the initial abundance of all species 

    specieschange <- reactive({
        speciessim<-celticsim
        unharvestedprojection <- project(celticsim, effort = c(commercial = 0, pelagic = 0, beam = 0, otter = 0), t_max = input$year[2])
        unharvested <- plotSpectra(unharvestedprojection, time_range = input$year[1]:input$year[2], return_data = TRUE)
        speciessim@initial_n["Herring", ]<-speciessim@initial_n["Herring", ]*input$species
        harvestedprojection <- project(speciessim, effort = c(commercial = 0, pelagic = 0, beam = 0, otter = 0), t_max = input$year[2])
        harvested<-plotSpectra(harvestedprojection, time_range = input$year[1]:input$year[2], return_data=TRUE)


#this next function separates the size spectrum into bins so that the effect of changing one species is observed on a community level
        create_log_bins <- function(data, column, bins = 10) {
  # Calculate logarithmically spaced breaks
  breaks <- exp(seq(log(min(data[[column]])), log(max(data[[column]])), length.out = bins + 1))
  
  # Bin the data
  data <- data %>%
    mutate(log_bin = cut(data[[column]], breaks = breaks, include.lowest = TRUE))
  
  # Calculate the average weight for each bin
  bin_means <- data %>%
    group_by(log_bin) %>%
    summarise(avg_weight = mean(!!sym(column)))
  
  # Map the average weights back to the original data
  data <- data %>%
    left_join(bin_means, by = "log_bin")
  
  # Return the data with the new column
  return(data)
}
#run the function on the harvested and unharvested data
binnedharvested<-create_log_bins(harvested, "w", bins = 10)
binnedunharvested<-create_log_bins(unharvested, "w", bins = 10)
#average the values in each bin
binnedharvested<-binnedharvested %>%
group_by(avg_weight) %>%
summarise(value = mean(value))
binnedunharvested<-binnedunharvested %>%
group_by(avg_weight) %>%
summarise(value = mean(value))
#calculate the percentage change in each bin
percentage_diffbinned <- binnedharvested %>%
left_join(binnedunharvested, by = "avg_weight") %>%
mutate(percentage_diff = (value.x / value.y) * 100) %>%
select(avg_weight, percentage_diff)
percentage_diffbinned$percentage_diff <- percentage_diffbinned$percentage_diff - 100
#plot the percentage change in each bin

sizelevel=ggplot(percentage_diffbinned, aes(x = factor(avg_weight), y = percentage_diff, fill = factor(avg_weight))) +
  geom_bar(stat = "identity") +
  labs(title = "Average Percentage Change by Size", x = "Average Weight", y = "Percentage Change") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

        # Group by Species and calculate the average value of the value column for each species
        harvested <- harvested %>%
            group_by(Species) %>%
            summarise(avg_value = mean(value))
        unharvested <- unharvested %>%
            group_by(Species) %>%
            summarise(avg_value = mean(value))
        percentage_diff <- harvested %>% 
            left_join(unharvested, by = "Species") %>%
            mutate(percentage_diff = (avg_value.x / avg_value.y) * 100) %>%
            select(Species, percentage_diff)
        percentage_diff$percentage_diff <- percentage_diff$percentage_diff - 100
        specieslevel=ggplot(percentage_diff, aes(x = Species, y = percentage_diff, fill = Species)) +
                    geom_bar(stat = "identity") +
                    labs(title = "Average Percentage Change by Species", x = "Species", y = "Percentage Change") +
                    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
         list(sizelevel=ggplot(percentage_diffbinned, aes(x = factor(avg_weight), y = percentage_diff, fill = factor(avg_weight))) +
  geom_bar(stat = "identity") +
  labs(title = "Average Percentage Change by Size", x = "Average Weight", y = "Percentage Change") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)),
         specieslevel=ggplot(percentage_diff, aes(x = Species, y = percentage_diff, fill = Species)) +
                    geom_bar(stat = "identity") +
                    labs(title = "Average Percentage Change by Species", x = "Species", y = "Percentage Change") +
                    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)))
    })

output$speciesPlot <- renderPlot({
    specieschange()$specieslevel
})
output$sizePlot <- renderPlot({
    specieschange()$sizelevel
})

} 

# Run the app
shinyApp(ui = ui, server = server)


