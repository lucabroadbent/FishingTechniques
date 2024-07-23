
library(shiny)
library(mizer)
library(ggplot2)
library(dplyr)
library(plotly)

# Define the UI
ui <- fluidPage(
  titlePanel("Mizer Model App"),
  tabsetPanel(
    tabPanel("Decreasing Species",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("species", "Abundance Decreased by:", min = 0, max = 1, value = 1, step = 0.01),
                 sliderInput("year", "Time Range", min = 0, max = 100, value = c(1, 2), step = 1),
                 selectInput("species_name_select", "Select a Species:", 
                             choices = c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Herring", "Sprat", 
                                         "Cod", "Haddock", "Whiting", "Blue whiting", "Norway Pout", "Poor Cod", 
                                         "European Hake", "Monkfish", "Horse Mackerel", "Mackerel", "Common Dab", 
                                         "Plaice", "Megrim", "Sole")),
                 actionButton("set_year_5", "5 Years"),
                 actionButton("set_year_15", "15 Years"),
                 actionButton("set_year_30", "30 Years")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Change in Species", plotOutput("speciesPlot")),
                   tabPanel("Change in Size", plotOutput("sizePlot")),
                   tabPanel("Guilds", plotOutput("guildPlot"))
                 )
               )
             )
    ),
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
    ),
    tabPanel("Mortality Species",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("mortspecies", "Mortality Imposed", min = 0, max = 0.5, value = 0, step = 0.01),
                 selectInput("name_select", "Select a Species:", 
                             choices = c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Herring", "Sprat", 
                                         "Cod", "Haddock", "Whiting", "Blue whiting", "Norway Pout", "Poor Cod", 
                                         "European Hake", "Monkfish", "Horse Mackerel", "Mackerel", "Common Dab", 
                                         "Plaice", "Megrim", "Sole")),
                 sliderInput("mortyear", "Time Range", min = 0, max = 100, value = c(1, 2), step = 1)
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Change in Species", plotOutput("mortspeciesPlot")),
                   tabPanel("Change in Size", plotOutput("mortsizePlot")),
                   tabPanel("Guilds", plotOutput("mortguildPlot"))
                 )
               )
             )
    ),
    tabPanel("Breakpoints",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("breakyear", "Year to Analyse", min = 0, max = 100, value = 1, step = 1),
                 sliderInput("breakrange", "Range of Mortality", min = 0, max = 0.5, value = c(0,0), step = 0.01),
                 numericInput("breaknumber", "Number of Simulations", value=10),
                 selectInput("breakname_select", "Select a Species:", 
                             choices = c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Herring", "Sprat", 
                                         "Cod", "Haddock", "Whiting", "Blue whiting", "Norway Pout", "Poor Cod", 
                                         "European Hake", "Monkfish", "Horse Mackerel", "Mackerel", "Common Dab", 
                                         "Plaice", "Megrim", "Sole"))
                 
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Change in Species", plotOutput("breakspeciesPlot")),
                   tabPanel("bug",textOutput("bug")),
                   tabPanel("Breaks",uiOutput("plots_breaks"))
                   
                 )
                 
                 
               )
             )
    )
  )
)
shinyApp(ui = ui, server = server)
