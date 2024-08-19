library(shiny)
library(mizer)
library(ggplot2)
library(dplyr)
library(bslib)
library(plotly)
library(shinyuieditor)
library(ggplot2)
library(gridlayout)
library(thematic)
# Load the mizer model

setwd("C:/Users/lucab/Downloads")

celticsim <- readRDS("Celtic_16_untuned.rds")

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
                                choices = c("Herring", "Sprat", 
                                            "Cod", "Haddock", "Whiting", "Blue whiting", "Norway Pout", "Poor Cod", 
                                            "European Hake", "Monkfish", "Horse Mackerel", "Mackerel", "Common Dab", 
                                            "Plaice", "Megrim", "Sole")),
                    actionButton("set_year_5", "5 Years"),
                    actionButton("set_year_15", "15 Years"),
                    actionButton("set_year_30", "30 Years"),
                    actionButton("goButton1", "Run Simulation")
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
                    sliderInput("otter", "Otter", min = 0, max = 1, value = 0.5, step = 0.1),
                    actionButton("goButton2", "Run Simulation")
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
                                choices = c("Herring", "Sprat", 
                                            "Cod", "Haddock", "Whiting", "Blue whiting", "Norway Pout", "Poor Cod", 
                                            "European Hake", "Monkfish", "Horse Mackerel", "Mackerel", "Common Dab", 
                                            "Plaice", "Megrim", "Sole")),
                    sliderInput("mortyear", "Time Range", min = 0, max = 100, value = c(1, 2), step = 1),
                    actionButton("goButton3", "Run Simulation")
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
                                     choices = c("Herring", "Sprat", 
                                                 "Cod", "Haddock", "Whiting", "Blue whiting", "Norway Pout", "Poor Cod", 
                                                 "European Hake", "Monkfish", "Horse Mackerel", "Mackerel", "Common Dab", 
                                                 "Plaice", "Megrim", "Sole")),
                         actionButton("goButton", "Run Simulation")
                        
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


# Define the server
  server <- function(input, output, session) {

    thematic::thematic_shiny()
    
  #plots the spectra of the fishery model
   spectra <- eventReactive(input$goButton2,{
        effort <- c(commercial = input$industrial, pelagic = input$pelagic, beam = input$beam, otter = input$otter)
        projection <- project(celticsim, effort = effort)
        list(
            spectrum = plotSpectra(projection, time_range = input$year),
            yield = plotYield(projection)
        )
    })
  #this is just plotting the outputs of spectra
    output$spectrumPlot <- renderPlot({
        spectra()$spectrum
    })
    output$yieldPlot <- renderPlot({
       spectra()$yield
    })

    
    #here is the buttons for the years
    observeEvent(input$set_year_5, {
        updateSliderInput(session, "year", value = c(5, 5))
    })
    observeEvent(input$set_year_15, {
        updateSliderInput(session, "year", value = c(15, 15))
    })
    observeEvent(input$set_year_30, {
        updateSliderInput(session, "year", value = c(30, 30))
    })
    
    #Decrease Herring Abundance 
    specieschange <- eventReactive(input$goButton1,{
        speciessim <- celticsim
        unharvestedprojection <- project(celticsim,
                                         effort = c(commercial = 0, pelagic = 1, beam = 1, otter = 1),
                                         t_max = input$year[2])
        unharvested <- plotSpectra(unharvestedprojection, time_range = input$year[1]:input$year[2], return_data = TRUE)
        speciessim@initial_n[input$species_name_select, ] <- speciessim@initial_n[input$species_name_select, ] * input$species
        harvestedprojection <- project(speciessim,
                                       effort = c(commercial = 0, pelagic = 1, beam = 1, otter = 1),
                                       t_max = input$year[2])
        harvested <- plotSpectra(harvestedprojection, time_range = input$year[1]:input$year[2], return_data = TRUE)

        harvested2 <- harvested
        unharvested2 <- unharvested

        # This next function separates the size spectrum into bins
        #so that the effect of changing one species is observed on a community level
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
        # Run the function on the harvested and unharvested data
        binnedharvested <- create_log_bins(harvested, "w", bins = 10)
        binnedunharvested <- create_log_bins(unharvested, "w", bins = 10)
        # Average the values in each bin
        binnedharvested <- binnedharvested %>%
            group_by(avg_weight) %>%
            summarise(value = mean(value))
        binnedunharvested <- binnedunharvested %>%
            group_by(avg_weight) %>%
            summarise(value = mean(value))
        # Calculate the percentage change in each bin
        percentage_diffbinned <- binnedharvested %>%
            left_join(binnedunharvested, by = "avg_weight") %>%
            mutate(percentage_diff = (value.x / value.y) * 100) %>%
            select(avg_weight, percentage_diff)
        
        percentage_diffbinned$percentage_diff <- percentage_diffbinned$percentage_diff - 100
        # Plot the percentage change in each bin

        sizelevel <- ggplot(percentage_diffbinned, aes(x = factor(avg_weight, labels = paste(1:11)),
                                                       y = percentage_diff)) +
            geom_bar(stat = "identity") +
            labs(title = "Average Percentage Change by Size", x = "Size: Smaller to Larger", y = "Percentage Change") +
          theme_minimal() +
          theme(axis.text.x = element_text(size = 14, hjust = 1, vjust = 0.5),
                axis.text.y = element_text(size = 14),
                legend.position = "none",
                axis.title.x = element_text(size = 16, face = "bold"),
                axis.title.y = element_text(size = 16, face = "bold"))

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
            select(Species, percentage_diff)%>%
          filter(!Species %in% c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"))
        
        percentage_diff$percentage_diff <- percentage_diff$percentage_diff - 100
        specieslevel <- ggplot(percentage_diff, aes(x = Species, y = percentage_diff)) +
            geom_bar(stat = "identity") +
            labs(title = "Average Percentage Change by Species", x = "Species", y = "Percentage Change") +
          theme_minimal() +
          theme(axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = 0.5),
                axis.text.y = element_text(size = 14),
                legend.position = "none",
                axis.title.x = element_text(size = 16, face = "bold"),
                axis.title.y = element_text(size = 16, face = "bold"))

        # Firstly, I need to extract the size spectrum for each guild
        plank <- harvested2 %>%
            filter(Species %in% c("Herring", "Sprat", "Blue whiting", "Norway pout", "Mackerel")) %>%
  mutate(log_w = log10(w),
         size_category = cut(log_w,
                             breaks = quantile(log_w, probs = seq(0, 1, by = 1 / 3), na.rm = TRUE),
                             labels = c("small", "medium", "large"),
                             include.lowest = TRUE)) %>%
    group_by(size_category) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), Guild = "Planktivorous")

benth <- harvested2 %>%
    filter(Species %in% c("Poor Cod", "Common Dab", "Plaice", "Sole")) %>%
    mutate(log_w = log10(w),
         size_category = cut(log_w,
                             breaks = quantile(log_w, probs = seq(0, 1, by = 1 / 3), na.rm = TRUE),
                             labels = c("small", "medium", "large"),
                             include.lowest = TRUE)) %>%
    group_by(size_category) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), Guild = "Benthic")

pisco <- harvested2 %>%
    filter(Species %in%
               c("Cod", "Haddock", "Whiting", "European Hake",
             "Monkfish", "Horse Mackerel", "Megrim")) %>%
    mutate(log_w = log10(w),
            size_category = cut(log_w,
                                breaks = quantile(log_w, probs = seq(0, 1, by = 1 / 3), na.rm = TRUE),
                                labels = c("small", "medium", "large"),
                                include.lowest = TRUE)) %>%
    group_by(size_category) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), Guild = "Piscovorous")

        # Combine
        guilds <- rbind(plank, benth, pisco)
        # Calculate the percentage change in each guild
        unharvestedplank <- unharvested2%>%
  filter(Species %in% c("Herring", "Sprat", "Blue whiting", "Norway pout", "Mackerel")) %>%
  mutate(log_w = log10(w),
         size_category = cut(log_w,
                             breaks = quantile(log_w, probs = seq(0, 1, by = 1 / 3), na.rm = TRUE),
                             labels = c("small", "medium", "large"),
                             include.lowest = TRUE)) %>%
    group_by(size_category) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), Guild = "Planktivorous")

       unharvestedbenth <- unharvested2 %>%
    filter(Species %in% c("Poor Cod", "Common Dab", "Plaice", "Sole")) %>%
    mutate(log_w = log10(w),
         size_category = cut(log_w,
                             breaks = quantile(log_w, probs = seq(0, 1, by = 1 / 3), na.rm = TRUE),
                             labels = c("small", "medium", "large"),
                             include.lowest = TRUE)) %>%
    group_by(size_category) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), Guild = "Benthic")

    unharvestedpisco <- unharvested2 %>%
    filter(Species %in%
               c("Cod", "Haddock", "Whiting", "European Hake",
             "Monkfish", "Horse Mackerel", "Megrim")) %>%
    mutate(log_w = log10(w),
            size_category = cut(log_w,
                                breaks = quantile(log_w, probs = seq(0, 1, by = 1 / 3), na.rm = TRUE),
                                labels = c("small", "medium", "large"),
                                include.lowest = TRUE)) %>%
    group_by(size_category) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), Guild = "Piscovorous") 

        # Combine
        unguilds <- rbind(unharvestedplank, unharvestedbenth, unharvestedpisco)

        merged_df <- guilds %>%
            inner_join(unguilds, by = c("size_category", "Guild"), suffix = c("_guilds", "_unguilds"))

        percentage_diffguilds <- merged_df %>%
            mutate(percentage_diff = ((mean_value_guilds - mean_value_unguilds) / mean_value_unguilds) * 100)

        # Plot the percentage change in each guild
        guildlevel <- ggplot(percentage_diffguilds, aes(x = Guild, y = percentage_diff, fill = size_category)) + # nolint
            geom_bar(stat = "identity", position = "dodge") +
            labs(title = "Average Percentage Change by Guild", x = "Size Category", y = "Percentage Change") +
          theme_minimal() +
          theme(axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust = 0.5),
                axis.text.y = element_text(size = 14),
                legend.position = "none",
                axis.title.x = element_text(size = 16, face = "bold"),
                axis.title.y = element_text(size = 16, face = "bold"))
        

        list(sizelevel = sizelevel, specieslevel = specieslevel, guildlevel = guildlevel)
    })
    #Plots of herring decrease
    output$speciesPlot <- renderPlot({
        specieschange()$specieslevel
    })
    output$sizePlot <- renderPlot({
        specieschange()$sizelevel
    })
    output$guildPlot <- renderPlot({
        specieschange()$guildlevel
    })
    
    #Plots of herring mortality decrease.
    mortspecieschange <- eventReactive(input$goButton3,{
      speciessim <- celticsim
      unharvestedprojection <- project(celticsim,
                                       effort = c(commercial = 0, pelagic = 1, beam = 1, otter = 1),
                                       t_max = input$mortyear[2])
      unharvested <- plotSpectra(unharvestedprojection, time_range = input$mortyear[1]:input$mortyear[2], return_data = TRUE)
      #trying to set external mortality rate, here is how you call it
      
      test <- getExtMort(speciessim)
      totalmort <- getMort(speciessim)
      
      test[input$name_select,] <- test[input$name_select,]+(input$mortspecies*totalmort[input$name_select,])
      
      ext_mort(speciessim) <- test
      
      harvestedprojection <- project(speciessim,
                                     effort = c(commercial = 0, pelagic = 1, beam = 1, otter = 1),
                                     t_max = input$mortyear[2])
      harvested <- plotSpectra(harvestedprojection, time_range = input$mortyear[1]:input$mortyear[2], return_data = TRUE)
      
      harvested2 <- harvested
      unharvested2 <- unharvested
      
      # This next function separates the size spectrum into bins
      #so that the effect of changing one species is observed on a community level
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
      # Run the function on the harvested and unharvested data
      binnedharvested <- create_log_bins(harvested, "w", bins = 10)
      binnedunharvested <- create_log_bins(unharvested, "w", bins = 10)
      # Average the values in each bin
      binnedharvested <- binnedharvested %>%
        group_by(avg_weight) %>%
        summarise(value = mean(value))
      binnedunharvested <- binnedunharvested %>%
        group_by(avg_weight) %>%
        summarise(value = mean(value))
      # Calculate the percentage change in each bin
      percentage_diffbinned <- binnedharvested %>%
        left_join(binnedunharvested, by = "avg_weight") %>%
        mutate(percentage_diff = (value.x / value.y) * 100) %>%
        select(avg_weight, percentage_diff)
      
      percentage_diffbinned$percentage_diff <- percentage_diffbinned$percentage_diff - 100
      # Plot the percentage change in each bin
      
      sizelevel <- ggplot(percentage_diffbinned, aes(x = factor(avg_weight, labels = paste(1:10)),
                                                     y = percentage_diff)) +
        geom_bar(stat = "identity") +
        labs(title = "Average Percentage Change by Size", x = "Size: Smaller to Larger", y = "Percentage Change") +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 14, hjust = 1, vjust = 0.5),
              axis.text.y = element_text(size = 14),
              legend.position = "none",
              axis.title.x = element_text(size = 16, face = "bold"),
              axis.title.y = element_text(size = 16, face = "bold"))
      
      
      # Group by Species and calculate the average value of the value column for each species
      harvested <- harvested %>%
        group_by(Species) %>%
        summarise(avg_value = mean(value))
      
      unharvested <- unharvested %>%
        group_by(Species) %>%
        summarise(avg_value = mean(value))
      
      percentage_diff <- harvested %>%
        inner_join(unharvested, by = "Species") %>%
        mutate(percentage_diff = ((avg_value.x / avg_value.y) * 100)-100) %>%
        select(Species, percentage_diff)%>%
        filter(!Species %in% c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"))
      
      specieslevel <- ggplot(percentage_diff, aes(x = Species, y = percentage_diff, fill = Species)) +
        geom_bar(stat = "identity") +
        labs(title = "Average Percentage Change by Species", x = "Species", y = "Percentage Change") +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = 0.5),
              axis.text.y = element_text(size = 14),
              legend.position = "none",
              axis.title.x = element_text(size = 16, face = "bold"),
              axis.title.y = element_text(size = 16, face = "bold"))
      
      
      # This next section will be for the guilds, and the percentage change in each guild
      
      # Firstly, I need to extract the size spectrum for each guild
      plank <- harvested2 %>%
        filter(Species %in% c("Herring", "Sprat", "Blue whiting", "Norway pout", "Mackerel")) %>%
        mutate(log_w = log10(w),
               size_category = cut(log_w,
                                   breaks = quantile(log_w, probs = seq(0, 1, by = 1 / 3), na.rm = TRUE),
                                   labels = c("small", "medium", "large"),
                                   include.lowest = TRUE)) %>%
        group_by(size_category) %>%
        summarise(mean_value = mean(value, na.rm = TRUE), Guild = "Planktivorous")
      
      benth <- harvested2 %>%
        filter(Species %in% c("Poor Cod", "Common Dab", "Plaice", "Sole")) %>%
        mutate(log_w = log10(w),
               size_category = cut(log_w,
                                   breaks = quantile(log_w, probs = seq(0, 1, by = 1 / 3), na.rm = TRUE),
                                   labels = c("small", "medium", "large"),
                                   include.lowest = TRUE)) %>%
        group_by(size_category) %>%
        summarise(mean_value = mean(value, na.rm = TRUE), Guild = "Benthic")
      
      pisco <- harvested2 %>%
        filter(Species %in%
                 c("Cod", "Haddock", "Whiting", "European Hake",
                   "Monkfish", "Horse Mackerel", "Megrim")) %>%
        mutate(log_w = log10(w),
               size_category = cut(log_w,
                                   breaks = quantile(log_w, probs = seq(0, 1, by = 1 / 3), na.rm = TRUE),
                                   labels = c("small", "medium", "large"),
                                   include.lowest = TRUE)) %>%
        group_by(size_category) %>%
        summarise(mean_value = mean(value, na.rm = TRUE), Guild = "Piscovorous")
      
      # Combine
      guilds <- rbind(plank, benth, pisco)
      # Calculate the percentage change in each guild
      unharvestedplank <- unharvested2%>%
        filter(Species %in% c("Herring", "Sprat", "Blue whiting", "Norway pout", "Mackerel")) %>%
        mutate(log_w = log10(w),
               size_category = cut(log_w,
                                   breaks = quantile(log_w, probs = seq(0, 1, by = 1 / 3), na.rm = TRUE),
                                   labels = c("small", "medium", "large"),
                                   include.lowest = TRUE)) %>%
        group_by(size_category) %>%
        summarise(mean_value = mean(value, na.rm = TRUE), Guild = "Planktivorous")
      
      unharvestedbenth <- unharvested2 %>%
        filter(Species %in% c("Poor Cod", "Common Dab", "Plaice", "Sole")) %>%
        mutate(log_w = log10(w),
               size_category = cut(log_w,
                                   breaks = quantile(log_w, probs = seq(0, 1, by = 1 / 3), na.rm = TRUE),
                                   labels = c("small", "medium", "large"),
                                   include.lowest = TRUE)) %>%
        group_by(size_category) %>%
        summarise(mean_value = mean(value, na.rm = TRUE), Guild = "Benthic")
      
      unharvestedpisco <- unharvested2 %>%
        filter(Species %in%
                 c("Cod", "Haddock", "Whiting", "European Hake",
                   "Monkfish", "Horse Mackerel", "Megrim")) %>%
        mutate(log_w = log10(w),
               size_category = cut(log_w,
                                   breaks = quantile(log_w, probs = seq(0, 1, by = 1 / 3), na.rm = TRUE),
                                   labels = c("small", "medium", "large"),
                                   include.lowest = TRUE)) %>%
        group_by(size_category) %>%
        summarise(mean_value = mean(value, na.rm = TRUE), Guild = "Piscovorous") 
      
      # Combine
      unguilds <- rbind(unharvestedplank, unharvestedbenth, unharvestedpisco)
      
      merged_df <- guilds %>%
        inner_join(unguilds, by = c("size_category", "Guild"), suffix = c("_guilds", "_unguilds"))
      
      percentage_diffguilds <- merged_df %>%
        mutate(percentage_diff = ((mean_value_guilds - mean_value_unguilds) / mean_value_unguilds) * 100)
      
      # this has been changed from guildlevel <- 
      guildlevel <- ggplot(percentage_diffguilds, aes(x = Guild, y = percentage_diff)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Percentage Change by Guild", x = "Size Category", y = "Percentage Change")+
        theme_minimal() +
        theme(axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust = 0.5),
              axis.text.y = element_text(size = 14),
              legend.position = "none",
              axis.title.x = element_text(size = 16, face = "bold"),
              axis.title.y = element_text(size = 16, face = "bold"))
      
      
      list(sizelevel = sizelevel, specieslevel = specieslevel, guildlevel = guildlevel)
    })
    #Plotting the outputs of the mortality decreases
    output$mortspeciesPlot <- renderPlot({
      mortspecieschange()$specieslevel
    })
    output$mortsizePlot <- renderPlot({
      mortspecieschange()$sizelevel
    })
    output$mortguildPlot <- renderPlot({
      mortspecieschange()$guildlevel
    })
    
    #Plotting breakpoints for the mortality increases
    breaks <- eventReactive(input$goButton,{
      req(input$breakrange, input$breaknumber, input$breakyear)
      
      # Generate breakpoints
      breaks <- seq(input$breakrange[1], input$breakrange[2], 
                     by = (input$breakrange[2] - input$breakrange[1]) / (input$breaknumber - 1))
      
      # Create a data frame with time range and mortality breakpoints
      databreak <- data.frame(mort = breaks, time = rep(input$breakyear, length(breaks)))
      
      return(databreak)
    })

    #Getting simulations from these mortality rates.
    breaksim <- eventReactive(input$goButton,{
      
      #read in the breaks data and make a empty dataframe
      breakpoints <- breaks()
      breaksim <- data.frame()
      
      #read in species and get the mortality rates
      speciessim <- celticsim
      
      for (i in 1:nrow(breakpoints)) {
        
        speciessim <- celticsim
        test <- getExtMort(speciessim)
        totalmort <- getMort(speciessim)
        test[input$breakname_select, ] <- test[input$breakname_select, ] + (breakpoints$mort[i] * totalmort[input$breakname_select, ])
        
        ext_mort(speciessim) <- test
        
        harvestedprojection <- project(speciessim,
                                       effort = c(commercial = 0, pelagic = 1, beam = 1, otter = 1),
                                       t_max = breakpoints$time[i])
        
        harvested <- plotSpectra(harvestedprojection, time_range = breakpoints$time[i]:breakpoints$time[i], return_data = TRUE)
        
        harvested$sim <- i
        breaksim <- rbind(breaksim, harvested)
      }
      
      return(breaksim)
      
    })
    
    #I DONT KNOW WHAT DO DO HERE - WHICH TO COMPARE? IS IT STEADY STATE?
    #DOES IT MATTER IF WE JUST USE A RANDOM UNHARVESTED SIM
    #normalising to the unharvested
    breaknorm <- eventReactive(input$goButton,{
      
      sims <- breaksim()
      breakpoints <- breaks()
      #join the unharvested data to make it easier.
      
      unharvestedprojection <- project(celticsim,
                                       effort = c(commercial = 0, pelagic = 1, beam = 1, otter = 1),
                                       t_max = breakpoints$time[1])
      unharvested <- plotSpectra(unharvestedprojection, time_range = breakpoints$time[1]:breakpoints$time[1], return_data = TRUE)
      
      unharvested <- unharvested %>%
        group_by(Species)%>%
        summarise(mean_value = mean(value, na.rm = TRUE))
      
      sims <- sims %>%
        group_by(Species, sim)%>%
        summarise(mean_value = mean(value, na.rm = TRUE))
      
      normalized_data <- sims %>%
        inner_join(unharvested, by = "Species")%>%
        mutate(normalized_value = ((mean_value.x / mean_value.y)-1)*100) %>%
        select(Species, normalized_value, sim) %>%
        filter(!Species %in% c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"))
      
      return(normalized_data)
    })
    
    #now plotting this data
    #first need to create a function for it.
    create_species_level_plot <- function(data, plot_title) {
      ggplot(data, aes(x = Species, y = normalized_value, fill = Species)) +
        geom_bar(stat = "identity") +
        labs(title = plot_title, x = "Species", y = "Percentage Change") +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust = 0.5),
              axis.text.y = element_text(size = 14),
              legend.position = "none",
              axis.title.x = element_text(size = 16, face = "bold"),
              axis.title.y = element_text(size = 16, face = "bold"))
      
    }
    
    #now a function to split the dataframe into a list
    data_list <- reactive({
      split(breaknorm(), breaknorm()$sim)
    })

    
    #now plotting these plots.
    observe({
      breakpoint <- breaks()
      num_plots <- input$breaknumber
      lapply(1:num_plots, function(i) {
        local({
          my_i <- i
          plotname <- paste("plot", my_i, sep = "")
          output[[plotname]] <- renderPlot({
            current_data <- data_list()[[my_i]]
            mort_value <- breakpoint$mort[my_i] * 100
            formatted_mort_value <- sprintf("+%.2f%%", mort_value)
            plot_title <- paste("Mortality", formatted_mort_value) 
            create_species_level_plot(current_data, plot_title)
          })
        })
      })
    })
  
    #now creating the UI for the plots
    output$plots_breaks <- renderUI({
      num_plots <- input$breaknumber
      plot_output_list <- lapply(1:num_plots, function(i) {
        plotname <- paste("plot", i, sep = "")
        plotOutput(plotname, height = "300px")
      })
      do.call(tagList, plot_output_list)
    })

    bs_themer()

    
}

shinyApp(ui = ui, server = server)




ui <- page_navbar(
  title = "Celtic Sea Mizer Model",
  selected = "Species",
  collapsible = TRUE,
  tags$style(HTML("
    .btn-small{
      padding: 5px 10px;
      font-size: 12px;
      border-radius: 4px;
    }
  ")),
  theme = bs_theme(bootswatch="cerulean"),
  tags$style(HTML("
    .nav-tabs .nav-link.active, .nav-tabs .nav-item.show .nav-link {
      color: #ffffff;
      background-color: #007bff;
      border-color: #007bff #007bff #007bff;
    }
    .nav-tabs .nav-link {
      color: #007bff;
      border: 1px solid transparent;
      border-top-left-radius: .25rem;
      border-top-right-radius: .25rem;
    }
    .nav-tabs .nav-link:hover {
      border-color: #e9ecef #e9ecef #ddd;
      color: #0056b3;
    }
    .card {
      margin-top: 100px; /* Increased margin to make it more noticeable */
      border: 2px solid #007bff; /* Added border to highlight the card */
      border-radius: .5rem; /* Increased border-radius for more visible rounding */
    }
.nav-tabs {
  margin-bottom: 30px; /* Adjust this value as needed */
}

/* Alternatively, add padding to the top of the plots container */
.plots-container {
  padding-top: 30px; /* Adjust this value as needed */
}

  ")),
  
  # Species Tab with Biomass and Mortality Subtabs
  tabPanel(
    title = "Species",
    tabsetPanel(
      selected = "Biomass",
      
      # Biomass Tab
      tabPanel(
        title = "Biomass",
        grid_container(
          layout = c(
            "area1 area0"
          ),
          row_sizes = c(
            "1fr"
          ),
          col_sizes = c(
            "0.3fr",
            "1.7fr"
          ),
          gap_size = "10px",
          
          # Sidebar for Biomass
          grid_card(
            area = "area1",
            card_body(
              sliderInput(
                inputId = "species",
                label = "Abundance Decreased by:",
                min = 0,
                max = 1,
                value = 1,
                step = 0.01,
                width = "100%"
              ),
              sliderInput(
                inputId = "year",
                label = "Time Range",
                min = 0,
                max = 100,
                value = c(1, 2),
                step = 1,
                width = "100%"
              ),
              selectInput(
                inputId = "species_name_select",
                label = "Select a Species:",
                choices = c("Herring", "Sprat", 
                            "Cod", "Haddock", "Whiting", "Blue whiting", "Norway Pout", "Poor Cod", 
                            "European Hake", "Monkfish", "Horse Mackerel", "Mackerel", "Common Dab", 
                            "Plaice", "Megrim", "Sole")
              ),
              actionButton(inputId = "set_year_5", label = "5 Years", class = "btn-small"),
              actionButton(inputId = "set_year_15", label = "15 Years", class = "btn-small"),
              actionButton(inputId = "set_year_30", label = "30 Years", class = "btn-small"),
              actionButton(inputId = "goButton1", label = "Run Simulation")
            )
          ),
          
          # Main Panel for Biomass
          grid_card(
            area = "area0",
            card_body(
              tabsetPanel(
                tabPanel(title = "Change in Species", plotOutput("speciesPlot")),
                tabPanel(title = "Change in Size", plotOutput("sizePlot")),
                tabPanel(title = "Guilds", plotOutput("guildPlot"))
              )
            )
          )
        )
      ),
      
      # Mortality Tab
      tabPanel(
        title = "Mortality",
        grid_container(
          layout = c(
            "area1 area0"
          ),
          row_sizes = c(
            "1fr"
          ),
          col_sizes = c(
            "0.3fr",
            "1.7fr"
          ),
          gap_size = "10px",
          
          # Sidebar for Mortality
          grid_card(
            area = "area1",
            card_body(
              sliderInput(
                inputId = "mortspecies",
                label = "Mortality Imposed",
                min = 0,
                max = 0.5,
                value = 0,
                step = 0.01,
                width = "100%"
              ),
              selectInput(
                inputId = "name_select",
                label = "Select a Species:",
                choices = c("Herring", "Sprat", 
                            "Cod", "Haddock", "Whiting", "Blue whiting", "Norway Pout", "Poor Cod", 
                            "European Hake", "Monkfish", "Horse Mackerel", "Mackerel", "Common Dab", 
                            "Plaice", "Megrim", "Sole")
              ),
              sliderInput(
                inputId = "mortyear",
                label = "Time Range",
                min = 0,
                max = 100,
                value = c(1, 2),
                step = 1,
                width = "100%"
              ),
              actionButton(inputId = "goButton3", label = "Run Simulation", class = "btn-small")
            )
          ),
          
          # Main Panel for Mortality
          grid_card(
            area = "area0",
            card_body(
              tabsetPanel(
                tabPanel(title = "Change in Species", plotOutput("mortspeciesPlot")),
                tabPanel(title = "Change in Size", plotOutput("mortsizePlot")),
                tabPanel(title = "Guilds", plotOutput("mortguildPlot"))
              )
            )
          )
        )
      )
    )
  ),
  
  # Breakpoint Tab
  tabPanel(
    title = "Breakpoint",
    grid_container(
      layout = c(
        "area1 area0"
      ),
      row_sizes = c(
        "1fr"
      ),
      col_sizes = c(
        "0.3fr",
        "1.7fr"
      ),
      gap_size = "10px",
      
      # Sidebar for Breakpoint
      grid_card(
        area = "area1",
        card_body(
          sliderInput(
            inputId = "breakyear",
            label = "Year to Analyse",
            min = 0,
            max = 100,
            value = 1,
            step = 1,
            width = "100%"
          ),
          sliderInput(
            inputId = "breakrange",
            label = "Range of Mortality",
            min = 0,
            max = 0.5,
            value = c(0,0),
            step = 0.01,
            width = "100%"
          ),
          numericInput(
            inputId = "breaknumber",
            label = "Number of Simulations",
            value = 10
          ),
          selectInput(
            inputId = "breakname_select",
            label = "Select a Species:",
            choices = c("Herring", "Sprat", 
                        "Cod", "Haddock", "Whiting", "Blue whiting", "Norway Pout", "Poor Cod", 
                        "European Hake", "Monkfish", "Horse Mackerel", "Mackerel", "Common Dab", 
                        "Plaice", "Megrim", "Sole")
          ),
          actionButton(inputId = "goButton", label = "Run Simulation", class = "btn-small")
        )
      ),
      
      # Main Panel for Breakpoint
      grid_card(
        area = "area0",
        card_body(
          tabsetPanel(
            tabPanel(title = "Change in Species", plotOutput("breakspeciesPlot")),
            tabPanel(title = "Breaks", uiOutput("plots_breaks"))
          )
        )
      )
    )
  ),
  
  # Fishery Strategy Tab
  tabPanel(
    title = "Fishery Strategy",
    grid_container(
      layout = c(
        "area1 area0"
      ),
      row_sizes = c(
        "1fr"
      ),
      col_sizes = c(
        "0.3fr",
        "1.7fr"
      ),
      gap_size = "10px",
      
      # Sidebar for Fishery Strategy
      grid_card(
        area = "area1",
        card_body(
          sliderInput(
            inputId = "year",
            label = "Time Range",
            min = 0,
            max = 100,
            value = c(1, 2),
            step = 1,
            width = "100%"
          ),
          sliderInput(
            inputId = "industrial",
            label = "Commercial",
            min = 0,
            max = 1,
            value = 0.5,
            step = 0.1,
            width = "100%"
          ),
          sliderInput(
            inputId = "pelagic",
            label = "Pelagic",
            min = 0,
            max = 1,
            value = 0.5,
            step = 0.1,
            width = "100%"
          ),
          sliderInput(
            inputId = "beam",
            label = "Beam",
            min = 0,
            max = 1,
            value = 0.5,
            step = 0.1,
            width = "100%"
          ),
          sliderInput(
            inputId = "otter",
            label = "Otter",
            min = 0,
            max = 1,
            value = 0.5,
            step = 0.1,
            width = "100%"
          ),
          actionButton(inputId = "goButton2", label = "Run Simulation")
        )
      ),
      
      # Main Panel for Fishery Strategy
      grid_card(
        area = "area0",
        card_body(
          tabsetPanel(
            tabPanel(title = "Yield", plotOutput("yieldPlot")),
            tabPanel(title = "Spectra", plotOutput("spectrumPlot"))
          )
        )
      )
    )
  )
)
  shinyApp(ui = ui, server = server)












#so nothing is changing, but the herring is decreasing why?
#and also the 0 mortality range isnt working


#testing stuff

input <- list(breakrange = c(0.0, 0.2), breaknumber = 10, breakyear = 10)

# Reactive function for breaks
breaks <- function() {
  req <- function(...) NULL
  req(input$breakrange, input$breaknumber, input$breakyear)
  
  # Generate breakpoints
  breaks <- seq(input$breakrange[1], input$breakrange[2], 
                by = (input$breakrange[2] - input$breakrange[1]) / (input$breaknumber - 1))
  
  # Create a data frame with time range and mortality breakpoints
  databreak <- data.frame(mort = breaks, time = rep(input$breakyear, length(breaks)))
  
  return(databreak)
}

# Running the simulations and getting the dataframe of outputs
breaksim <- function() {
  # Read in the breaks data and make an empty dataframe
  breakpoints <- breaks()
  breaksim <- data.frame()
  
  # Read in species and get the mortality rates
  speciessim <- celticsim
  test <- getExtMort(speciessim)
  totalmort <- getMort(speciessim)
  
  unharvestedprojection <- project(celticsim,
                                   effort = c(commercial = 1, pelagic = 1, beam = 1, otter = 1),
                                   t_max = breakpoints$time[1])
  
  unharvested <- plotSpectra(unharvestedprojection, time_range = 10:10, return_data = TRUE)
  
  for (i in 1:nrow(breakpoints)) {
    

    speciessim <- celticsim
    test <- getExtMort(speciessim)
    totalmort <- getMort(speciessim)
    
    test["Herring", ] <- test["Herring", ] + (breakpoints$mort[i] * totalmort["Herring", ])
    ext_mort(speciessim) <- test

    harvestedprojection <- project(speciessim,
                                   effort = c(commercial = 1, pelagic = 1, beam = 1, otter = 1),
                                   t_max = breakpoints$time[i])
     
    harvested <- plotSpectra(harvestedprojection, time_range = breakpoints$time[i]:breakpoints$time[i], return_data = TRUE)
    print(breakpoints$mort[i])
    harvested$sim <- i

    breaksim <- rbind(breaksim, harvested)
    
  }
  
  
  return(breaksim)
}


breaknorm <- function() {
  sims <- breaksim()
  
  # Join the unharvested data to make it easier
  unharvested_grouped <- unharvested %>%
    group_by(Species) %>%
    summarise(mean_value = mean(value, na.rm = TRUE))

  sims_grouped <- sims %>%
    group_by(Species, sim) %>%
    summarise(mean_value = mean(value, na.rm = TRUE))

  normalized_data <- sims_grouped %>%
    inner_join(unharvested_grouped, by = "Species") %>%
  mutate(normalized_value = (mean_value.x / mean_value.y)-1) %>%
    select(Species, normalized_value, sim)%>%
    filter(!Species %in% c("8","6", "4", "20", "2", "19",
                           "18", "17", "16"))
  
  return(normalized_data)
}

# Run the functions to see the outputs
breaks_output <- breaks()
breaksim_output <- breaksim()
breaknorm_output <- breaknorm()

#have the output, now lets plot it.
listof <- split(breaknorm_output, breaknorm_output$sim)

example <- breaknorm_output%>%filter(sim == 5)

example <- example%>%filter(!Species %in% c("8","6", "4", "20", "2", "19",
                            "18", "17", "16"))

ggplot(example, aes(x = Species, y = normalized_value, fill = Species)) +
  geom_bar(stat = "identity") +
  labs(x = "Species", y = "Percentage Change") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  theme_minimal()

#ok so ideas - this solves the problem that the mortality added was compounding
#but it still doesnt get added to the system properly - or plotted.
#and there is no trophic cascade from the external mortality added -
#there is no difference to the unharvested than when the mortality is added
#which is just strange,.



# Define the function
run_simulation <- function(celticsim, mortspecies = 0.0, mortyear = c(10, 10)) {
  
  # Project unharvested simulation
  unharvestedprojection <- project(celticsim,
                                   effort = c(commercial = 1, pelagic = 1, beam = 1, otter = 1),
                                   t_max = mortyear[2])
  
  unharvested <- plotSpectra(unharvestedprojection, time_range = mortyear[1]:mortyear[2], return_data = TRUE)
  
  # Set external mortality rate
  speciessim <- celticsim
  test <- getExtMort(speciessim)
  totalmort <- getMort(speciessim)
  
  test["Herring",] <- test["Herring",] + (mortspecies * totalmort["Herring",])
  ext_mort(speciessim) <- test
  
  # Project harvested simulation
  harvestedprojection <- project(speciessim,
                                 effort = c(commercial = 1, pelagic = 1, beam = 1, otter = 1),
                                 t_max = mortyear[2])
  
  harvested <- plotSpectra(harvestedprojection, time_range = mortyear[1]:mortyear[2], return_data = TRUE)
  
  # Process harvested and unharvested data
  harvested <- harvested %>%
    group_by(Species) %>%
    summarise(avg_value = mean(value))
  
  unharvested <- unharvested %>%
    group_by(Species) %>%
    summarise(avg_value = mean(value))
  
  # Calculate percentage difference
  percentage_diff <- harvested %>%
    inner_join(unharvested, by = "Species") %>%
    mutate(percentage_diff = ((avg_value.x / avg_value.y) * 100)) %>%
    select(Species, percentage_diff)
  
  # Create the plot
  specieslevel <- ggplot(percentage_diff, aes(x = Species, y = percentage_diff, fill = Species)) +
    geom_bar(stat = "identity") +
    labs(title = "Average Percentage Change by Species", x = "Species", y = "Percentage Change") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 14),
          legend.position = "none",
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 16, face = "bold"))
  
  
  # Return the plot
  return(specieslevel)
}

# Run the function and store the plot

(specieslevel_plot <- run_simulation(celticsim))


percentage_diffguilds <- data.frame(
  Guild = c("Guild1", "Guild1", "Guild1", "Guild1", "Guild2", "Guild2", "Guild2", "Guild2",
            "Guild3", "Guild3", "Guild3", "Guild3", "Guild4", "Guild4", "Guild4", "Guild4"),
  percentage_diff = c(7.02, -39.80, 10.54, -12.34, 48.84, -30.12, 20.32, -5.43,
                      -10.23, 35.56, -7.89, 23.45, -6.14, -29.11, 15.67, 22.18),
  size_category = c("Medium", "Large", "Small", "Medium", "Small", "Medium", "Large", "Small",
                    "Medium", "Small", "Large", "Medium", "Small", "Medium", "Large", "Small")
)

# Plot
ggplot(percentage_diffguilds, aes(x = Guild, y = percentage_diff, fill = size_category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Percentage Change by Guild", x = "Size Category", y = "Percentage Change") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 14),
        legend.position = "none",
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"))
