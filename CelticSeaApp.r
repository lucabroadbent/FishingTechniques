library(shiny)
library(mizer)
library(ggplot2)
library(dplyr)
library(plotly)

# Load the mizer model
setwd("C:/Users/lucab/OneDrive/Desktop/Shiny")
celticsim <- readRDS("Celtic_16_untuned.rds")


# Define the UI
ui <- fluidPage(
    titlePanel("Mizer Model App"),
    tabsetPanel(
        tabPanel("Decreasing Species",
            sidebarLayout(
                sidebarPanel(
                    sliderInput("species", "Herring", min = 0, max = 1, value = 1, step = 0.01),
                    sliderInput("year", "Time Range", min = 0, max = 100, value = c(1, 2), step = 1),
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
                    sliderInput("mortspecies", "Herring", min = 0, max = 0.5, value = 0, step = 0.01),
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
                         numericInput("breaknumber", "Number of Simulations", value=10)
                        
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
  rv <- reactiveValues()
  #plots the spectra of the fishery model
   spectra <- reactive({
        effort <- c(commercial = input$industrial, pelagic = input$pelagic, beam = input$beam, otter = input$otter)
        projection <- project(celticsim, effort = effort)
        list(
            spectrum = plotSpectra(projection, time_range = input$year),
            yield = plotYield(projection)
        )
    })
  #this is just plotting the outputs of spectra
    output$spectrumPlot <- renderPlot({
        plot(spectra()$spectrum)
    })
    output$yieldPlot <- renderPlot({
        plot(spectra()$yield)
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
    specieschange <- reactive({
        speciessim <- celticsim
        unharvestedprojection <- project(celticsim,
                                         effort = c(commercial = 0, pelagic = 0, beam = 0, otter = 0),
                                         t_max = input$year[2])
        unharvested <- plotSpectra(unharvestedprojection, time_range = input$year[1]:input$year[2], return_data = TRUE)
        speciessim@initial_n["Herring", ] <- speciessim@initial_n["Herring", ] * input$species
        harvestedprojection <- project(speciessim,
                                       effort = c(commercial = 0, pelagic = 0, beam = 0, otter = 0),
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

        sizelevel <- ggplot(percentage_diffbinned, aes(x = factor(avg_weight),
                                                       y = percentage_diff, fill = factor(avg_weight))) +
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
        specieslevel <- ggplot(percentage_diff, aes(x = Species, y = percentage_diff, fill = Species)) +
            geom_bar(stat = "identity") +
            labs(title = "Average Percentage Change by Species", x = "Species", y = "Percentage Change") +
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

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

        # Plot the percentage change in each guild
        guildlevel <- ggplot(percentage_diffguilds, aes(x = Guild, y = percentage_diff, fill = size_category)) + # nolint
            geom_bar(stat = "identity", position = "dodge") +
            labs(title = "Average Percentage Change by Guild", x = "Size Category", y = "Percentage Change") +
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

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
    mortspecieschange <- reactive({
      speciessim <- celticsim
      unharvestedprojection <- project(celticsim,
                                       effort = c(commercial = 0, pelagic = 1, beam = 1, otter = 1),
                                       t_max = input$mortyear[2])
      unharvested <- plotSpectra(unharvestedprojection, time_range = input$mortyear[1]:input$mortyear[2], return_data = TRUE)
      #trying to set external mortality rate, here is how you call it
      
      test <- getExtMort(speciessim)
      totalmort <- getMort(speciessim)
      
      test["Herring",] <- test["Herring",]+(input$mortspecies*totalmort["Herring",])
      
      #setExtMort(speciessim, test)
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
      
      sizelevel <- ggplot(percentage_diffbinned, aes(x = factor(avg_weight),
                                                     y = percentage_diff, fill = factor(avg_weight))) +
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
        inner_join(unharvested, by = "Species") %>%
        mutate(percentage_diff = ((avg_value.x / avg_value.y) * 100)-100) %>%
        select(Species, percentage_diff)
      
      specieslevel <- ggplot(percentage_diff, aes(x = Species, y = percentage_diff, fill = Species)) +
        geom_bar(stat = "identity") +
        labs(title = "Average Percentage Change by Species", x = "Species", y = "Percentage Change") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
      
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
      
      # Plot the percentage change in each guild
      guildlevel <- ggplot(percentage_diffguilds, aes(x = Guild, y = percentage_diff, fill = size_category)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Average Percentage Change by Guild", x = "Size Category", y = "Percentage Change") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
      
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
    breaks <- reactive({
      req(input$breakrange, input$breaknumber, input$breakyear)
      
      # Generate breakpoints
      breaks <- seq(input$breakrange[1], input$breakrange[2], 
                     by = (input$breakrange[2] - input$breakrange[1]) / (input$breaknumber - 1))
      
      # Create a data frame with time range and mortality breakpoints
      databreak <- data.frame(mort = breaks, time = rep(input$breakyear, length(breaks)))
      
      return(databreak)
    })

    #Getting simulations from these mortality rates.
    breaksim <- reactive({
      
      #read in the breaks data and make a empty dataframe
      breakpoints <- breaks()
      breaksim <- data.frame()
      
      #read in species and get the mortality rates
      speciessim <- celticsim
      
      test <- getExtMort(speciessim)
      totalmort <- getMort(speciessim)
      
      unharvestedprojection <- project(celticsim,
                                       effort = c(commercial = 1, pelagic = 1, beam = 1, otter = 1),
                                       t_max = breakpoints$time[1])
      unharvested <- plotSpectra(unharvestedprojection, time_range = breakpoints$time[1]:breakpoints$time[1], return_data = TRUE)
      
      for (i in 1:nrow(breakpoints)) {
        
        speciessim <- celticsim
        
        test["Herring", ] <- test["Herring", ] + (breakpoints$mort[i] * totalmort["Herring", ])
        
        ext_mort(speciessim) <- test
        
        harvestedprojection <- project(speciessim,
                                       effort = c(commercial = 1, pelagic = 1, beam = 1, otter = 1),
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
    breaknorm <- reactive({
      
      sims <- breaksim()
      
      #join the unharvested data to make it easier.
      
      unharvested <- unharvested %>%
        group_by(Species)%>%
        summarise(mean_value = mean(value, na.rm = TRUE))
      
      sims <- sims %>%
        group_by(Species, sim)%>%
        summarise(mean_value = mean(value, na.rm = TRUE))
      
      normalized_data <- sims %>%
        inner_join(unharvested, by = "Species")%>%
        mutate(normalized_value = ((mean_value.x / mean_value.y)-1)*100) %>%
        select(Species, normalized_value, sim)
      
      return(normalized_data)
    })
    
    #now plotting this data
    #first need to create a function for it.
    create_species_level_plot <- function(data, plot_title) {
      ggplot(data, aes(x = Species, y = normalized_value, fill = Species)) +
        geom_bar(stat = "identity") +
        labs(title = plot_title, x = "Species", y = "Percentage Change") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
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
            plot_title <- paste("Plot - Mortality", formatted_mort_value) 
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

    

    
}

shinyApp(ui = ui, server = server)



#testing stuff

input <- list(breakrange = c(0.01, 0.2), breaknumber = 10, breakyear = 10)

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
  
  for (i in 1:nrow(breakpoints)) {

    speciessim <- celticsim
    
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

# Unharvested projection
unharvestedprojection <- project(celticsim,
                                 effort = c(commercial = 1, pelagic = 1, beam = 1, otter = 1),
                                 t_max = 100)

unharvested <- plotSpectra(unharvestedprojection, time_range = 10:10, return_data = TRUE)

# Reactive function for normalizing data
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
    select(Species, normalized_value, sim)
  
  return(normalized_data)
}

# Run the functions to see the outputs
breaks_output <- breaks()
breaksim_output <- breaksim()
breaknorm_output <- breaknorm()

#have the output, now lets plot it.
listof <- split(breaknorm_output, breaknorm_output$sim)


ggplot(breaknorm_output, aes(x = Species, y = normalized_value, fill = Species)) +
  facet_wrap(~sim) +
  geom_bar(stat = "identity") +
  labs(title = "Average Percentage Change by Species", x = "Species", y = "Percentage Change") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#ok so problems for tomorrow
#normalisation stuff is not working in breakpoints/



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
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  
  # Return the plot
  return(specieslevel)
}

# Run the function and store the plot

(specieslevel_plot <- run_simulation(celticsim))
