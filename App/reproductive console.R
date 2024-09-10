library(shiny)
library(mizer)
library(ggplot2)
library(dplyr)
library(bslib)
library(plotly)
library(ggplot2)
library(gridlayout)
library(thematic)
library(tidyverse)
library(forcats)
library(shinyBS)
library(cicerone)
library(shinydashboard)
library(shinyWidgets)

server <- function(input, output, session) {
  #here is the unharvested simulation to be used throughout for comparison
  #tmax is 200 because the maximum time to set on the sliders is 100, and 
  #for the species plots with the time range, it needs to be 2 times this amount.
  
  unharvestedprojection <- project(celticsim,
                                   effort = c(commercial = 0, pelagic = 1, beam = 1, otter = 1),
                                   t_max = 200)
  
  #This section contains all the functions to be used
  
  
  #Firstly, the function that plots the relative size spectrum plots between 2 
  #mizersim objects for a given year range - when the mizer sims differ in 
  #the starting biomass of a given species.
  
  #' MizerSim Relative Community Size Spectrum 
  #'
  #' This function plot the relative community size spectrum between two 
  #' mizerSim objects.
  #'
  #' @param object1 A mizerSim object, this is the sim you are comparing.
  #' @param object2 A mizerSim object, this is the sim you are comparing to.
  #'
  #' @return A community size spectrum - values are the relative abundance
  #' at a given size class.
  #'
  #' @examples
  #' # Compare between mizerSim objects differing in fishing strategy.
  #' 
  #' plotSpectraRelative(harvestedprojection, unharvestedprojection)
  #'
  #' @export
  plotSpectraRelative <- function(object1, object2, time1, time2) {
    
    sf1 <- mizer::plotSpectra(object1, return_data = TRUE, 
                              resource = FALSE, background = FALSE,
                              time_range = time1:time2)
    sf2 <- mizer::plotSpectra(object2, return_data = TRUE, 
                              resource = FALSE, background = FALSE,
                              time_range = time1:time2)
    
    sf <- left_join(sf1, sf2, by = c("w", "Legend")) |>
      group_by(w) |>
      summarise(x = sum(value.x, na.rm = TRUE),
                y = sum(value.y, na.rm = TRUE)) |>
      mutate(rel_diff = 2 * (y - x) / (x + y))
    
    sf <- ggplot() +
      geom_line(data = sf, 
                aes(x = w, y = rel_diff * 100), 
                color = "#2FA4E7") +
      geom_hline(yintercept = 0, linetype = 1,
                 colour = "dark grey", linewidth = 0.75) +
      labs(x = "Size (g)", 
           y = "Percentage Change") +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 14, hjust = 1, vjust = 0.5),
            axis.text.y = element_text(size = 14),
            legend.position = "none",
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16))
    
    return(sf)
  }
  
  
  #' MizerSim Relative Biomass per Species 
  #'
  #' This calculates the percentage difference between the value of biomass 
  #' that are separated by a Species column.
  #'
  #' @param harvested An array (time x species)
  #' @param unharvested An array (time x species), the value you are comparing to.
  #'
  #' @return A dataframe of Species and Biomass. Biomass gives the percentage
  #' difference of the value of biomass between the harvested and unharvested
  #' mizerSim objects.
  #' 
  #'
  #' @examples
  #' harvested <- getBiomass(NS_sim)
  #' unharvested <- getBiomass(NS_sim)
  #' percentdiff(harvested, unharvested)
  #'
  #' @export
  percentdiff <- function(harvested, unharvested) {
    harvested %>%
      left_join(unharvested, by = "Species") %>%
      mutate(percentage_diff = ((value.x - value.y) / value.y) * 100) %>%
      select(Species, percentage_diff) %>%
      filter(!Species %in% c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"))
  }
  
  
  
  
  #This function plots the species plot - which the change in species for a given 
  #year, and also for 2x in future and 1/3 year in the past.
  
  
  #' Plot MizerSim Relative Biomass per Species Across Varying Timescales 
  #'
  #' This function takes two mizerSim objects and calculates the relative % 
  #' change in each given species in the chosen year, short term (1/3 of the 
  #' chosen year) and the long term (2x the chosen year) 
  #'
  #' @param harvested A mizerSim object
  #' @param unharvested A mizerSim object - to compare to.
  #' @param chosentime The year to plot 
  #'
  #' @return A ggplot object that plots 3 bars per species - in the short, 
  #' chosen and long time - it plots the relative biomass of each species in
  #' comparison to the unharvested.
  #' 
  #'
  #' @examples
  #' harvested <- getBiomass(NS_sim)
  #' unharvested <- getBiomass(NS_sim)
  #' percentdiff(harvested, unharvested)
  #'
  #' @export
  plotSpeciesWithTimeRange <- function(harvestedprojection, unharvestedprojection, chosentime) {
    
    #get the biomass of the species
    unharvestedbio <- getBiomass(unharvestedprojection) %>%
      .[chosentime, ] %>%
      melt() %>%
      rownames_to_column(var = "Species")
    
    harvestedbio <- getBiomass(harvestedprojection) %>%
      .[chosentime, ] %>%
      melt() %>%
      rownames_to_column(var = "Species")
    
    #calculate percentage change in species in the chosen year
    percentage_diff <- percentdiff(harvestedbio, unharvestedbio)
    percentage_diff$class <- "chosen"
    
    calculate_biomass_triples <- function(unharvestedprojection, harvestedprojection, year) {
      
      # Calculate unharvested biomass at different time points
      unharvestedbiotriple <- getBiomass(unharvestedprojection)
      
      lowunbiotrip <- unharvestedbiotriple[max(1, round(year * (1/2))), ] %>%
        melt() %>%
        rownames_to_column(var = "Species")
      
      highunbiotrip <- unharvestedbiotriple[year * 2, ] %>%
        melt() %>%
        rownames_to_column(var = "Species")
      
      # Calculate harvested biomass at different time points
      harvestedbiotriple <- getBiomass(harvestedprojection)
      
      lowbiotrip <- harvestedbiotriple[max(1, round(year * (1/2))), ] %>%
        melt() %>%
        rownames_to_column(var = "Species")
      
      highbiotrip <- harvestedbiotriple[year * 2, ] %>%
        melt() %>%
        rownames_to_column(var = "Species")
      
      # Return the results as a list
      list(
        lowunbiotrip,
        highunbiotrip,
        lowbiotrip,
        highbiotrip
      )
    }
    #calculate percentage change in other years
    biorange <- calculate_biomass_triples(unharvestedprojection, harvestedprojection, chosentime)
    
    percentage_difflow <- percentdiff(biorange[[3]], biorange[[1]])
    percentage_difflow$class <- "short"
    
    percentage_diffhigh <- percentdiff(biorange[[4]], biorange[[2]])
    percentage_diffhigh$class <- "long"
    
    percentage_diff <- rbind(percentage_difflow, percentage_diff, percentage_diffhigh)
    
    #now plot them together - the first lines sort out the colors of the bars
    percentage_diff$class <- factor(percentage_diff$class, levels = c("short", "chosen", "long"))
    percentage_diff$fill_group <- interaction(percentage_diff$percentage_diff >= 0, percentage_diff$class)
    
    ggplot(percentage_diff, aes(x = Species, y = percentage_diff, fill = fill_group)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
      geom_hline(yintercept = 0, color = "grey", linetype = "dashed", size = 0.5)+
      labs(x = "Species", y = "Percentage Change") +
      scale_fill_manual(values = c(
        "FALSE.short" = "#E76F51",  
        "FALSE.chosen" = "#E98C6B",  
        "FALSE.long" = "#F2A488",   
        "TRUE.short" = "#2FA4E7", 
        "TRUE.chosen" = "#2FA4E7cc",
        "TRUE.long" = "#2FA4E799" 
      )) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 14),
        legend.position = "none",
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
      )
  }
  
  
  
  
  #' Plot Guild Relative Change Across Timescales  
  #'
  #' This function takes two mizerSim objects and calculates the relative % 
  #' change in each given feeding guilde in the chosen year, short term (1/3 of the 
  #' chosen year) and the long term (2x the chosen year) 
  #'
  #' @param harvested A mizerSim object
  #' @param unharvested A mizerSim object - to compare to.
  #' @param chosentime The year to plot 
  #'
  #' @return A ggplot object that plots 3 bars per species - in the short, 
  #' chosen and long time - it plots the relative biomass of each feeding guild 
  #' in comparison to the unharvested.
  #' 
  #'
  #' @examples
  #' harvested <- getBiomass(NS_sim)
  #' unharvested <- getBiomass(NS_sim)
  #' guildplot(harvested, unharvested, 5)
  #'
  #' @export
  guildplot <- function(harvestedprojection, unharvestedprojection, chosentime) {
    #remember to add the rule about the smallest sizes being plantivores.
    
    harvestedshort <- plotSpectra(harvestedprojection, time_range = round(1/2*chosentime), return_data = TRUE)
    harvested <- plotSpectra(harvestedprojection, time_range = chosentime, return_data = TRUE)
    harvestedlong <- plotSpectra(harvestedprojection, time_range = 2*chosentime, return_data = TRUE)
    
    unharvestedshort <- plotSpectra(unharvestedprojection, time_range = round(1/2*chosentime), return_data = TRUE)
    unharvested <- plotSpectra(unharvestedprojection, time_range = chosentime, return_data = TRUE)
    unharvestedlong <- plotSpectra(unharvestedprojection, time_range = 2*chosentime, return_data = TRUE)
    
    process_guilds <- function(harvested2) {
      
      
      find_guild <- function(w, species, guildparams) {
        matched <- guildparams %>%
          filter(Species == species, w >= minw, w < maxw)
        
        if (nrow(matched) > 0) {
          return(matched$Feeding.guild)
        } else {
          return(NA_character_)
        }
      }
      
      result <- harvested2 %>%
        rowwise() %>%
        mutate(Guild = find_guild(w, Species, guildparams)) %>%
        ungroup() %>%
        drop_na(Guild)%>% 
        group_by(Guild)%>%
        summarise(value=mean(value))%>%
        distinct()
      
      
      return(result)
    }
    
    #for the harvested - 
    guildsshort <- process_guilds(harvestedshort)
    guilds <- process_guilds(harvested)
    guildslong <- process_guilds(harvestedlong)
    #for the unharvested - 
    unguildsshort <- process_guilds(unharvestedshort)
    unguilds <- process_guilds(unharvested)
    unguildslong <- process_guilds(unharvestedlong)
    
    #now joining them together
    guildsshort$time <- "short"
    guilds$time <- "chosen"
    guildslong$time <- "long"
    unguildsshort$time <- "short"
    unguilds$time <- "chosen"
    unguildslong$time <- "long"
    
    joinedguilds <- bind_rows(guildsshort, guilds, guildslong) %>%
      group_by(Guild, time) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
    
    unjoinedguilds <- bind_rows(unguildsshort, unguilds, unguildslong) %>%
      group_by(Guild, time) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
    
    joinedguilds <- joinedguilds%>%
      full_join(unjoinedguilds, by = c("Guild", "time"),relationship = "many-to-many") %>%
      mutate(percentage_diff = ((value.x-value.y)/value.y))%>%
      select(Guild, time, percentage_diff)
    
    joinedguilds$time <- factor(joinedguilds$time, levels = c("short", "chosen", "long"))
    
    #plotting
    ggplot(joinedguilds, aes(x = Guild, y = percentage_diff, fill = factor(time))) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
      scale_fill_manual(values = c("#2FA4E7", "#2FA4E7cc", "#2FA4E799")) +
      labs(title = "Percentage Change by Guild", 
           x = "Guild", 
           y = "Percentage Change") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 14),
        legend.position = "none",
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
      )
    
  }
  
  
  #This loads and formats in the data for the guilds
  
  guildinfo <- read.table("Guilds information/guild_cleaned.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  fish_names <- read.table("Guilds information/fishinfo.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  
  guildparams <- celticsim@species_params%>%
    select(species, a, b)%>%
    rename(Common_Name=species)%>%
    #this code has correctly formatted the species params
    inner_join(fish_names, by=c("Common_Name"))%>%
    #we have now joined species params to a table containing the scientific names
    rename(Species=Scientific_Name)%>%
    inner_join(
      guildinfo%>%
        filter(Species %in% fish_names$Scientific_Name), 
      by="Species")%>%
    #we have now joined the rows with the same scientific names - so 
    #we have joined the a and b values to the given species 
    
    #this is converting from length to weight
    mutate(maxw=a*Max.cm^b,
           minw=a*Min.cm^b)%>%
    select(Common_Name, maxw, minw, Feeding.guild)%>%
    rename(Species=Common_Name)
  
  #So what the code does above is take the table of the guild information,
  #from the pilot assessment by Murray Thompson, then you give it a table
  #containing the species common + scientific names, and this is all then 
  #joined together so you have the a and b values next to given species, 
  #so therefore we are able to convert from the length measurements to weight,
  #which can then be used in mizer to filter into the correct guilds.
  
  
  
  #This function plots the diet matrix from the mizersim objects.
  #' Plot Relative Diet Proportion of each Prey/Predator  
  #'
  #' This function takes two mizerSim objects and calculates the relative 
  #' change in the proportion of a given prey species in a predators diet. This 
  #' is done for every prey/predator in the model. 
  #'
  #' @param harvested A mizerSim object
  #' @param unharvested A mizerSim object - to compare to.
  #' @param chosentime The year to plot 
  #'
  #' @return A ggplot object of a matrix of predator species on the X axis, 
  #' prey species on the Y axis. The colour of the box indicates the change 
  #' of the proportion in the predator's diet of the given prey species.
  #' 
  #'
  #' @examples
  #' harvested <- getBiomass(NS_sim)
  #' unharvested <- getBiomass(NS_sim)
  #' comparedietmatrix(harvested, unharvested, 5)
  #'
  #' @export
  comparedietmatrix <- function(unharvestedprojection, harvestedprojection, timerange){
    
    dietunharv <- getDiet(unharvestedprojection@params, 
                          n = unharvestedprojection@n[timerange,,],
                          n_pp = unharvestedprojection@n_pp[timerange,],
                          n_other = unharvestedprojection@n_other[timerange,],
                          proportion = TRUE)%>%
      as.table()%>%
      as.data.frame()%>%
      group_by(predator, prey)%>%
      summarise(Proportion=mean(Freq))
    
    dietharv <- getDiet(harvestedprojection@params, 
                        n = harvestedprojection@n[timerange,,],
                        n_pp = harvestedprojection@n_pp[timerange,],
                        n_other = harvestedprojection@n_other[timerange,],
                        proportion = TRUE)%>%
      as.table()%>%
      as.data.frame()%>%
      group_by(predator, prey)%>%
      summarise(Proportion=mean(Freq))
    
    joindiet <- left_join(dietharv, dietunharv, by = c("prey", "predator"))%>%
      mutate(Difference = ((Proportion.x - Proportion.y) / Proportion.y) * 100) %>%  # Calculate percentage change
      select(predator, prey, Difference)%>%
      filter(!predator %in% c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"), 
             !prey %in% c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"))
    
    dietplot <- ggplot(joindiet, aes(x = predator, y = prey, fill = Difference)) +
      geom_tile() +  
      scale_fill_gradient2() +  
      labs(x = "Predator",
           y = "Prey",
           fill = "Difference") +  
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16))
    
    return(dietplot)
    
  }
  
  
  #Here is the code that sets the ordering of the species on the plots.
  
  ordered_species <- reactive({
    if (input$species_order == "Alphabetical") {
      # Order species alphabetically
      as.data.frame(celticsim@species_params$species)%>%
        setNames("Species")%>%
        filter(!Species %in% c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"))%>%
        pull(Species)
    } else if (input$species_order == "Guild") {
      #THIS WILL NOT WORK AUTOMATICALLY WITH NEW GUILDS/SPECIES, needs to be changed.
      c("Cod","Whiting", "European Hake","Monkfish","Haddock","Common Dab","Poor Cod",
        "Plaice","Megrim","Sole","Blue Whiting","Herring","Sprat","Norway Pout","Horse Mackerel",
        "Mackerel"
      )
    } else if (input$species_order == "Size") {
      #order by maturity size
      species_order <- data.frame(celticsim@species_params$species, celticsim@species_params$w_mat)%>%
        setNames(c("Species", "mat"))%>%
        filter(!Species %in% c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"))%>%
        arrange(mat)%>%
        pull(Species)
      species_order
    }
  })
  
 
  
  #load in the simulations 
  celticsim <- readRDS("Celtic_16_untuned.rds")
  
  
  speciessim <- reactiveVal(celticsim)
  
  confirmResetModal <- function() {
    showModal(modalDialog(
      title = "Confirm Reset",
      "Are you sure you want to reset the simulation?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirmReset", "Confirm")
      )
    ))
  }
  
  # Show the confirmation modal when the reset button is clicked
  observeEvent(input$goButton3, {
    confirmResetModal()
  })
  
  # Perform reset when the confirm button is clicked
  observeEvent(input$confirmReset, {
    removeModal()  # Close the modal after confirmation
    reset_simulation()  # Call the reset function to reset the simulation
  })
  
  # Define your reset function
  reset_simulation <- function() {
    speciessim(celticsim)  # Reset the simulation to celticsim
  }
  
  
  # Find out which species is being selected
  species_index <- reactive({
    which(speciessim()@species_params$species == input$species_name_select)
  })
  

  
  
  #this is for the buttons 
  adjustment_amount <- reactiveVal(0)
  
  # Update adjustment amount based on button clicks
  observeEvent(input$minus_001, { adjustment_amount(-0.001) })
  observeEvent(input$plus_001, { adjustment_amount(0.001) })
  observeEvent(input$minus_005, { adjustment_amount(-0.005) })
  observeEvent(input$plus_005, { adjustment_amount(0.005) })
  observeEvent(input$minus_1_percent, {
    sim <- speciessim()
    current_value <- sim@species_params$erepro[species_index()]
    adjustment_amount(adjustment_amount() - 0.01 * current_value)
  })
  observeEvent(input$plus_1_percent, {
    sim <- speciessim()
    current_value <- sim@species_params$erepro[species_index()]
    adjustment_amount(adjustment_amount() + 0.01 * current_value)
  })
  observeEvent(input$minus_5_percent, {
    sim <- speciessim()
    current_value <- sim@species_params$erepro[species_index()]
    adjustment_amount(adjustment_amount() - 0.05 * current_value)
  })
  observeEvent(input$plus_5_percent, {
    sim <- speciessim()
    current_value <- sim@species_params$erepro[species_index()]
    adjustment_amount(adjustment_amount() + 0.05 * current_value)
  })
  
  #this updates the value when the slider is changed
  species_reactive <- reactiveVal(1)
  
  observeEvent(input$goButton2, {
    species_reactive(input$species)
  })
  
  speciessim_altered <- reactive({
    sim <- speciessim()  # Retrieve current simulation
    sim@species_params$erepro[species_index()] <- sim@species_params$erepro[species_index()] * species_reactive()
    species_reactive(1)
    #this is where you adjust by the other buttons
    current_value <- sim@species_params$erepro[species_index()]
    new_value <- current_value + adjustment_amount() 
    adjustment_amount(0) # Apply adjustment
    sim@species_params$erepro[species_index()] <- new_value
    speciessim(sim)  
    return(sim)
  })

  
  text_alter <- observeEvent(input$goButton2, {
    sim <- speciessim()
    repro_value <- ifelse(is.na(as.numeric(input$repro_id)) || input$repro_id == "", 1, as.numeric(input$repro_id))
    sim@species_params$erepro[species_index()] <- as.numeric(repro_value)
    speciessim(sim)
  })
  
  
  #run simulation and plot
  specieschange <- eventReactive(input$goButton1,{

    
    unharvested <- plotSpectra(unharvestedprojection, time_range = input$year[1]:input$year[2], return_data = TRUE)
    
    
    speciessim <- speciessim_altered()
    
    
    harvestedprojection <- project(speciessim,
                                   effort = c(commercial = 0, pelagic = 1, beam = 1, otter = 1),
                                   t_max = input$year[2]*2)
    
    harvested <- plotSpectra(harvestedprojection, time_range = input$year[1]:input$year[2], return_data = TRUE)
    
    
    harvested2 <- harvested
    unharvested2 <- unharvested
    harvested3 <- harvested
    unharvested3 <- unharvested
    
    
    #plotting the relative size spectrum
    
    sizelevel <- plotSpectraRelative(harvestedprojection, unharvestedprojection, input$year[1], input$year[2])
    
    
    #This next section calculates the species level change - across 
    
    specieslevel <- plotSpeciesWithTimeRange(harvestedprojection, unharvestedprojection, input$year[1])
    
    
    #This next section calculates the guilds
    
    guildlevel <- guildplot(harvestedprojection, unharvestedprojection, input$year[1])
    
    
    #now I am plotting the diet matrixes
    
    dietplot <- comparedietmatrix(harvestedprojection, unharvestedprojection, input$year[1])
    
    list(sizelevel = sizelevel, specieslevel = specieslevel, guildlevel = guildlevel, dietplot = dietplot, 
         harvestedprojection=harvestedprojection)
  })
  
  
  output$speciesPlot <- renderPlotly({
    specieschange()$specieslevel
  })
  
  output$sizePlot <- renderPlotly({
    specieschange()$sizelevel
  })
  output$guildPlot <- renderPlotly({
    specieschange()$guildlevel
  })
  
  output$dietPlot <- renderPlotly({
    specieschange()$dietplot
  })
  
  output$biomassplot <- renderPlotly({
    plotBiomass(specieschange()$harvestedprojection, start_time = input$year[1], end_time = input$year[2])
  })
  
  output$plot <- renderPlotly({
    plot(speciessim_altered())
  })
  
  dietchange <- reactive({input$species})
  
  output$dietplot2 <- renderPlotly({
    req(input$species_name_select)  # Ensure input$species is available
    plotDiet(speciessim_altered(), species = input$species_name_select)
  })
  
  output$feeding <- renderPlotly({
    plotFeedingLevel(specieschange()$harvestedprojection, time_range = input$year[1]:input$year[2])
  })
  
  
  
  
  ##this plots the table of the reactivity

  output$speciesTable <- renderTable({
    # Check the structure of speciessim_altered()
    altered <- speciessim_altered()

    table <- data.frame(
      Species = altered@species_params$species,
      ReproductiveValue = altered@species_params$erepro
    )
    
    # Format the ReproductiveValue column
    table$ReproductiveValue <- format(table$ReproductiveValue, digits = 4, scientific = FALSE)

    table  # Return the table for rendering
  })

  
  observe({
    output$speciesValueBox <- renderValueBox({
      value <- speciessim_altered()@species_params$erepro[species_index()] * species_reactive()
      value <- format(value, digits = 6)
      valueBox(
        value = value, 
        subtitle = paste("Reproductive value of", input$species_name_select,
                         ": Original -", celticsim@species_params$erepro[species_index()]),
        width = 12
      )
    })
  })
  
  
  #download sims - or download epro file
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("speciessim_altered-", Sys.Date(), ".RData", sep = "")
    },
    content = function(file) {
      sim <- speciessim_altered()@species_params$erepro
      save(reproductive_values, file = file)
    }
  )
}

ui <- dashboardPage(
  dashboardHeader(title = "Celtic Sea Mizer Model"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Simulation Controls", tabName = "controls", icon = icon("sliders")),
      selectInput(
        inputId = "species_name_select",
        label = "Select a Species:",
        choices = c("Herring", "Sprat", "Cod", "Haddock", "Whiting", "Blue whiting", 
                    "Norway Pout", "Poor Cod", "European Hake", "Monkfish", "Horse Mackerel", 
                    "Mackerel", "Common Dab", "Plaice", "Megrim", "Sole")
      ),
      sliderInput(
        inputId = "species",
        label = "Reproductive level",
        min = 0.5,
        max = 1.5,
        value = 1,
        step = 0.01
      ),
      
      sliderInput(
        inputId = "year",
        label = "Time Range",
        min = 1,
        max = 100,
        value = c(5,6),
        step = 1
      ),
      textInput(inputId = "repro_id", label = "Enter erepro value:", value = ""),
      actionButton(inputId = "goButton2", label = "Change"),
      actionButton(inputId = "goButton1", label = "Run Simulation"),
      actionButton(inputId = "goButton3", label = "Reset (all)"),
      valueBoxOutput("speciesValueBox", width=12),
      fluidRow(
        # First row of buttons: +/- 0.01 and +/- 0.05
        actionButton("minus_001", "- 0.001", class = "custom-btn"),
        actionButton("plus_001", "+ 0.001", class = "custom-btn"),
        actionButton("minus_005", "- 0.005", class = "custom-btn"),
        actionButton("plus_005", "+ 0.005", class = "custom-btn"),
        actionButton("minus_1_percent", "- 1%", class = "custom-btn"),
        actionButton("plus_1_percent", "+ 1%", class = "custom-btn"),
        actionButton("minus_5_percent", "- 5%", class = "custom-btn"),
        actionButton("plus_5_percent", "+ 5%", class = "custom-btn")
      ),

      downloadButton("downloadData", "Save")
      
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .row {
          margin-right: 45px;
          margin-left: -20px;
        }
        section.sidebar .fluidRow {
          display: flex;
          flex-direction: column; /* Stack buttons vertically within the container */
          align-items: center; /* Center buttons horizontally */
          padding: 0;
          margin: 0; /* Remove default margin */
        }
  /* Ensure custom buttons are displayed side by side in the sidebar */
section.sidebar .shiny-bound-input.custom-btn {
    display: inline-block;
    margin: -1px -1px 4px 42px;
    width: 59px;
}
  
  /* Ensure that the parent container (fluidRow) supports inline buttons */
  section.sidebar .fluidRow {
    overflow-x: auto; /* Adds horizontal scroll if necessary */
  }
  section.sidebar .fluidRow {
    white-space: nowrap; /* Prevents wrapping of buttons */
    overflow-x: auto; /* Adds horizontal scroll if necessary */
    padding-left: 10px; /* Add padding to ensure buttons are not off the screen */
    padding-right: 10px; /* Add padding to ensure buttons are not off the screen */
    box-sizing: border-box; /* Includes padding in the element's total width and height */
  }
  section.sidebar .shiny-bound-input.custom-btn {
    display: inline-block;
    margin: -1px -1px 4px 42px;
    width: 59px;
  }
.main-sidebar .user-panel, .sidebar-menu, .sidebar-menu>li.header {
    white-space: normal;
    overflow: hidden;
}
      "))
    ),
    tabItems(
      tabItem(tabName = "controls",
              fluidRow(
                tabBox(
                  width = 12,
                  tabPanel("Table", tableOutput("speciesTable")),
                  tabPanel("Plots", plotlyOutput("plot")),
                  tabPanel("Species", plotlyOutput("speciesPlot")),
                  tabPanel("Size", plotlyOutput("sizePlot")),
                  tabPanel("Guilds", plotlyOutput("guildPlot")),
                  tabPanel("Diet", plotlyOutput("dietPlot")),
                  tabPanel("Biomass", plotlyOutput("biomassplot")),
                  tabPanel("Feeding", plotlyOutput("feeding")),
                  tabPanel("Single Diet", plotlyOutput("dietplot2"))
                )
              )
      )
    )
  )
)


shinyApp(ui = ui, server = server)
 