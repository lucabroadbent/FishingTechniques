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

##questions to ask 
#we have smaller sizes for the guilds - can I just assume that anything of the very
#small size classes is planktivorous? - for example anything smaller than 0.5g?
#otherwise, there is big problems - we miss out a lot of the spectrum -
#and to include some size resolution in the plot, we have to do weird things.
#currently this is the entire size spectrum range in mizer logged and 1/3.
#there are none in the smallest size class - but if we do not log and just take 1/3
#then everything is in the smallest size class. 

server <- function(input, output, session) {
  
  celticsim <- readRDS("Celtic_16_untuned.rds")
  
  thematic::thematic_shiny()
  
  #plots the spectra of the fishery model
  spectra <- eventReactive(input$goButton2,{
    effort <- c(commercial = input$industrial, pelagic = input$pelagic, beam = input$beam, otter = input$otter)
    projection <- project(celticsim, effort = effort)
    list(
      spectrum = plotlySpectra(projection, time_range = input$year),
      yield = plotlyYield(projection)
    )
  })
  
  #this is just plotting the outputs of spectra
  output$spectrumPlot <- renderPlotly({
    spectra()$spectrum
  })
  output$yieldPlot <- renderPlotly({
    spectra()$yield
  })
  
  
  
  #This section has all the buttons for the years to change
  
  observeEvent(input$set_year_5, {
    updateSliderInput(session, "year", value = c(5, 5))
  })
  observeEvent(input$set_year_15, {
    updateSliderInput(session, "year", value = c(15, 15))
  })
  observeEvent(input$set_year_30, {
    updateSliderInput(session, "year", value = c(30, 30))
  })
  
  observeEvent(input$mortset_year_5, {
    updateSliderInput(session, "mortyear", value = c(5, 5))
  })
  observeEvent(input$mortset_year_15, {
    updateSliderInput(session, "mortyear", value = c(15, 15))
  })
  observeEvent(input$mortset_year_30, {
    updateSliderInput(session, "mortyear", value = c(30, 30))
  })
  
  observeEvent(input$breakset_year_5, {
    updateSliderInput(session, "breakyear", value = 5)
  })
  observeEvent(input$breakset_year_15, {
    updateSliderInput(session, "breakyear", value = 15)
  })
  observeEvent(input$breakset_year_30, {
    updateSliderInput(session, "breakyear", value = 30)
  })
  
  
  
  #here is the unharvested simulation to be used throughout for comparison
  
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
  plotSpectraRelative <- function(object1, object2) {
    
    sf1 <- mizer::plotSpectra(object1, return_data = TRUE, 
                              resource = FALSE, background = FALSE)
    sf2 <- mizer::plotSpectra(object2, return_data = TRUE, 
                              resource = FALSE, background = FALSE)
    
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
      labs(title = "Percentage Change by Size", 
           x = "Size (g)", 
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
      
      lowunbiotrip <- unharvestedbiotriple[round(year * (1/3)), ] %>%
        melt() %>%
        rownames_to_column(var = "Species")
      
      highunbiotrip <- unharvestedbiotriple[year * 2, ] %>%
        melt() %>%
        rownames_to_column(var = "Species")
      
      # Calculate harvested biomass at different time points
      harvestedbiotriple <- getBiomass(harvestedprojection)
      
      lowbiotrip <- harvestedbiotriple[round(year * (1/3)), ] %>%
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
      labs(title = "Percentage Change by Species", x = "Species", y = "Percentage Change") +
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
    
    harvestedshort <- plotSpectra(harvestedprojection, time_range = round(1/3*chosentime), return_data = TRUE)
    harvested <- plotSpectra(harvestedprojection, time_range = chosentime, return_data = TRUE)
    harvestedlong <- plotSpectra(harvestedprojection, time_range = 2*chosentime, return_data = TRUE)
    
    unharvestedshort <- plotSpectra(unharvestedprojection, time_range = round(1/3*chosentime), return_data = TRUE)
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
      labs(title = "Heatmap of Difference",
           x = "Predator",
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
  
  
  
  #This section is for the biomass change of species (tab = biomass)
  
  specieschange <- eventReactive(input$goButton1,{
    
    #this creates a progress bar.
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Running simulation...", value = 0)
    
    #firstly, you run the simulation - with changed biomass and 
    # for the unharvested
    
    speciessim <- celticsim
    
    unharvested <- plotSpectra(unharvestedprojection, time_range = input$year[1]:input$year[2], return_data = TRUE)
    
    
    #changing the initial biomass
    
    speciessim@initial_n[input$species_name_select, ] <- speciessim@initial_n[input$species_name_select, ] * input$species
    
    
    #running the simulation with the changed biomass
    
    harvestedprojection <- project(speciessim,
                                   effort = c(commercial = 0, pelagic = 1, beam = 1, otter = 1),
                                   t_max = input$year[2]*2,
                                   progress_bar = progress)
    
    harvested <- plotSpectra(harvestedprojection, time_range = input$year[1]:input$year[2], return_data = TRUE)
    
    
    #setting the variables to use later
    
    harvested2 <- harvested
    unharvested2 <- unharvested
    harvested3 <- harvested
    unharvested3 <- unharvested
    
    
    #plotting the relative size spectrum
    
    sizelevel <- plotSpectraRelative(harvestedprojection, unharvestedprojection)
    
    
    #This next section calculates the species level change - across 
    
    specieslevel <- plotSpeciesWithTimeRange(harvestedprojection, unharvestedprojection, input$year[1])
    
    
    #This next section calculates the guilds
    
    guildlevel <- guildplot(harvestedprojection, unharvestedprojection, input$year[1])
    
    
    #now I am plotting the diet matrixes
    
    dietplot <- comparedietmatrix(harvestedprojection, unharvestedprojection, input$year[1])
    
    list(sizelevel = sizelevel, specieslevel = specieslevel, guildlevel = guildlevel, dietplot = dietplot)
  })
  
  #Now this next bit of code takes the outputs from the biomass change section 
  #and plots them into the tabs / app
  
  output$speciesPlot <- renderPlotly({
    specieschange()$specieslevel+scale_x_discrete(limits = ordered_species())
  })
  
  output$sizePlot <- renderPlotly({
    specieschange()$sizelevel
  })
  output$guildPlot <- renderPlotly({
    specieschange()$guildlevel
  })
  
  output$dietPlot <- renderPlotly({
    specieschange()$dietplot+scale_x_discrete(limits = ordered_species())
  })
  
  #fake radar plot
  output$radar <- renderPlotly({
    # Create the data
    data <- tibble(
      Category = c("Scenario 1", "Scenario 2"),
      Change_in_Biomass = c(0.5, 0.4),
      Change_in_Yield = c(0.2, 0.3),
      Proportion_Large_Fish = c(0.4, 0.6),
      Risk = c(0.3, 0.5)
    )
    
    # Create radar chart
    radar <- ggradar(data,
                     axis.labels = c("Change_in_Biomass", "Yield", "Proportion Large Fish", "Risk"),
                     group.colours = c("#2FA4E7CC", "#9ad6fc"),
                     background.circle.colour = "white",
                     label.gridline.mid=FALSE,
                     legend.position = "top")
    
    # Print radar chart
    print(radar)
  })
  
  
  
  #Plots of herring mortality decrease.
  
  
  #This next section is for the added mortality - everything is the same as 
  #above, except for the first section where the mortality is added
  mortspecieschange <- eventReactive(input$goButton3,{
    
    speciessim <- celticsim
    
    
    unharvested <- plotSpectra(unharvestedprojection, time_range = input$mortyear[1]:input$mortyear[2], return_data = TRUE)
    
    
    #changing the mortality here 
    
    extmort <- getExtMort(speciessim)
    totalmort <- getMort(speciessim)
    extmort[input$name_select,] <- extmort[input$name_select,]+(input$mortspecies*totalmort[input$name_select,])
    ext_mort(speciessim) <- extmort
    
    
    harvestedprojection <- project(speciessim,
                                   effort = c(commercial = 0, pelagic = 1, beam = 1, otter = 1),
                                   t_max = input$mortyear[2]*2)
    
    harvested <- plotSpectra(harvestedprojection, time_range = input$mortyear[1]:input$mortyear[2], return_data = TRUE)
    
    
    harvested2 <- harvested
    unharvested2 <- unharvested
    harvested3 <- harvested
    unharvested3 <- unharvested
    
    
    #plotting the relative size spectrum
    
    sizelevel <- plotSpectraRelative(harvestedprojection, unharvestedprojection)
    
    
    #This next section calculates the species level change - across 
    
    specieslevel <- plotSpeciesWithTimeRange(harvestedprojection, unharvestedprojection, input$year[1])
    
    
    #This next section calculates the guilds
    
    guildlevel <- guildplot(harvestedprojection, unharvestedprojection, input$year[1])
    
    
    #now I am plotting the diet matrixes
    
    dietplot <- comparedietmatrix(harvestedprojection, unharvestedprojection, input$year[1])
    
    list(sizelevel = sizelevel, specieslevel = specieslevel, guildlevel = guildlevel, dietplot = dietplot)
  })
  
  
  #Plotting the outputs of the mortality decreases
  output$mortspeciesPlot <- renderPlotly({
    mortspecieschange()$specieslevel
  })
  output$mortsizePlot <- renderPlotly({
    mortspecieschange()$sizelevel
  })
  output$mortguildPlot <- renderPlotly({
    mortspecieschange()$guildlevel
  })
  
  
  #This next section is for 
  #Plotting breakpoints for the mortality increases
  
  
  #this first chunk generates a dataframe of the values of mortality added
  #for each simulation
  
  breaks <- eventReactive(input$goButton,{
    req(input$breakrange, input$breaknumber, input$breakyear)
    
    breaks <- seq(input$breakrange[1], input$breakrange[2], 
                  by = (input$breakrange[2] - input$breakrange[1]) / (input$breaknumber - 1))
    
    databreak <- data.frame(mort = breaks, time = rep(input$breakyear, length(breaks)), 
                            sim=seq_len(length(breaks)))
    return(databreak)
  })
  
  
  
  #this next chunk takes the values of mortality and runs a simulation with them
  #added
  
  breaksim <- eventReactive(input$goButton,{
    breakpoints <- breaks()
    breaksim <- data.frame()
    
    celticsim <- readRDS("Celtic_16_untuned.rds")
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
      
      harvestedbio <- getBiomass(harvestedprojection)
      harvestedbio <- melt(harvestedbio[breakpoints$time[i]:breakpoints$time[i],])
      harvestedbio <- rownames_to_column(harvestedbio, var="Species")
      harvestedbio$sim <- i
      
      breaksim <- rbind(breaksim, harvestedbio)
      
    }
    
    return(breaksim)
    
  })
  
  
  
  #Now this next chunk normalises the species biomass to the unharvested biomass
  
  breaknorm <- eventReactive(input$goButton,{
    
    sims <- breaksim()
    breakpoints <- breaks()
    
    
    unharvestedprojection <- project(celticsim,
                                     effort = c(commercial = 0, pelagic = 1, beam = 1, otter = 1),
                                     t_max = breakpoints$time[1])
    
    unharvestedbio <- getBiomass(unharvestedprojection)
    unharvestedbio <- melt(unharvestedbio[breakpoints$time[1]:breakpoints$time[1],])
    
    unharvestedbio <- rownames_to_column(unharvestedbio, var="Species")
    
    
    normalized_data <- sims %>%
      inner_join(unharvestedbio, by = "Species")%>%
      mutate(normalized_value = ((value.x / value.y)-1)*100) %>%
      select(Species, normalized_value, sim) %>%
      filter(!Species %in% c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"))
    return(normalized_data)
    
  })
  
  
  
  #This function is to plot the species level data here with a dynamic title
  
  create_species_level_plot <- function(data, plot_title) {
    ggplot(data, aes(x = Species, y = normalized_value, fill = Species)) +
      geom_bar(stat = "identity", fill="#2FA4E7") +
      labs(title = plot_title, x = "Species", y = "Percentage Change") +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust = 0.5),
            axis.text.y = element_text(size = 14),
            legend.position = "none",
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16))
    
  }
  
  
  
  #now a function to split the dataframe into a list
  data_list <- reactive({
    split(breaknorm(), breaknorm()$sim)
  })
  
  
  
  #this chunk plots each of the simulations and stores them as a list
  
  observe({
    breakpoint <- breaks()
    num_plots <- input$breaknumber
    lapply(1:num_plots, function(i) {
      local({
        my_i <- i
        plotname <- paste("plot", my_i, sep = "")
        output[[plotname]] <- renderPlotly({
          current_data <- data_list()[[my_i]]
          mort_value <- breakpoint$mort[my_i] * 100
          formatted_mort_value <- sprintf("+%.2f%%", mort_value)
          plot_title <- paste("Mortality", formatted_mort_value) 
          create_species_level_plot(current_data, plot_title)
        })
      })
    })
  })
  
  
  
  #this chunk takes the plots, and generates a new section of the ui, 
  #which has to be dynamic - as the number of plots changes depending on the user
  #input
  
  output$plots_breaks <- renderUI({
    num_plots <- input$breaknumber
    plot_output_list <- lapply(1:num_plots, function(i) {
      plotname <- paste("plot", i, sep = "")
      plotlyOutput(plotname, height = "300px")
    })
    do.call(tagList, plot_output_list)
  })
  
  
  
  #this chunk is to plot the line graph of the normalized values across the mortality values
  
  linebreak <- reactive({
    breaknorm() 
  })
  
  output$line_breaks <- renderPlotly({
    data <- linebreak()
    sims <- breaks()
    
    data <- data%>%
      left_join(sims, by="sim")
    
    p <- ggplot(data, aes(x = mort, y = normalized_value, color = Species)) +
      geom_line() +
      geom_point() +
      labs(
        title = "Normalized Value Across Simulations",
        x = "% Mortality Added",
        y = "Normalized Value (%)",
        color = "Species"
      ) +
      theme_minimal()
  })
  
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    .btn-info .fa-info-circle {
  all: unset; !important/* Remove all styles */
  font-size: 16px; !important/* Reapply necessary styles */
  color: white; /* Ensure the icon color */
  display: inline-block; /* Ensure it is displayed */
  line-height: normal; /* Reset line-height */
}
    "))
    #OKAY, IF THE CONTENT OF THE BUTTON CONTAINS ANY . / () , IT WONT WORK!!.

  ),
  page_navbar(
    title = tagList(
      img(src = "mizer.png", height = "75px", style = "vertical-align: middle; margin-right: 15px;"), # Image from the www folder
      "Celtic Sea Mizer Model"
    ),
    selected = "Species",
    collapsible = TRUE,
    theme = bs_theme(bootswatch="cerulean"),
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
                  label = HTML("Starting Biomass %     <button id='infoButtonSpecies' class='btn btn-info btn-xs' type='button'><i class='fa fa-info-circle' style='font-size: 16px;'></i></button>"),
                  min = 0,
                  max = 2,
                  value = 1,
                  step = 0.01,
                  width = "100%"
                ),
                sliderInput(
                  inputId = "year",
                  label = "Time Range",
                  min = 0,
                  max = 100,
                  value = c(1,1),
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
                actionButton(inputId = "goButton1", label = "Run Simulation"),
                selectInput(
                  inputId = "species_order",
                  label = "Order of Species:",
                  choices = c("Alphabetical","Size","Guild")
                )
              )
            ),
            
            
            # Main Panel for Biomass
            grid_card(
              area = "area0",
              card_body(
                tabsetPanel(
                  tabPanel(title = "Change in Species", plotlyOutput("speciesPlot")),
                  tabPanel(title = "Change in Size", plotlyOutput("sizePlot")),
                  tabPanel(title = "Guilds", plotlyOutput("guildPlot")),
                  tabPanel(title = "Diet", plotlyOutput("dietPlot"))
                )
              )
            )
          )
        ),

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
              inputId = "breakrange",
              label = "% Mortality Added",
              min = -0.5,
              max = 0.5,
              value = c(0,0),
              step = 0.01,
              width = "100%"
            ),
            sliderInput(
              inputId = "breakyear",
              label = "Year to Analyse",
              min = 0,
              max = 100,
              value = 1,
              step = 1,
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
            actionButton(inputId = "breakset_year_5", label = "5 Years", class = "btn-small"),
            actionButton(inputId = "breakset_year_15", label = "15 Years", class = "btn-small"),
            actionButton(inputId = "breakset_year_30", label = "30 Years", class = "btn-small"),
            actionButton(inputId = "goButton", label = "Run Simulation")
          )
        ),
        
        # Main Panel for Breakpoint
        grid_card(
          area = "area0",
          card_body(
            tabsetPanel(
              tabPanel(title = "Scrollable Species", uiOutput("plots_breaks")),
              tabPanel(title = "Line Graph", plotlyOutput("line_breaks"))
              
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
              tabPanel(title = "Yield", plotlyOutput("yieldPlot")),
              tabPanel(title = "Spectra", plotlyOutput("spectrumPlot"))
            )
          )
        )
      )
    )
  )
)

shinyApp(ui = ui, server = server)
