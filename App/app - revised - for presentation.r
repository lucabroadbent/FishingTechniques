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
library(tidyverse)
#questions to ask:
#yield - compare it to what - easy to plot, but what can we plot to show?
#radar plot - goes from 0-100 on all - it doesnt work
#


# Load the mizer model

#setwd("C:/Users/lucab/Downloads")
#celticsim <- readRDS("Celtic_16_untuned.rds")

# Define the UI

# Define the server
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
    #this is the same but instead for the mortality decrease.
    observeEvent(input$mortset_year_5, {
      updateSliderInput(session, "mortyear", value = c(5, 5))
    })
    observeEvent(input$mortset_year_15, {
      updateSliderInput(session, "mortyear", value = c(15, 15))
    })
    observeEvent(input$mortset_year_30, {
      updateSliderInput(session, "mortyear", value = c(30, 30))
    })
    #this is the same for the breakpoint 
    observeEvent(input$breakset_year_5, {
      updateSliderInput(session, "breakyear", value = 5)
    })
    observeEvent(input$breakset_year_15, {
      updateSliderInput(session, "breakyear", value = 15)
    })
    observeEvent(input$breakset_year_30, {
      updateSliderInput(session, "breakyear", value = 30)
    })
  
    #Decrease Herring Abundance 
    specieschange <- eventReactive(input$goButton1,{
        speciessim <- celticsim
        unharvestedprojection <- project(celticsim,
                                         effort = c(commercial = 0, pelagic = 1, beam = 1, otter = 1),
                                         t_max = input$year[2]*2)
        unharvested <- plotSpectra(unharvestedprojection, time_range = input$year[1]:input$year[2], return_data = TRUE)
        unharvestedbio <- getBiomass(unharvestedprojection)
        unharvestedbio <- melt(unharvestedbio[input$year[1],])
        #unharvestedbio <- melt(apply(unharvestedbio, 2, mean, na.rm = TRUE))
        unharvestedbio <- rownames_to_column(unharvestedbio, var = "Species")
        
        speciessim@initial_n[input$species_name_select, ] <- speciessim@initial_n[input$species_name_select, ] * input$species
        harvestedprojection <- project(speciessim,
                                       effort = c(commercial = 0, pelagic = 1, beam = 1, otter = 1),
                                       t_max = input$year[2]*2)
        harvested <- plotSpectra(harvestedprojection, time_range = input$year[1]:input$year[2], return_data = TRUE)
        harvestedbio <- getBiomass(harvestedprojection)
        harvestedbio <- melt(harvestedbio[input$year[1],])
        #harvestedbio <- melt(apply(harvestedbio, 2, mean, na.rm = TRUE))
        harvestedbio <- rownames_to_column(harvestedbio, var = "Species")
        
        harvested2 <- harvested
        unharvested2 <- unharvested
        harvested3 <- harvested
        unharvested3 <- unharvested
        
        # This next function separates the size spectrum into bins
        #so that the effect of changing one species is observed on a community level
        create_log_bins <- function(data, column, bins = 10) {
          # Calculate logarithmically spaced breaks
          breaks <- exp(seq(log(min(data[[column]])), log(max(data[[column]])), length.out = bins + 1))
          
          # Bin the data
          data <- data %>%
            mutate(log_bin = cut(data[[column]], breaks = breaks, include.lowest = TRUE))%>%
            group_by(log_bin)%>%
            summarise(value =  mean(percentage_diff)) %>%
            mutate(
              lower_bound = breaks[-length(breaks)][as.numeric(log_bin)],
              upper_bound = breaks[-1][as.numeric(log_bin)]
            )%>%
            mutate(midpoint = (lower_bound + upper_bound) / 2)
          
          return(data)
        }
        
        #arvested <- harvested%>%group_by(w)%>%
         # summarise(value=mean(value))
        #unharvested <- unharvested2%>%group_by(w)%>%
         # summarise(value=mean(value))
        
        #percentagediff <- harvested%>%
          #left_join(unharvested, by = "w")%>%
         #mutate(percentage_diff = ((value.x / value.y ) * 100)-100)
        
        #percentage_diffbinned <- create_log_bins(percentagediff, "w", bins=10)
        
        # Run the function on the harvested and unharvested data
        #innedharvested <- create_log_bins(harvested, "w", bins = 10)
        #binnedunharvested <- create_log_bins(unharvested, "w", bins = 10)
        # Average the values in each bin
        #binnedharvested <- binnedharvested %>%
        #  group_by(avg_weight) %>%
        #  summarise(value = mean(value))
        #binnedunharvested <- binnedunharvested %>%
        #  group_by(avg_weight) %>%
        #  summarise(value = mean(value))
        # Calculate the percentage change in each bin
        #percentage_diffbinned <- binnedharvested %>%
         # left_join(binnedunharvested, by = "log_bin") %>%
          #mutate(percentage_diff = ((value.x / value.y) * 100)-100)
        
        #now i am calculating the community spectrum to plot.
        
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
             
          return(sf)
        }
        
        sf <- plotSpectraRelative(harvestedprojection, unharvestedprojection)
  
        sizelevel <- ggplot() +
          #geom_rect(data = percentage_diffbinned, 
               ##     aes(xmin = log(lower_bound), xmax = log(upper_bound), 
                #        ymin = 0, ymax = value), 
                #    fill = "#2FA4E7") +
          geom_line(data = sf, 
                    aes(x = w, y = rel_diff*100), 
                    color = "#2FA4E7") +
          geom_hline(yintercept = 0, linetype = 1,
                     colour = "dark grey", linewidth = 0.75)+
          labs(title = "Percentage Change by Size", x = "Size (g)", y = "Percentage Change") +
          theme_minimal() +
          theme(axis.text.x = element_text(size = 14, hjust = 1, vjust = 0.5),
                axis.text.y = element_text(size = 14),
                legend.position = "none",
                axis.title.x = element_text(size = 16),
                axis.title.y = element_text(size = 16))

        # Group by Species and calculate the average value of the value column for each species

        percentage_diff <- harvestedbio %>%
            left_join(unharvestedbio, by = "Species") %>%
            mutate(percentage_diff = (percentage_diff = ((value.x - value.y) / value.y) * 100)) %>%
            select(Species, percentage_diff)%>%
          filter(!Species %in% c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"))
        percentage_diff$class <- "chosen"
        
        
        #here i am plotting the species level change across years
        #so harvested bio is the correct one, i need to calculate these years diff.
        

        unharvestedbiotriple <- getBiomass(unharvestedprojection)
        lowunbiotrip <- melt(unharvestedbiotriple[round(input$year[1]*(1/3)),])
        highunbiotrip <- melt(unharvestedbiotriple[input$year[1]*2,])
        lowunbiotrip$Species <- rownames(lowunbiotrip)
        highunbiotrip$Species <- rownames(highunbiotrip)
        
        
        harvestedbiotriple <- getBiomass(harvestedprojection)
        lowbiotrip <- melt(harvestedbiotriple[round(input$year[1]*(1/3)),])
        highbiotrip <- melt(harvestedbiotriple[input$year[1]*2,])
        lowbiotrip$Species <- rownames(lowbiotrip)
        highbiotrip$Species <- rownames(highbiotrip)
        
        percentage_difflow <- lowbiotrip %>%
          left_join(lowunbiotrip, by = "Species") %>%
          mutate(percentage_diff = (percentage_diff = ((value.x - value.y) / value.y) * 100)) %>%
          select(Species, percentage_diff)%>%
          filter(!Species %in% c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"))
        percentage_difflow$class <- "low"
        
        percentage_diffhigh <- highbiotrip %>%
          left_join(highunbiotrip, by = "Species") %>%
          mutate(percentage_diff = (percentage_diff = ((value.x - value.y) / value.y) * 100)) %>%
          select(Species, percentage_diff)%>%
          filter(!Species %in% c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"))
        percentage_diffhigh$class <- "high"
        
        percentage_diff <- rbind(percentage_diff, percentage_difflow, percentage_diffhigh)
        
        
        specieslevel <- ggplot(percentage_diff, aes(x = Species, y = percentage_diff, fill=factor(class))) +
            geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
            labs(title = "Percentage Change by Species", x = "Species", y = "Percentage Change") +
          scale_fill_manual(values =  c("#2FA4E7", "#2FA4E7cc", "#2FA4E799")) +
          theme_minimal() +
          theme(axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = 0.5),
                axis.text.y = element_text(size = 14),
                legend.position = "none",
                axis.title.x = element_text(size = 16),
                axis.title.y = element_text(size = 16))

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
        guildlevel <- ggplot(percentage_diffguilds, aes(x = Guild, y = percentage_diff, fill = factor(size_category))) +
          geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
          scale_fill_manual(values =  c("#2FA4E7", "#2FA4E7cc", "#2FA4E799")) +
          labs(title = "Percentage Change by Guild", x = "Size Category", y = "Percentage Change") +
          theme_minimal() +
          theme(axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust = 0.5),
                axis.text.y = element_text(size = 14),
                legend.position = "none",
                axis.title.x = element_text(size = 16),
                axis.title.y = element_text(size = 16))
        

        list(sizelevel = sizelevel, specieslevel = specieslevel, guildlevel = guildlevel)
    })
    #Plots of herring decrease
    output$speciesPlot <- renderPlotly({
        specieschange()$specieslevel
    })
    output$sizePlot <- renderPlotly({
        specieschange()$sizelevel
    })
    output$guildPlot <- renderPlotly({
        specieschange()$guildlevel
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
    mortspecieschange <- eventReactive(input$goButton3,{
      speciessim <- celticsim
      
      unharvestedprojection <- project(celticsim,
                                       effort = c(commercial = 0, pelagic = 1, beam = 1, otter = 1),
                                       t_max = input$mortyear[2]*2)
      unharvested <- plotSpectra(unharvestedprojection, time_range = input$mortyear[1]:input$mortyear[2], return_data = TRUE)
      unharvestedbio <- getBiomass(unharvestedprojection)
      unharvestedbio <- melt(unharvestedbio[input$mortyear[1],])
      #unharvestedbio <- melt(apply(unharvestedbio, 2, mean, na.rm = TRUE))
      unharvestedbio <- rownames_to_column(unharvestedbio, var = "Species")
      
      test <- getExtMort(speciessim)
      totalmort <- getMort(speciessim)
      
      test[input$name_select,] <- test[input$name_select,]+(input$mortspecies*totalmort[input$name_select,])
      ext_mort(speciessim) <- test
      
      harvestedprojection <- project(speciessim,
                                     effort = c(commercial = 0, pelagic = 1, beam = 1, otter = 1),
                                     t_max = input$mortyear[2]*2)
    
      harvested <- plotSpectra(harvestedprojection, time_range = input$mortyear[1]:input$mortyear[2], return_data = TRUE)
      harvestedbio <- getBiomass(harvestedprojection)
      harvestedbio <- melt(harvestedbio[input$mortyear[1],])
      #harvestedbio <- melt(apply(harvestedbio, 2, mean, na.rm = TRUE))
      harvestedbio <- rownames_to_column(harvestedbio, var = "Species")
      
      harvested2 <- harvested
      unharvested2 <- unharvested
      harvested3 <- harvested
      unharvested3 <- unharvested
      
      #now i am calculating the community spectrum to plot.
      
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
        
        return(sf)
      }
      
      sf <- plotSpectraRelative(harvestedprojection, unharvestedprojection)
      
      sizelevel <- ggplot() +
        #geom_rect(data = percentage_diffbinned, 
        ##     aes(xmin = log(lower_bound), xmax = log(upper_bound), 
        #        ymin = 0, ymax = value), 
        #    fill = "#2FA4E7") +
        geom_line(data = sf, 
                  aes(x = w, y = rel_diff*100), 
                  color = "#2FA4E7") +
        geom_hline(yintercept = 0, linetype = 1,
                   colour = "dark grey", linewidth = 0.75)+
        labs(title = "Percentage Change by Size", x = "Size (g)", y = "Percentage Change") +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 14, hjust = 1, vjust = 0.5),
              axis.text.y = element_text(size = 14),
              legend.position = "none",
              axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16))
      
      # Group by Species and calculate the average value of the value column for each species
      
      percentage_diff <- harvestedbio %>%
        left_join(unharvestedbio, by = "Species") %>%
        mutate(percentage_diff = (percentage_diff = ((value.x - value.y) / value.y) * 100)) %>%
        select(Species, percentage_diff)%>%
        filter(!Species %in% c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"))
      percentage_diff$class <- "chosen"
      
      #here i am plotting the species level change across years
      #so harvested bio is the correct one, i need to calculate these years diff.
      
      
      unharvestedbiotriple <- getBiomass(unharvestedprojection)
      lowunbiotrip <- melt(unharvestedbiotriple[round(input$mortyear[1]*(1/3)),])
      highunbiotrip <- melt(unharvestedbiotriple[input$mortyear[1]*2,])
      lowunbiotrip$Species <- rownames(lowunbiotrip)
      highunbiotrip$Species <- rownames(highunbiotrip)
      
      harvestedbiotriple <- getBiomass(harvestedprojection)
      lowbiotrip <- melt(harvestedbiotriple[round(input$mortyear[1]*(1/3)),])
      highbiotrip <- melt(harvestedbiotriple[input$mortyear[1]*2,])
      lowbiotrip$Species <- rownames(lowbiotrip)
      highbiotrip$Species <- rownames(highbiotrip)

      percentage_difflow <- lowbiotrip %>%
        left_join(lowunbiotrip, by = "Species") %>%
        mutate(percentage_diff = (percentage_diff = ((value.x - value.y) / value.y) * 100)) %>%
        select(Species, percentage_diff)%>%
        filter(!Species %in% c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"))
      percentage_difflow$class <- "low"
      
      
      percentage_diffhigh <- highbiotrip %>%
        left_join(highunbiotrip, by = "Species") %>%
        mutate(percentage_diff = (percentage_diff = ((value.x - value.y) / value.y) * 100)) %>%
        select(Species, percentage_diff)%>%
        filter(!Species %in% c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"))
      percentage_diffhigh$class <- "high"
      
      percentage_diff <- rbind(percentage_diff, percentage_difflow, percentage_diffhigh)
      
      
      specieslevel <- ggplot(percentage_diff, aes(x = Species, y = percentage_diff, fill=factor(class))) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
        labs(title = "Percentage Change by Species", x = "Species", y = "Percentage Change") +
        scale_fill_manual(values =  c("#2FA4E7", "#2FA4E7cc", "#2FA4E799")) +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = 0.5),
              axis.text.y = element_text(size = 14),
              legend.position = "none",
              axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16))
      
      
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
      
      # this has been changed from guildlevel <- ggplot(percentage_diffguilds, aes(x = Guild, y = percentage_diff, fill = factor(size_category))) +
      guildlevel <- ggplot(percentage_diffguilds, aes(x = Guild, y = percentage_diff, fill = factor(size_category))) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
        scale_fill_manual(values = c("#2FA4E7", "#2FA4E7cc", "#2FA4E799")) +
        labs(title = "Percentage Change by Guild", x = "Size Category", y = "Percentage Change") +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust = 0.5),
              axis.text.y = element_text(size = 14),
              legend.position = "none",
              axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16))
      
      
      list(sizelevel = sizelevel, specieslevel = specieslevel, guildlevel = guildlevel)
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
    
    #Plotting breakpoints for the mortality increases
    breaks <- eventReactive(input$goButton,{
      req(input$breakrange, input$breaknumber, input$breakyear)
      
      # Generate breakpoints
      breaks <- seq(input$breakrange[1], input$breakrange[2], 
                     by = (input$breakrange[2] - input$breakrange[1]) / (input$breaknumber - 1))
      
      # Create a data frame with time range and mortality breakpoints
      databreak <- data.frame(mort = breaks, time = rep(input$breakyear, length(breaks)), 
                              sim=seq_len(length(breaks)))
      return(databreak)
    })

    #Getting simulations from these mortality rates.
    breaksim <- eventReactive(input$goButton,{
      breakpoints <- breaks()
      breaksim <- data.frame()
      #read in species and get the mortality rates
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
       #harvestedbio <- (melt(apply(harvestedbio, 1, mean, na.rm = TRUE)))
       harvestedbio <- rownames_to_column(harvestedbio, var="Species")
       harvestedbio$sim <- i
        
       breaksim <- rbind(breaksim, harvestedbio)
      
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
      #unharvested <- plotBiomass(unharvestedprojection, time_range = breakpoints$time[1]:breakpoints$time[1], return_data = TRUE)
      unharvestedbio <- getBiomass(unharvestedprojection)
      unharvestedbio <- melt(unharvestedbio[breakpoints$time[1]:breakpoints$time[1],])
      #unharvestedbio <- (melt(apply(unharvestedbio, 2, mean, na.rm = TRUE)))
      unharvestedbio <- rownames_to_column(unharvestedbio, var="Species")
      #unharvested <- unharvested %>%
       # group_by(Species)%>%
        #summarise(mean_value = mean(value, na.rm = TRUE))
      
      #sims <- sims %>%
       # group_by(Species, sim)%>%
        #summarise(mean_value = mean(value, na.rm = TRUE))
      
      normalized_data <- sims %>%
        inner_join(unharvestedbio, by = "Species")%>%
        mutate(normalized_value = ((value.x / value.y)-1)*100) %>%
        select(Species, normalized_value, sim) %>%
        filter(!Species %in% c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"))
      return(normalized_data)
      
    })
    
    #now plotting this data
    #first need to create a function for it.
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

    
    #now plotting these plots.
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
  
    #now creating the UI for the plots
    output$plots_breaks <- renderUI({
      num_plots <- input$breaknumber
      plot_output_list <- lapply(1:num_plots, function(i) {
        plotname <- paste("plot", i, sep = "")
        plotlyOutput(plotname, height = "300px")
      })
      do.call(tagList, plot_output_list)
    })
    
    #now i am plotting them as a line graph, lets see.
    
    
    linebreak <- reactive({
      breaknorm() # This should return a data frame with columns: Species, normalized_value, sim
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
    
    
    
    #now I am trying to make plots but for the size spectrum 
}
  
ui <- fluidPage(tags$head(
    tags$style(HTML("
      .btn-small {
        padding: 5px 10px;
        font-size: 12px;
        border-radius: 4px;
      }
      .nav-tabs .nav-link.active, .nav-tabs .nav-item.show .nav-link {
        color: #ffffff;
        background-color: #2FA4E7;
        border-color: #2FA4E7 #2FA4E7 #2FA4E7;
      }
      .nav-tabs .nav-link {
        color: #2FA4E7;
        border: 1px solid transparent;
        border-top-left-radius: .25rem;
        border-top-right-radius: .25rem;
      }
      .nav-tabs .nav-link:hover {
        border-color: #e9ecef #e9ecef #ddd;
        color: #0056b3;
      }
      .card {
        border: 2px solid #2FA4E7;
        border-radius: .5rem;
      }
      .nav-tabs {
        margin-bottom: 30px;
      }
      .plots-container {
        padding-top: 30px;
      }
    "))
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
                label = "Starting Biomass %:",
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
                tabPanel(title = "Change in Species", plotlyOutput("speciesPlot")),
                tabPanel(title = "Change in Size", plotlyOutput("sizePlot")),
                tabPanel(title = "Guilds", plotlyOutput("guildPlot"))
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

          grid_card(
            area = "area1",
            card_body(
              sliderInput(
                inputId = "mortspecies",
                label = "% Mortality Added",
                min = -0.5,
                max = 0.5,
                value = 0,
                step = 0.01,
                width = "100%"
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
              selectInput(
                inputId = "name_select",
                label = "Select a Species:",
                choices = c("Herring", "Sprat", 
                            "Cod", "Haddock", "Whiting", "Blue whiting", "Norway Pout", "Poor Cod", 
                            "European Hake", "Monkfish", "Horse Mackerel", "Mackerel", "Common Dab", 
                            "Plaice", "Megrim", "Sole")
              ),
              actionButton(inputId = "mortset_year_5", label = "5 Years", class = "btn-small"),
              actionButton(inputId = "mortset_year_15", label = "15 Years", class = "btn-small"),
              actionButton(inputId = "mortset_year_30", label = "30 Years", class = "btn-small"),
              actionButton(inputId = "goButton3", label = "Run Simulation")
            )
          ),
          
          # Main Panel for Mortality
          grid_card(
            area = "area0",
            card_body(
              tabsetPanel(
                tabPanel(title = "Change in Species", plotlyOutput("mortspeciesPlot")),
                tabPanel(title = "Change in Size", plotlyOutput("mortsizePlot")),
                tabPanel(title = "Guilds", plotlyOutput("mortguildPlot"))
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
  