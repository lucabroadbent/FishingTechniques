library(shiny)
library(mizer)
library(ggplot2)
library(dplyr)
library(plotly)
library(styler)
# Load the mizer model
setwd("C:/Users/lucab/OneDrive/Desktop/Shiny")
celticsim <- readRDS("Celtic_16_untuned.rds")

style_file("C:/Users/lucab/OneDrive/Desktop/Shiny/CelticSeaApp.r")

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
        )
    )
)

# Define the server
server <- function(input, output) {
    # Reactive function to calculate the abundance size spectrum and yield size spectrum based on the fishing effort.
    spectra <- reactive({
        effort <- c(commercial = input$industrial, pelagic = input$pelagic, beam = input$beam, otter = input$otter)
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

    # This is for the initial abundance of all species
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

    output$speciesPlot <- renderPlot({
        specieschange()$specieslevel
    })
    output$sizePlot <- renderPlot({
        specieschange()$sizelevel
    })

    output$guildPlot <- renderPlot({
        specieschange()$guildlevel
    })

}

# Run the app
shinyApp(ui = ui, server = server)



test<-project(celticsim, effort = c(commercial = 0, pelagic = 0, beam = 0, otter = 0))

unharvested <- plotSpectra(test, return_data = TRUE)

unharvestedpisco <- unharvested %>%
            filter(Species %in%
               c("Cod", "Haddock", "Whiting", "European Hake",
             "Monkfish", "Horse Mackerel", "Megrim")) %>%
            summarise(Value = mean(value)) %>%
            mutate(size_category = cut(log10(unharvested$w),
                   breaks = quantile(log10(unharvested$w), probs = seq(0, 1, by = 1 / 3), na.rm = TRUE),
                   labels = c("small", "medium", "large"),
                   include.lowest = TRUE),
               Guild = "Piscovorous")
unharvestedpisco <- unharvested %>%
  filter(Species %in% c("Cod", "Haddock", "Whiting", "European Hake", "Monkfish", "Horse Mackerel", "Megrim")) %>%
  mutate(log_w = log10(w),
         size_category = cut(log_w,
                             breaks = quantile(log_w, probs = seq(0, 1, by = 1 / 3), na.rm = TRUE),
                             labels = c("small", "medium", "large"),
                             include.lowest = TRUE)) %>%
    group_by(size_category) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), Guild = "Piscovorous")