#load in required packages
library(patchwork)
library(cowplot)
library(RColorBrewer)
library(ggplot2)
library(flextable)
library(dplyr)
library(mgcv)

##### plotting the methods example plots ####

##### plotting the sampling figure

#function for generating points across a line

generate_points_along_line <- function(line_func, num_points) {
  x_values <- seq(0, 3, length.out = num_points)
  y_values <- line_func(1/3*x_values)
  return(data.frame(x = x_values, y = y_values))
}


#generate the line
x <- seq(0, 3, length.out = 100)
y <- -x + 1
#generate the points on the line

line_func <- function(x) -x+1
points_df <- generate_points_along_line(line_func, 20)
points_df$c <- c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4)
line_df <- data.frame(x = x, y = y)
#now plotting the line graph and making it all nice
ggplot()+
  geom_abline(slope = -1/3, intercept = 1, color = "grey", size=1, linetype="dashed")+
  geom_point(data = points_df, aes(x = x, y = y), color = "black", size=2)+
  geom_text(data=points_df, aes(x=x, y=y, label = seq_along(x), color=as.factor(c)), size = 3, position = position_nudge(y = 0.09)) +
  scale_color_manual(values=c("#8c510a","#d8b365", "#998ec3","#542788"))+
  scale_y_continuous(breaks = seq(0, 1.2, by = 0.2))+
  xlim(0, 3) +
  #ylim(0, 1.2) +
  xlab("F")+
  ylab("S")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        legend.position = "none")+
  coord_cartesian(ylim = c(0, 1.1))

##### end ####

#### plotting table 1 ####

#this plots the table.
data <- c("1,2,3,4,5", "6,7,8,9,10", "11,12,13,14,15", "16,17,18,19,20")
average_fishing <- c("(0.32, 0.9)", "(1.11,0.63)", "(1.89, 0.37)", "(2.68 ,0.11)")
sample_group <- c(1, 2, 3, 4)

table <- data.frame(
  data,sample_group,
  matrix(unlist(strsplit(gsub("[()]", "", average_fishing), ",")), nrow=length(average_fishing),
         byrow=TRUE))
colnames(table) <- c("Sample Number", "Sample Group", "S", "F")


flextable(table)

#### end ####

#### plotting the Figure 3. ####

#reading in the data
strategies_df <- read.csv("fishcarbon_metrics_9sp.csv")

#now i am adding in the residuals. This next section of code is taken from Falciani et al. and is not mine,

model1 <- gam(formula = strategies_df$B ~ s(strategies_df$Y, bs = "cs"), method = "REML")
strategies_df$residuals <- residuals(model1, type = "deviance")

##after this it is my code.

#this function creates adds the colors, and adds the low residual colors.
fill_color <- function(x) {
  ifelse(x >= -0.02 & x <= 0.02, "grey", colorRampPalette(brewer.pal(9, "Blues"))(40)[findInterval(x, seq(-0.1, 0.1, length.out = 40))])
}


strategies_df$fill_color <- sapply(strategies_df$residuals, fill_color)

 ggplot(strategies_df, aes(x=Y, y=B, color=fill_color)) +
  geom_point() +
  geom_smooth(method = 'gam', color = "black", se = FALSE) +
  #scale_color_gradientn("Residuals", colors = colors1, values = values1) +
  scale_x_continuous(name = expression("Yield"), breaks = seq(0, 2, .2), expand = c(0, 0)) +
  scale_y_continuous(name = expression("Relative Biomass"), breaks = seq(0, 2, .2)) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "gray50", barwidth = 0.5, title.hjust = 5)) +
  theme_bw() +scale_color_identity()+
   theme(axis.text = element_text(size=10, color="black"))

#### end ####

#### plotting the residuals data - Figure 4 ####

#this is the function that gets takes the average from around a fishing strategy
#to make it work properly, you change lines in to subset the data
#for the small/medium/large fishery. All 3 lines to add in are included in the function
#you just have to remove the ### on the fishery you want the data for, and add # to the fisheries you dont want
#it just changes which part of the data is subsetted to average from
generate_residuals_coordinates_data <- function(num_coordinates, percentage) {

  x <- seq(0, 3, length.out = num_coordinates)
  y <- seq(0, 1, length.out = num_coordinates)

  coordinates_df <- expand.grid(x = x, y = y)

  results_df <- data.frame(row_index = numeric(), x = numeric(), y = numeric(), p_value_col1 = numeric(), p_value_col2 = numeric(),
                           p_value_col3 = numeric(), p_value_col4 = numeric())

  perform_lm_on_column <- function(data) {
    result <- mean(data$residuals)
    return(result)
  }

  results_list <- list()
  for (i in 1:nrow(coordinates_df)) {

    x <- coordinates_df[i, "x"]
    y <- coordinates_df[i, "y"]

    x_range <- c(x - percentage * 3, x + percentage * 3)
    y_range <- c(y - percentage * 1, y + percentage * 1)

    #subset_df <- subset(distributeddata, F_small >= x_range[1] & x_range[2] >= F_small &
    #                      S_small >= y_range[1] & y_range[2] >= S_small)
    subset_df <- subset(distributeddata, F_med >= x_range[1] & x_range[2] >= F_med &
                          S_med >= y_range[1] & y_range[2] >= S_med)
    #subset_df <- subset(distributeddata, F_large >= x_range[1] & x_range[2] >= F_large &
    #                      S_large >= y_range[1] & y_range[2] >= S_large)

    lm_results <- perform_lm_on_column(subset_df)

    results_list[[i]] <- c(i, x, y, lm_results)
    results_df <- do.call(rbind, results_list)
    colnames(results_df) <- c("number", "X", "Y", "residuals")
  }

  return(as.data.frame(results_df))
}

#This is then individually run 3 times and saved as results[FISHERY]residuals, where FISHERY is SMALL,MED,LARGE
test <- generate_residuals_coordinates_data(30,0.1)
write.csv(test, file = "resultMEDresiduals", row.names = FALSE)

#i have the data now, i jst need to plot it nice

#this function just adds in when the range I want to be grey
fill_color <- function(x) {
  ifelse(x >= -0.02 & x <= 0.02, "grey", colorRampPalette(brewer.pal(9, "Blues"))(40)[findInterval(x, seq(-0.1, 0.1, length.out = 40))])
}

#this is for the large fishery, first reading in the data, then adding in the grey, then making nice plot
test <- read.csv("resultLARGEresiduals")
test$fill_color <- sapply(test$residuals, fill_color)
plot1 <- ggplot(test, aes(x = X, y = Y, fill = fill_color)) +
  geom_tile() +
  labs(title = "Large",
       x = "F", y = "S", fill = "Residuals") +
  scale_fill_identity() +
  theme_classic() +
  theme(axis.text = element_text(size = 11, color="black"),
        axis.ticks = element_line(size = 1, lineend = "round"),
        axis.title = element_text(size = 11),
        panel.grid.major = element_blank(),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 3, by = 0.5), labels = c("0","0.5", "1", "1.5", "2", "2.5", "3")) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), labels = c("0","0.25", "0.5", "0.75", "1"))

#repeated for the medium fishery
test <- read.csv("resultMEDresiduals")
test$fill_color <- sapply(test$residuals, fill_color)
plot2 <- ggplot(test, aes(x = X, y = Y, fill = fill_color)) +
  geom_tile() +
  labs(title = "Medium",
       x = "F", y = "S", fill = "Residuals") +
  scale_fill_identity() +
  theme_classic() +
  theme(axis.text = element_text(size = 11, color="black"),
        axis.ticks = element_line(size = 1, lineend = "round"),
        axis.title = element_text(size = 11),
        panel.grid.major = element_blank(),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 3, by = 0.5), labels = c("0","0.5", "1", "1.5", "2", "2.5", "3")) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), labels = c("0","0.25", "0.5", "0.75", "1"))

#repeated for the small fishery
test <- read.csv("resultSMALLresiduals")
test$fill_color <- sapply(test$residuals, fill_color)
plot3 <- ggplot(test, aes(x = X, y = Y, fill = fill_color)) +
  geom_tile() +
  labs(title = "Small",
       x = "F", y = "S", fill = "Residuals") +
  scale_fill_identity() +
  theme_classic() +
  theme(axis.text = element_text(size = 11, color="black"),
        axis.ticks = element_line(size = 1, lineend = "round"),
        axis.title = element_text(size = 11),
        panel.grid.major = element_blank(),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(size = 0.5)) +
  scale_x_continuous(breaks = seq(0, 3, by = 0.5), labels = c("0","0.5", "1", "1.5", "2", "2.5", "3")) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), labels = c("0","0.25", "0.5", "0.75", "1"))

#to make a scale i need to make a separate plot and add it.

fake_data <- data.frame(z = seq(-0.1, 0.1, length.out = 100))
fake_data$fill_color <- sapply(fake_data$z, fill_color)
fake_plot <- ggplot(fake_data, aes(x = 1, y = z, fill = fill_color)) +
  geom_tile() +
  scale_fill_identity() +
  theme_void() +
  theme(legend.position = "right", plot.margin = unit(c(0, 0, 0, 0), "lines"), plot.title=element_text(size=11)) +
  annotate("text", x = c(2.3, 2.18,2.4 ,2.3), y = c(-0.08, 0.024,-0.014, 0.09),#-0.013, 0.015,
  label = c(" -0.1", "  +/-", "0.02", " 0.1"),size = 4, color = "black") +#"  -0.02", "  0.02",
  ggtitle("Residual")+
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0))
#so this makes a fake plot that works as a figure legend

#this is for combining and plotting all of the plots
plot12 <- plot1
plot22 <- plot2 + theme(plot.margin = unit(c(0,30,0,0), "pt"))
plot32 <- plot3 +  theme(plot.margin = unit(c(0,30,0,0), "pt"))

(plot32 + plot22+ plot12+ plot_spacer()+inset_element(fake_plot,left = 0,bottom = 0.2,
   right = unit(0.2, 'npc'),top = unit(.8, 'npc'))+
   plot_layout(guides = "collect", axes = "collect_y", nrow=1))


#### end ####

#### plotting the sampling stuff across the line - Figure 5####
#this is the line and the data
line_func <- function(x) -x+1
df <- strategies_df

#this is to average the points around the line (i didn't have to do this, but I didn't realise till after)
#I could have just assumed the values.
#this works to make the linepoints dataframe for the alrge fishery
generate_and_average_pointslarge <- function(line_func, num_points, sampling_range) {

  generate_points_along_line <- function(line_func, num_points) {
    x_values <- seq(0, 3, length.out = num_points)
    y_values <- line_func(1/3*x_values)
    return(data.frame(x = x_values, y = y_values))
  }

  points_df <- generate_points_along_line(line_func, num_points)

  calculate_average <- function(points_df, sampling_range) {
    averaged_df <- data.frame(x = numeric(), y = numeric())

    for (i in 1:nrow(points_df)) {
      point <- points_df[i, ]  # Get the current point

      x_range <- c(point[1] - 3*(sampling_range), point[1] + 3*(sampling_range))
      y_range <- c(point[2] - sampling_range, point[2] + sampling_range)

      Mframe <- subset(strategies_df, S_large>=y_range[1] & y_range[2]>=S_large & F_large>=x_range[1] & x_range[2]>=F_large)
      avMframe <- unlist(colMeans(Mframe, na.rm = TRUE))
      avMframe <- matrix(avMframe, ncol=22)

      averaged_df <- rbind(averaged_df, avMframe)
    }

    return(averaged_df)
  }
  calculate_average(points_df, sampling_range)
}

#the same code but to make the linepoints for the medium fishery
generate_and_average_pointsmed <- function(line_func, num_points, sampling_range) {

  generate_points_along_line <- function(line_func, num_points) {
    x_values <- seq(0, 3, length.out = num_points)
    y_values <- line_func(1/3*x_values)
    return(data.frame(x = x_values, y = y_values))
  }

  points_df <- generate_points_along_line(line_func, num_points)

  calculate_average <- function(points_df, sampling_range) {

    averaged_df <- data.frame(x = numeric(), y = numeric())

    for (i in 1:nrow(points_df)) {
      point <- points_df[i, ]  # Get the current point

      x_range <- c(point[1] - 3*(sampling_range), point[1] + 3*(sampling_range))
      y_range <- c(point[2] - sampling_range, point[2] + sampling_range)

      Mframe <- subset(strategies_df, S_med>=y_range[1] & y_range[2]>=S_med & F_med>=x_range[1] & x_range[2]>=F_med)
      avMframe <- unlist(colMeans(Mframe, na.rm = TRUE))
      avMframe <- matrix(avMframe, ncol=22)

      averaged_df <- rbind(averaged_df, avMframe)
    }

    return(averaged_df)
  }
  calculate_average(points_df, sampling_range)
}

#now this is function for the small fishery. it is the same
generate_and_average_pointssmall <- function(line_func, num_points, sampling_range) {

  generate_points_along_line <- function(line_func, num_points) {
    x_values <- seq(0, 3, length.out = num_points)
    y_values <- line_func(1/3*x_values)
    return(data.frame(x = x_values, y = y_values))
  }

  points_df <- generate_points_along_line(line_func, num_points)

  calculate_average <- function(points_df, sampling_range) {

    averaged_df <- data.frame(x = numeric(), y = numeric())

    for (i in 1:nrow(points_df)) {
      point <- points_df[i, ]  # Get the current point

      x_range <- c(point[1] - 3*(sampling_range), point[1] + 3*(sampling_range))
      y_range <- c(point[2] - sampling_range, point[2] + sampling_range)

      Mframe <- subset(strategies_df, S_small>=y_range[1] & y_range[2]>=S_small & F_small>=x_range[1] & x_range[2]>=F_small)
      avMframe <- unlist(colMeans(Mframe, na.rm = TRUE))
      avMframe <- matrix(avMframe, ncol=22)

      averaged_df <- rbind(averaged_df, avMframe)
    }

    return(averaged_df)
  }
  calculate_average(points_df, sampling_range)
}

#now i am using the functions to generate the dataframes of the points in the fisheries.
# to do it you have to reread in the strategies_df data
strategies_df <- read.csv("fishcarbon_metrics_9sp.csv")

linepoints18large <- generate_and_average_pointslarge(line_func, 20, 0.05)
colnames(linepoints18large) <-colnames(strategies_df)[1:22]

linepoints18med <- generate_and_average_pointsmed(line_func, 20, 0.05)
colnames(linepoints18med) <-colnames(strategies_df)[1:22]

linepoints18small <- generate_and_average_pointssmall(line_func, 20, 0.05)
colnames(linepoints18small) <-colnames(strategies_df)[1:22]



#now we have 3 linepoints dataframes linepoints18large, linepoints18med, linepoints18small
#these are the fishing strategies that will be run to produce Figure 5.

#this code takes the fishing strategies from the linepoints dataframes and runs them as code and then
#takes the predation mortality for each

#need to make the trait based model now. This code is not mine and is taken from Falciani et al.
params <- newTraitParams(no_sp = 9)

S.list <- rep(.01, 9)

gear_params(params) <- data.frame(species = species_params(params)$species,
                                  gear = c(rep("small", 3), rep("medium", 3), rep("large", 3)),
                                  sel_func = "knife_edge",
                                  sel = rep(S.list),
                                  knife_edge_size = species_params(params)$w_max * rep(S.list))

#now after this point it is my code.

#you put the linepoints dataframe into the function, and getPredMort for the mortrateframe function
mortrateframe <- function(dataframe, getPredMort) {
  results_df <- data.frame(row_index = numeric(), redmort_mean = numeric())

  for (i in 1:nrow(dataframe)) {
    row_values <- dataframe[i, ]
    gear_params(params)$sel <- rep(as.numeric(row_values[5:7]), each = 3)
    gear_params(params)$knife_edge_size <- species_params(params)$w_max * gear_params(params)$sel
    simred <- project(params, t_max = 300, effort = c("small" = row_values$F_small,
                                                      "medium" = row_values$F_med,
                                                      "large" = row_values$F_large))
    redmort <- getPredMort(simred)
    redmort_mean <- colSums(apply(tail(redmort, 100), c(2, 3), mean))
    results_df <- rbind(results_df, data.frame(row_index = i, redmort_mean = redmort_mean))
    #if there is problem it is this
    results_df$rowname <- parse_number(rep(results_df$rowname[1:141], each = nrow(2820)))
  }
  results_df <- rownames_to_column(results_df)
  return(results_df)
}
#this does the same but gets the biomass
spectraframe <- function(dataframe) {
  results_df <- data.frame(row_index = numeric(), redmort_mean = numeric())

  for (i in 1:nrow(dataframe)) {
    row_values <- dataframe[i, ]
    gear_params(params)$sel <- rep(as.numeric(row_values[5:7]), each = 3)
    gear_params(params)$knife_edge_size <- species_params(params)$w_max * gear_params(params)$sel
    simred <- project(params, t_max = 300, effort = c("small" = row_values$F_small,
                                                      "medium" = row_values$F_med,
                                                      "large" = row_values$F_large))
    redmort <- plotSpectra(simred, timerange=200:300, power=1, return_data = TRUE)

    redmort_mean <- aggregate(value~w, redmort, sum)
    #have to do this next bit of code as for the large species it doesnt go across entire spectrum
    #it doesnt go to all 181 size bins, it stops for some.
    n_needed <- 181 - nrow(redmort_mean)
    if (n_needed > 0) {
      zero_rows <- data.frame(w = rep(0, n_needed), value = rep(0, n_needed))
      redmort_mean <- rbind(redmort_mean, zero_rows)
    }
    results_df <- rbind(results_df, data.frame(row_index = i, redmort_mean = redmort_mean))
    #this might be the problem if there is one
  }
  return(results_df)
}
#this does the same but gets the feeding level
feedrateframe <- function(dataframe) {
  results_df <- data.frame(row_index = numeric(), redmort_mean = numeric())

  for (i in 1:nrow(dataframe)) {
    row_values <- dataframe[i, ]
    gear_params(params)$sel <- rep(as.numeric(row_values[5:7]), each = 3)
    gear_params(params)$knife_edge_size <- species_params(params)$w_max * gear_params(params)$sel
    simred <- project(params, t_max = 300, effort = c("small" = row_values$F_small,
                                                      "medium" = row_values$F_med,
                                                      "large" = row_values$F_large))
    redmort <- getFeedingLevel(simred)
    redmort_mean <- as.data.frame(t(colMeans(redmort[201:301, 9, , drop = FALSE])))
    results_df <- rbind(results_df, data.frame(row_index = i, redmort_mean = redmort_mean))
    results_df$rowname <- parse_number(rep(results_df$rowname[1:141], each = nrow(2820)))
  }
  results_df <- rownames_to_column(results_df)
  return(results_df)
}

#the function are all then run on the linepoints dataframes as the dataframe, and saved as csv file.
#they are also run by metrics0, which is the unharvested fishing strategy from the falciani paper.
#so after this I have all my dataframes, of small/med/large/unharvested and their feedrate/biomass/feedinglevel
#and i have the dataframe for the unharvested for feedrate/biomass/feedinglevel
#these dataframes are included, but you can generate here


#next section is plotting all this data
#### plotting data ####

#first you read the data in
testharvest <- read.csv("resultUNHARVEST20,0.0.5+y=x+1Spectra")
test <- read.csv("resultsmall20,0.0.5+y=x+1Spectra")

#now this makes adds in a column that is relative to the unharvested - this is for the biomass plot

test$redmort_mean.value2 <- rep(testharvest$redmort_mean.value[1:181], times=20)
test$divide <- test$redmort_mean.value/test$redmort_mean.value2

#this turns it from 20 samples into 4 sampling groups
test <- test%>%mutate(row_index=(row_index-1)%/%5)%>%
  group_by(row_index,redmort_mean.w)%>%summarise(mean_value=mean(divide))
#this is for the unharvested, so it is relative.
testharvest$redmort_mean.value <- 1

#reading in the data for the predation
testharvestpred <- read.csv("resultUNHARVEST20,0.0.5+y=x+1Pred")
testpred <- read.csv("resultsmall20,0.0.5+y=x+1Pred")

#the format is not correct, this sorts it out to be plotted
#it also adds in the sampling groups
testpred$rowname <- rep(testpred$rowname[1:141], each = nrow(2820))
testpred <- testpred%>%mutate(row_index=(row_index-1)%/%5)%>%
  group_by(row_index,rowname)%>%summarise(mean_value=mean(redmort_mean))

#reading in the data for the feeding rate
testharvestfeed <- read.csv("resultUNHARVESTED20,0.0.5+y=x+1Feed")
testfeed <- read.csv("resultsmall20,0.0.5+y=x+1Feed")

#sorting out the format and adding in the sampling groups
testfeed$rowname <- rep(testfeed$rowname[1:141], each = nrow(2820))
testfeed <- testfeed%>%mutate(row_index=(row_index-1)%/%5)%>%
  group_by(row_index,rowname)%>%summarise(mean_value=mean(X9))

#now we have read all the data in for the small fishery, we can plot it

#this is the asymptotic sizes of the species, which I add to the plots
points <- data.frame(y=0.001, x = c(0.001,6.309573, 15.848932, 39.810717, 100.000000, 251.188643, 630.957344, 1584.893192, 3981.071706, 10000.000000))
#this plots the biomass size spectrum for the small fishery
plot <- ggplot() +
  geom_line(data = subset(test, mean_value >= 0.0055), aes(x = redmort_mean.w, y = mean_value, group = row_index, color = factor(row_index)), size = 1) +
  scale_color_manual(values = c("#8c510a","#d8b365", "#998ec3","#542788"))+
  geom_line(data = testharvest, aes(x = redmort_mean.w, y = redmort_mean.value), color = "red") +
  scale_x_continuous(trans = "log10", limits = c(0.001, 10000), expand = c(0.01, 0.1)) +
  scale_y_continuous(trans="log10", breaks = 10^seq(-2, 1, by = 1), labels = c(0.01,0.1,1,10), expand = c(-0.03, 0.03),
                     limits=c(-20, 17))+ ylab("Relative Biomass") + xlab("Size")  +
  geom_segment(data = points, aes(x = x, xend = x, y = 0.0045, yend = 0.00335), color = "red", size = 1.5, alpha = 0.5) +
  geom_text(data = points, aes(x = x, y = 0.0028, label = 0:9), vjust = 1, size=4)+
  theme_bw()+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),axis.title.x = element_blank(),plot.margin = margin(b = 13, l = 0.5, t = 0, r = 0.5))+
  coord_cartesian(clip = "off")

#this plots the predation size spectrum for the small fishery
plot2 <- ggplot()+
  geom_line(data = testpred, aes(x = as.numeric(rowname), y = mean_value, color = factor(row_index), group=row_index), size = 1)+
  scale_color_manual(values = c("#8c510a","#d8b365", "#998ec3","#542788"))+
  geom_line(data = testharvestpred, aes(x = rowname, y = redmort_mean), color="red")+
  scale_x_continuous(trans="log10",limits = c(0.001, 10000), expand = c(0.01, 0.1))+
  scale_y_continuous(expand = c(0, 10))+ ylab("Predation Rate")+ xlab("Size")+ theme_bw()+
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.title.x = element_blank(),
        plot.margin = margin(b = 0, l = 0.5, t = 0, r = 0.5, unit = "cm"))+
  coord_cartesian(clip = "off")

#this plots the feeding size spectrum for the small fishery

plot3 <- ggplot()+#as.factor(row_index)))+
  geom_line(data = testfeed, aes(x = rowname, y = mean_value, group = row_index, color = factor(row_index)), size = 1)+
  scale_color_manual(values = c("#8c510a","#d8b365", "#998ec3","#542788"))+
  geom_line(data = testharvestfeed, aes(x = rowname, y = X9), color="red")+
  scale_x_continuous(trans="log10", expand = c(0.01, 0.1), limits = c(0.001, 10000))+
  scale_y_continuous(expand = c(0, 0.05), limits=c(0.3686729, 0.8606924))+
  ylab("Feeding Level")+
  xlab("Size (g)")+theme_bw()


#now I do the same for the medium - it is the same code you just read in different files

testharvest <- read.csv("resultUNHARVEST20,0.0.5+y=x+1Spectra")
test <- read.csv("resultmed20,0.0.5+y=x+1Spectra")

test$divide <- test$redmort_mean.value/rep(testharvest$redmort_mean.value[1:181], times=20)
test <- test%>%mutate(row_index=(row_index-1)%/%5)%>%
  group_by(row_index,redmort_mean.w)%>%summarise(mean_value=mean(divide))
testharvest$redmort_mean.value <- 1

#this is necessary to improve interpretability of the graph, as without the line disappears.
test[151,"mean_value"] <- 0.01


testharvestpred <- read.csv("resultUNHARVEST20,0.0.5+y=x+1Pred")
testpred <- read.csv("resultmed20,0.0.5+y=x+1Pred")

testpred$rowname <- rep(testpred$rowname[1:141], each = nrow(2820))#need to do this !!! the stuff is messed up
testpred <- testpred%>%mutate(row_index=(row_index-1)%/%5)%>%
  group_by(row_index,rowname)%>%summarise(mean_value=mean(redmort_mean))

testharvestfeed <- read.csv("resultUNHARVESTED20,0.0.5+y=x+1Feed")
testfeed <- read.csv("resultmed20,0.0.5+y=x+1Feed")

testfeed$rowname <- rep(testfeed$rowname[1:141], each = nrow(2820))
testfeed <- testfeed%>%mutate(row_index=(row_index-1)%/%5)%>%
  group_by(row_index,rowname)%>%summarise(mean_value=mean(X9))

#biomass size spectrum plot
plotm <- ggplot() +
  geom_line(data = subset(test, mean_value >= 0.0055), aes(x = redmort_mean.w, y = mean_value, group = row_index, color = factor(row_index)), size = 1) +
  scale_color_manual(values = c("#8c510a","#d8b365", "#998ec3","#542788"))+
  geom_line(data = testharvest, aes(x = redmort_mean.w, y = redmort_mean.value), color = "red") +
  scale_x_continuous(trans = "log10", limits = c(0.001, 10000), expand = c(0.01, 0.1)) +
  scale_y_continuous(trans="log10", breaks = 10^seq(-10, 1, by = 1), expand = c(-0.03, 0.03),
                     limits=c(-20, 17))+ ylab("Relative Biomass") + xlab("Size")  +
  geom_segment(data = points, aes(x = x, xend = x, y = 0.0045, yend = 0.00335), color = "red", size = 1.5, alpha = 0.5) +
  geom_text(data = points, aes(x = x, y = 0.0028, label = 0:9), vjust = 1, size=4)+
  theme_bw()+theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(b = 13, l = 0.5, t = 0, r = 0.5))+
  coord_cartesian(clip = "off")
#predation medium plot
plot2m <- ggplot()+
  geom_line(data = testpred, aes(x = as.numeric(rowname), y = mean_value, color = factor(row_index), group=row_index), size = 1)+
  scale_color_manual(values = c("#8c510a","#d8b365", "#998ec3","#542788"))+
  geom_line(data = testharvestpred, aes(x = rowname, y = redmort_mean), color="red")+
  scale_x_continuous(trans="log10",limits = c(0.001, 10000), expand = c(0.01, 0.1))+
  scale_y_continuous(expand = c(0, 10), limits=c(0.0000, 144.3435))+
  ylab("Predation")+
  xlab("Size")+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(b = 0, l = 0.5, t = 0, r = 0.5, unit = "cm"))+
  coord_cartesian(clip = "off")
#feeding level medium plot
plot3m <- ggplot()+
  geom_line(data = testfeed, aes(x = rowname, y = mean_value, group = row_index, color = factor(row_index)), size = 1)+
  scale_color_manual(values = c("#8c510a","#d8b365", "#998ec3","#542788"))+
  geom_line(data = testharvestfeed, aes(x = rowname, y = X9), color="red")+
  scale_x_continuous(trans="log10", expand = c(0.01, 0.1), limits = c(0.001, 10000))+
  scale_y_continuous(expand = c(0, 0.05), limits=c(0.3686729, 0.861924))+
  ylab("Feedrate")+
  xlab("Size (g)")+theme_bw()

#this is for the large fishery now
testharvest <- read.csv("resultUNHARVEST20,0.0.5+y=x+1Spectra")
test <- read.csv("resultlarge20,0.0.5+y=x+1Spectra")

test$divide <- test$redmort_mean.value/rep(testharvest$redmort_mean.value[1:181], times=20)
test <- test%>%mutate(row_index=(row_index-1)%/%5)%>%
  group_by(row_index,redmort_mean.w)%>%summarise(mean_value=mean(divide))
testharvest$redmort_mean.value <- 1

testharvestpred <- read.csv("resultUNHARVEST20,0.0.5+y=x+1Pred")
testpred <- read.csv("resultlarge20,0.0.5+y=x+1Pred")

testpred$rowname <- rep(testpred$rowname[1:141], each = nrow(2820))#need to do this !!! the stuff is messed up
testpred <- testpred%>%mutate(row_index=(row_index-1)%/%5)%>%
  group_by(row_index,rowname)%>%summarise(mean_value=mean(redmort_mean))

testharvestfeed <- read.csv("resultUNHARVESTED20,0.0.5+y=x+1Feed")
testfeed <- read.csv("resultlarge20,0.0.5+y=x+1Feed")

testfeed$rowname <- rep(testfeed$rowname[1:141], each = nrow(2820))
testfeed <- testfeed%>%mutate(row_index=(row_index-1)%/%5)%>%
  group_by(row_index,rowname)%>%summarise(mean_value=mean(X9))

#biomass plot large
plotl <- ggplot() +
  geom_line(data = subset(test, mean_value >= 0.0055), aes(x = redmort_mean.w, y = mean_value, group = row_index, color = factor(row_index)), size = 1) +
  scale_color_manual(values = c("#8c510a","#d8b365", "#998ec3","#542788"))+
  geom_line(data = testharvest, aes(x = redmort_mean.w, y = redmort_mean.value), color = "red") +
  scale_x_continuous(trans = "log10", limits = c(0.001, 10000), expand = c(0.01, 0.1)) +
  scale_y_continuous(trans="log10", breaks = 10^seq(-10, 1, by = 1), expand = c(-0.03, 0.03),
                     limits=c(-20, 17))+ ylab("Relative Biomass") + xlab("Size")  +
  geom_segment(data = points, aes(x = x, xend = x, y = 0.0045, yend = 0.00335), color = "red", size = 1.5, alpha = 0.5) +
  geom_text(data = points, aes(x = x, y = 0.0028, label = 0:9), vjust = 1, size=4)+
  theme_bw()+theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(b = 13, l = 0.5, t = 0, r = 0.5))+
  coord_cartesian(clip = "off")


#predation plot large

plot2l <- ggplot()+#as.factor(row_index)))+
  geom_line(data = testpred, aes(x = as.numeric(rowname), y = mean_value, color = factor(row_index), group=row_index), size = 1)+
  scale_color_manual(values =c("#8c510a","#d8b365", "#998ec3","#542788"))+
  geom_line(data = testharvestpred, aes(x = rowname, y = redmort_mean), color="red")+
  scale_x_continuous(trans="log10",limits = c(0.001, 10000), expand = c(0.01, 0.1))+
  scale_y_continuous(expand = c(0, 10),limits = c(0.0000, 144.3435))+
  ylab("Predation")+
  xlab("Size")+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(b = 0, l = 0.5, t = 0, r = 0.5, unit = "cm"))+
  coord_cartesian(clip = "off")

#feeding plot large

plot3l <- ggplot()+
  geom_line(data = testfeed, aes(x = rowname, y = mean_value, group = row_index, color = factor(row_index)), size = 1)+
  scale_color_manual(values = c("#8c510a","#d8b365", "#998ec3","#542788"))+
  geom_line(data = testharvestfeed, aes(x = rowname, y = X9), color="red")+
  scale_x_continuous(trans="log10", expand = c(0.01, 0.1), limits = c(0.001, 10000),
                     labels = c(0.001,0.01,0.1,1,10,100,1000,"10,000"), breaks = 10^seq(-3,4, by = 1))+
  scale_y_continuous(expand = c(0, 0.05), limits=c(0.3686729, 0.8606924))+
  ylab("Feedrate")+
  xlab("Size (g)")+theme_bw()


#now i have all the plots, i need to plot them all as 1
#the next chunk of code is just formatting correctly, and making sure they all look good and interpretable.

plot <- plot + theme(legend.position = "none",axis.text.y = element_text(size = 10),
                     axis.title.y = element_text(size = 11))

plot2 <- plot2 + theme(legend.position = "none",axis.text.y = element_text(size = 10),
                       axis.title.y = element_text(size = 11))

plot3 <- plot3 + theme(legend.position = "none",axis.title.x = element_blank(),
                       axis.text.x = element_text(size = 10),axis.text.y = element_text(size = 10),
                       axis.title.y = element_text(size = 11), panel.grid.minor = element_blank())+
  scale_x_continuous(trans="log10", expand = c(0.01, 0.1), limits = c(0.001, 10000),
                     labels = c(expression(10^-3), expression(10^-2), expression(10^-1), expression(10^0), expression(10^1),
                                expression(10^2), expression(10^3), expression(10^4)), breaks = 10^seq(-3,4, by = 1),
                     minor_breaks = 10^seq(-3,4, by = 1))
plotm <- plotm+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),legend.position = "none")

plot2m <- plot2m+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),legend.position = "none")

plot3m <- plot3m+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        legend.position = "none",panel.grid.minor = element_blank())+
  scale_x_continuous(trans="log10", expand = c(0.01, 0.1), limits = c(0.001, 10000),
                     labels = c(expression(10^-3), expression(10^-2), expression(10^-1), expression(10^0), expression(10^1),
                                expression(10^2), expression(10^3), expression(10^4)), breaks = 10^seq(-3,4, by = 1),
                     minor_breaks = 10^seq(-3,4, by = 1))

plotl <- plotl+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),legend.position = "none")

plot2l <- plot2l+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),legend.position = "none")

plot3l <- plot3l+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),legend.position = "none",
        axis.title.x = element_blank(),axis.text.x = element_text(size = 10),
        panel.grid.minor = element_blank())+
  scale_x_continuous(trans="log10", expand = c(0.01, 0.1), limits = c(0.001, 10000),
                     labels = c(expression(10^-3), expression(10^-2), expression(10^-1), expression(10^0), expression(10^1),
                                expression(10^2), expression(10^3), expression(10^4)), breaks = 10^seq(-3,4, by = 1),
                     minor_breaks = 10^seq(-3,4, by = 1))


#now all these plots look good, i can plot them together
plot + plotm + plotl +
plot2 + plot2m + plot2l+
plot3+ plot3m+ plot3l+
plot_layout(ncol = 3)+
plot_annotation(tag_levels = 'a')&theme(plot.tag = element_text(size = 13))
#here is the final plot

#### end ####

####plotting other plots for supplementary####

#Sup 1
Yplot <- ggplot()+
  geom_point(strategies_df, mapping=aes(Y, F_small))

Yplot2 <-ggplot()+
  geom_point(strategies_df, mapping=aes(Y, F_med))

Yplot3 <- ggplot()+
  geom_point(strategies_df, mapping=aes(Y, F_large))

Yplot+Yplot2+Yplot3

#Sup 2

(ggplot()+geom_point(strategies_df, mapping=aes(B, F_small)))+
  (ggplot()+geom_point(strategies_df, mapping=aes(B, F_small)))+
  (ggplot()+geom_point(strategies_df, mapping=aes(B, F_small)))
#Sup 3

(ggplot()+geom_point(strategies_df, mapping=aes(Bflux, F_small)))+
  (ggplot()+geom_point(strategies_df, mapping=aes(Bflux, F_small)))+
  (ggplot()+geom_point(strategies_df, mapping=aes(Bflux, F_small)))

#Sup 4

(ggplot()+geom_point(strategies_df, mapping=aes(Y, S_small)))+
  (ggplot()+geom_point(strategies_df, mapping=aes(Y, S_small)))+
  (ggplot()+geom_point(strategies_df, mapping=aes(Y, S_small)))

#Sup 5

(ggplot()+geom_point(strategies_df, mapping=aes(S_small, B)))+
  (ggplot()+geom_point(strategies_df, mapping=aes(S_med, B)))+
  (ggplot()+geom_point(strategies_df, mapping=aes(S_large, B)))

#Sup 6

(ggplot()+geom_point(strategies_df, mapping=aes(Bflux, S_small)))+
  (ggplot()+geom_point(strategies_df, mapping=aes(Bflux, S_small)))+
  (ggplot()+geom_point(strategies_df, mapping=aes(Bflux, S_small)))

#Sup 7

(ggplot()+geom_point(strategies_df, mapping=aes(Bflux, S_med, color=E_small))+
  (ggplot()+geom_point(strategies_df, mapping=aes(Bflux, S_med, color=E_med)))+
  (ggplot()+geom_point(strategies_df, mapping=aes(Bflux, S_med, color=E_large))))

#Sup 8

#need to add the colours again
strategies_df <- read.csv("fishcarbon_metrics_9sp.csv")
#now i am adding in the residuals. This next section of code is taken from Falciani et al. and is not mine,
model1 <- gam(formula = strategies_df$B ~ s(strategies_df$Y, bs = "cs"), method = "REML")
strategies_df$residuals <- residuals(model1, type = "deviance")

strategies_df$fill_color <- sapply(strategies_df$residuals, fill_color)

#linepoints has been generated prior in code.
#this code just extracts the values of Y and B to be plotted separately.
lar <- linepoints18large[, c("Y", "B")]
me <- linepoints18med[, c("Y", "B")]
sm <- linepoints18small[, c("Y", "B")]


ggplot(strategies_df, aes(x=Y, y=B, color=fill_color)) +
  geom_point() +
  geom_smooth(method = 'gam', color = "black", se = FALSE) +
  geom_smooth(lar, color="red", mapping=aes(Y,B))+
  geom_smooth(me, color="black", mapping=aes(Y,B))+
  geom_smooth(sm, color="green", mapping=aes(Y,B))+
  scale_x_continuous(name = expression("Yield"), breaks = seq(0, 2, .2), expand = c(0, 0)) +
  scale_y_continuous(name = expression("Relative Biomass"), breaks = seq(0, 2, .2)) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "gray50", barwidth = 0.5, title.hjust = 5)) +
  theme_light() +scale_color_identity()+
  theme(legend.position = c(.93, .65),
        plot.margin = grid::unit(c(0,0,0,0), "mm"),
        legend.background = element_rect(fill = alpha(0.8)),
        text = element_text(family = "serif"))




