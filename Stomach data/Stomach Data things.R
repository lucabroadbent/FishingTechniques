library(dplyr)
library(ggplot2)
library(tidyr)
#reading in the data
load("C:/Users/lucab/Downloads/stomach_dataset.Rdata") 

anc <- read.csv("C:/Users/lucab/Downloads/AnchovyAndSardine.csv")

#i need to first add the chile samples as well.
 
murray <- stom_df |> 
  filter(!is.na(pred_weight_g),
         prey_ind_weight_g > 0,
         prey_count > 0) |>
  select(pred_species, pred_weight_g, prey_ind_weight_g, prey_count) |>
  group_by(pred_species) |> 
  filter(n() > 1000) |> 
  rename(w_prey = prey_ind_weight_g, w_pred = pred_weight_g, n_prey = prey_count) |>
  mutate(log_ppmr = log(w_pred / w_prey),
         weight_numbers = n_prey / sum(n_prey))


### this section here what i want to do is investigate the spatial distribution of the mackerel data - is it different?

mack <- stom_df%>%
  filter(pred_species =="Scomber scombrus")

mackgreat12 <- mack%>%
  filter(ppmr > exp(12))

mackless12 <- mack%>%
  filter(ppmr < exp(12))

#so now i have the data. i want to find the proportion of each sample (which is the row, i think)
#that is inside each 

#first, i am going to make sure its a unique sample id (as this makes sure we dont count the same sample twice)
#i need to do this as i am assuming that the number of samples taken is the effort of sampling, and i hope it is 
#the same effort in the ranges for each mack dataset.

#what i want here is to only subset the dataframe, and only include sample ids that are unique

mackgreat12 <- mackgreat12%>%
  distinct(sample_id, ices_rectangle)

mackless12 <- mackless12%>%
  distinct(sample_id, ices_rectangle)

#now i need to add these up for each rectangle, to find the amount of samples per rectangle
mackgreat12 <- mackgreat12%>%
  group_by(ices_rectangle)%>%
  summarise(n = n())

mackless12 <- mackless12%>%
  group_by(ices_rectangle)%>%
  summarise(n = n())

#now i need to normalise these values

mackgreat12 <- mackgreat12%>%
  mutate(n = n/sum(n))

mackless12 <- mackless12%>%
  mutate(n = n/sum(n))

#now i want to find the difference between both data sets for a given value of ices rectangle
#if one dataset doesnt have a value, i want to set it to 0

mackdiff <- full_join(mackgreat12, mackless12, by = "ices_rectangle")%>%
  mutate(diff = n.x - n.y)

#now i want to plot this

ggplot(mackdiff, aes(x = ices_rectangle, y = diff))+
  geom_bar(stat = "identity")+
  xlab("ICES Rectangle")+
  ylab("Difference in proportion of samples")+
  ggtitle("Difference in proportion of samples with ppmr > 12 and ppmr < 12")

#ok this shows up something good - in general, the proportion of samples across both of the mack subsets is between 
#0.5% and 1%, so therefore, they only differ in an effort of 1% of their total samples.
#however, when I instead look at the difference in number of samples taken from each location, the mack dataset
#that contains values that are less than 12 has taken many more samples, so across all the rectangles, 
#there is a large difference. 

#but when it is proportional, there is only a difference of max 1%
#when i think about it, this is good, as there are not really large differences between the effort allocated
#to sampling - they have sampled equal amounts in all the rectangles, so the data is comparable.

#however, this variation might be a lot, so i should check the standard deviation of the proportion
#just to be sure.

#i want to plot the mack great and mack less dataframes as density plots, to see if they are distributed the same

mackgreat12 <- mackgreat12 %>% mutate(group = "ppmr > 12")
mackless12 <- mackless12 %>% mutate(group = "ppmr < 12")

# Combine the datasets
combined_data <- bind_rows(mackgreat12, mackless12)

# Plot
ggplot(combined_data, aes(x = n, color = group)) +
  geom_density() +
  scale_color_manual(values = c("ppmr > 12" = "red", "ppmr < 12" = "blue")) +
  xlab("Proportion of samples") +
  ylab("Density") +
  ggtitle("Density plot of proportion of samples with ppmr > 12 and < 12") +
  theme_minimal()
 #yes they are.
#the distribution is majority of rectangles are 0.1% of the samples, and there aren't very many that are above 0.5%

#i want to plot the distribution of the differences between the sampling effort in the rectangles as a boxplot

ggplot(mackdiff, aes(y = diff))+
  geom_boxplot()+
  xlab("ICES Rectangle")+
  ylab("Difference in proportion of samples")+
  ggtitle("Difference in proportion of samples with ppmr > 12 and ppmr < 12")

#ok now this looks very good now, the 25-75 percentiles are within 0.001 -  which is equal to 0.1% of the samples
#does this look good? if they on average differ by 0.1%, and they are usually 0.1% anyway, this is a large difference?

#just to check i am going to do it as a percentage change between rectangles of diff samples

mackdiff <- mackdiff%>%
  mutate(perc_diff = (n.x - n.y)/n.y)

ggplot(mackdiff, aes(x = ices_rectangle, y = perc_diff))+
  geom_bar(stat = "identity")+
  xlab("ICES Rectangle")+
  ylab("Percentage difference in proportion of samples")+
  ggtitle("Percentage difference in proportion of samples with ppmr > 12 and ppmr < 12")
#now a quick boxplot
ggplot(mackdiff, aes(y = perc_diff))+
  geom_boxplot()+
  xlab("ICES Rectangle")+
  ylab("Percentage difference in proportion of samples")+
  ggtitle("Percentage difference in proportion of samples with ppmr > 12 and ppmr < 12")
#yep, majority of the differences is between 1% change, so therefore, both samples are taken at the same effort 
#from both regions.

#so the region isnt different, so what reason for the different ppmr values?
#first i will look at the difference in the predator mass and prey mass


#i want to join the mackless and mackgreat dataframes into one so i can plot code easier

mackless12$group <- "less"
mackgreat12$group <- "great"
mackboth <- rbind(mackless12, mackgreat12)

#now we plot boxplots of both the predator mass and prey mass

ggplot(mackboth, aes(x = group, y = mackboth$pred_weight_g))+
  geom_boxplot()+
  xlab("Group")+
  ylab("Predator mass [g]")

ggplot(mackboth, aes(x = group, y = log(mackboth$prey_weight_g)))+
  geom_boxplot()+
  xlab("Group")+
  ylab("Prey mass [g]")

#the prey mass is considerably lower in the greater than 12 group.  But you cant see much a change
#in the predator mass, as it is categorical.

#i want to plot the predator mass against the prey mass on a density plot

ggplot(mackboth, aes(x = (mackboth$pred_weight_g), y = log(mackboth$prey_weight_g), color = group))+
  geom_point()+
  xlab("Log of predator mass [g]")+
  ylab("Log of prey mass [g]")

# so it appears that the predator mass is not changing, but the prey mass is.
#why might this be? they are not in different regions, but there is a large diference 
#between their feeding habits.

#i think i may need to unweight the dataset, as the points here may not be representative of the data
#also, looking at the data format, the ices data is binned, whereas the dapstom data isnt binned.
#this isnt important now, but might be later 

#now i am going to look at the prey species compositition between the two groups.


#first i need to unweight it.

mackboth <- mackboth %>%
  uncount(weights=prey_count)

#worked, now i need to see the species composition for each group

mackboth %>%
  group_by(group, prey_species) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = prey_species, y = log(n), fill = group))+
  geom_bar(stat = "identity", position = "dodge")+
  xlab("Prey species")+
  ylab("Number of samples")+
  ggtitle("Number of samples of each prey species in each group")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#it is too many species to check really. will look at functional group instead

mackboth %>%
  group_by(group, prey_funcgrp) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = prey_funcgrp, y = log(n), fill = group))+
  geom_bar(stat = "identity", position = "dodge")+
  xlab("Prey species")+
  ylab("Number of samples")+
  ggtitle("Number of samples of each prey species in each group")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#they are very similar, except for the nekton func group and a smaller difference in zooplankton
#OK, PROBLEMS / WHERE I AM AT.  need to make the functional groups proportional, then look at
#as the difference now looks like the number of samples in both. 



#both files are loaded in, first lets make them same format

anc <- anc |>
  rename(pred_species = Species, w_prey = wprey, w_pred = wpredator, n_prey = Nprey) |>
  mutate(log_ppmr = log(0.1 / w_prey),
         weight_numbers = n_prey / sum(n_prey))

#now we can bind the two datasets together
both <- bind_rows(murray, anc)

#i think this has worked.
#now lets plot stuff

ggplot(both,
       aes(x = log(w_pred), y = log_ppmr)) + geom_point() + geom_smooth(aes(weight = n_prey),
                                                                        colour = "red")+
  xlab("Log of predator mass [g]")+
  ylab("Log of predator/prey mass ratio")

#the predator mass changes the ppmr. 
#lets do the same plot but accounting for the available biomass of prey 

ggplot(both,
       aes(x = log(w_pred), y = log_ppmr)) + geom_point() + geom_smooth(aes(weight = (n_prey * w_prey^(2 / 3))),
                                                                        colour = "red")+
  xlab("Log of predator mass [g]")+
  ylab("Log of predator/prey mass ratio")+
  xlim(0,10)

#it is still not a straight line.
#going to try it in violin plot to see 

#this next code is separating into bins of equal datapoints
quantiles <- quantile(both$w_pred, probs = seq(0, 1, by = 0.1))
#then making the bins with these breaks.
both <- both %>%
  mutate(bin = cut(w_pred, breaks = quantiles, include.lowest = TRUE))

ggplot(both, aes(bin, log_ppmr)) +
  geom_violin(aes(weight = n_prey * w_prey^(2/3)), draw_quantiles = 0.5) +
  xlab("Predator weight [g]") +
  ylab("Log of predator/prey mass ratio")
#ok it is a lot better when accounting for biomass, but there is one 
#bin that is not good.


#going to try find out what data this is. 

ggplot(both, aes(x = log(w_pred), y = log_ppmr)) + 
  stat_summary_2d(aes(z = n_prey * w_prey^(2/3)), fun = "sum", bins = 60) + 
  scale_fill_viridis_c() + geom_smooth(aes(weight = n_prey * w_prey^(2/3)), colour = "red") + 
  xlab("Log of predator mass [g]") + ylab("Log of predator/prey mass ratio")+
  xlim(0,10)
#ppmr looks to be the same here on this plot, other than that at smaller predator sizes
#its not very good

#it is the bin 226 to 336 that causes trouble on the violin plot
#will investigate more

filtered_both <- both %>%
  filter(bin == "(226,336]")

df_long <- filtered_both %>%
  pivot_longer(cols = c(w_pred, w_prey, n_prey, log_ppmr, weight_numbers), names_to = "Variable", values_to = "Value")

hist_w_pred <- ggplot(filtered_both, aes(x = w_pred)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of w_pred", x = "w_pred", y = "Frequency")

hist_w_prey <- ggplot(filtered_both, aes(x = w_prey)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of w_prey", x = "w_prey", y = "Frequency")+
  scale_x_log10() +  # Apply log scale to x-axis
  scale_y_log10() 

hist_n_prey <- ggplot(filtered_both, aes(x = n_prey)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of n_prey", x = "n_prey", y = "Frequency")

hist_log_ppmr <- ggplot(filtered_both, aes(x = log_ppmr)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of log_ppmr", x = "log_ppmr", y = "Frequency")+
  scale_x_log10() +  
  scale_y_log10() 

hist_weight_numbers <- ggplot(filtered_both, aes(x = weight_numbers)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of weight_numbers", x = "weight_numbers", y = "Frequency")+
  scale_y_log10()


print(hist_w_pred)
print(hist_w_prey)
print(hist_n_prey)
print(hist_log_ppmr)
print(hist_weight_numbers)



weighted.sd <- function(x, w) {
  sqrt(sum(w * (x - weighted.mean(x, w))^2))
}
weight <- stomach$n_prey * (stomach$w_prey / stomach$w_pred)^dig
weight <- weight / sum(weight)

est_mean <- weighted.mean(stomach$log_ppmr, weight)
est_sd <- weighted.sd(stomach$log_ppmr, weight)

ggplot(stomach |> filter(log_ppmr < 12)) +
  geom_density(aes(log_ppmr, 
                   weight = n_prey * (w_prey / w_pred)^dig),
               bw = 0.2) +
  stat_function(fun = dnorm, 
                args = list(mean = est_mean, 
                            sd = est_sd), 
                colour = "blue") +
  xlab("Log of Predator/Prey mass ratio") +
  ylab("Normalised diet density")















