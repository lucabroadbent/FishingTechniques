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















