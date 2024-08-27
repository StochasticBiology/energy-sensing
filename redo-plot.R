library(ggplot2)
library(ggpubr)
library(dplyr)

df = read.csv("redo-out.csv")

# first focus on the case with no sensing cost
tdf = df[df$beta == 0,]
# take a subsample of results for processing
sub = tdf[sample(1:nrow(tdf), 10000),]

# get the best k0-kd parameter set for each environment (omega-phi)
optimal_params <- sub %>%
  group_by(phi, omega) %>%          
  slice_max(W, with_ties = FALSE) %>% 
  ungroup()   

# get the mean and sd of W for each parameter set, across environments
param_fitness_stats <- sub %>%
  group_by(k0, kp, ki, kd) %>%  
  summarise(
    mean_fitness = mean(W, na.rm = TRUE),
    sd_fitness = sd(W, na.rm = TRUE)
  ) %>%
  ungroup()  

# report the overall best-performing parameters
param_fitness_stats[order(param_fitness_stats$mean_fitness, decreasing = TRUE),]

# plot of best parameterisations by environment
beta0opts = ggarrange(
ggplot(optimal_params, aes(x=omega, y=phi, fill = k0)) + geom_tile(),
ggplot(optimal_params, aes(x=omega, y=phi, fill = kp)) + geom_tile(),
ggplot(optimal_params, aes(x=omega, y=phi, fill = ki)) + geom_tile(),
ggplot(optimal_params, aes(x=omega, y=phi, fill = kd)) + geom_tile()
)


#### now with a sensing cost

# first focus on the case with no sensing cost
tdf = df[df$beta == 0.2,]
# take a subsample of results for processing
sub = tdf[sample(1:nrow(tdf), 10000),]

# get the best k0-kd parameter set for each environment (omega-phi)
optimal_params <- sub %>%
  group_by(phi, omega) %>%          
  slice_max(W, with_ties = FALSE) %>% 
  ungroup()   

# get the mean and sd of W for each parameter set, across environments
param_fitness_stats <- sub %>%
  group_by(k0, kp, ki, kd) %>%  
  summarise(
    mean_fitness = mean(W, na.rm = TRUE),
    sd_fitness = sd(W, na.rm = TRUE)
  ) %>%
  ungroup()  

# report the overall best-performing parameters
param_fitness_stats[order(param_fitness_stats$mean_fitness, decreasing = TRUE),]

# plot of best parameterisations by environment
beta2opts = ggarrange(
  ggplot(optimal_params, aes(x=omega, y=phi, fill = k0)) + geom_tile(),
  ggplot(optimal_params, aes(x=omega, y=phi, fill = kp)) + geom_tile(),
  ggplot(optimal_params, aes(x=omega, y=phi, fill = ki)) + geom_tile(),
  ggplot(optimal_params, aes(x=omega, y=phi, fill = kd)) + geom_tile()
)

###### compare optimal behaviours

ggarrange(beta0opts, beta2opts)
