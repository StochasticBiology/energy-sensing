library(ggplot2)
library(ggpubr)
library(dplyr)

#### some specific examples

plot.time.series = function(tdf) {
  return(ggplot(tdf) + 
    geom_line(aes(x=t, y=env), color="grey") +
    geom_line(aes(x=t, y=stateW), color="blue") +
    geom_line(aes(x=t, y=stateL), color="red") +
    geom_line(aes(x=t, y=stateI), color="lightgreen") +
    geom_line(aes(x=t, y=stateA), color="darkgreen") +
      xlim(0,100) +
      theme_light() )
}

tdf.0a = read.csv("example-0a.csv")
tdf.0b = read.csv("example-0b.csv")
tdf.0c = read.csv("example-0c.csv")
tdf.1 = read.csv("example-1.csv")
tdf.2 = read.csv("example-2.csv")

ggarrange(plot.time.series(tdf.0a) + ggtitle("No sensing, flat"), 
          plot.time.series(tdf.0b) + ggtitle("No sensing, bad phase"),
          plot.time.series(tdf.0c) + ggtitle("No sensing, good phase"),
          plot.time.series(tdf.1) + ggtitle("Free sensing, bad phase"), 
          plot.time.series(tdf.2) + ggtitle("Costly sensing, bad phase"),
          nrow=3, ncol=2)

###### across parameterisations

df = read.csv("redo-out.csv")

# first focus on the case with no sensing cost
tdf = df[df$beta == 0,]
tdf[tdf$k0==1 & tdf$kp==0 & tdf$ki==0 & tdf$kd==0,]
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
ggplot(optimal_params, aes(x=log10(omega), y=phi, fill = k0)) + geom_tile(),
ggplot(optimal_params, aes(x=log10(omega), y=phi, fill = kp)) + geom_tile(),
ggplot(optimal_params, aes(x=log10(omega), y=phi, fill = ki)) + geom_tile(),
ggplot(optimal_params, aes(x=log10(omega), y=phi, fill = kd)) + geom_tile()
)


#### now with a sensing cost

# first focus on the case with no sensing cost
tdf = df[df$beta == 0.1,]
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
  ggplot(optimal_params, aes(x=log10(omega), y=phi, fill = k0)) + geom_tile(),
  ggplot(optimal_params, aes(x=log10(omega), y=phi, fill = kp)) + geom_tile(),
  ggplot(optimal_params, aes(x=log10(omega), y=phi, fill = ki)) + geom_tile(),
  ggplot(optimal_params, aes(x=log10(omega), y=phi, fill = kd)) + geom_tile()
)

###### compare optimal behaviours

ggarrange(beta0opts, beta2opts)
