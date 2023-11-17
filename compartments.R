library(ggplot2)
library(ggpubr)

#### sim1 -- normal setup -- how do delta and epsilon (cost and effect of sensing) change performance for different environments

df = read.csv("sim-out.csv")

# time series
ggplot(df[df$period < 4,]) + 
  geom_line(aes(x=t,y=W), color="green") + 
  geom_line(aes(x=t,y=L), color="red") + 
  geom_line(aes(x=t,y=alpha), color="grey") + 
  facet_grid(positive*period~epsilon) + 
  ggtitle("W (green) vs L (red) in env (grey) with epsilon (columns)")

df = read.csv("sim-out-sum.csv")

# summaries of performance
ggplot(df[df$period %in% c(0, 1, 2, 4, 8),], aes(x=epsilon,y=delta,z=W.ratio)) + 
  geom_contour_filled() + facet_grid(period ~ positive) +
  ggtitle("W with epsilon and delta by env")

ggplot(df[df$period >= 0 & df$delta==0,], aes(x=period, y=W.ratio, color=factor(epsilon))) + 
  geom_line() + facet_wrap(~positive) +
  ggtitle("W with epsilon and env period")

### sim2 -- varying rho and sigma (propensity for resting vs seeking)

df = read.csv("sim2-out.csv")

# time series
ggplot(df[df$period < 4 & df$period != 0,]) + 
  geom_line(aes(x=t,y=W), color="green") + 
  geom_line(aes(x=t,y=L), color="red") + 
  geom_line(aes(x=t,y=alpha), color="grey") + 
  facet_grid(positive*period~epsilon) + 
  ggtitle("W (green) vs L (red) in env (grey) with epsilon (columns)")

# summary plot
df = read.csv("sim2-out-sum.csv")
ggplot(df[df$period %in% c(-1, 1, 4, 8),], aes(x=rho,y=sigma,z=W.ratio)) + 
  geom_contour_filled() + facet_grid(period*positive ~ epsilon) +
  ggtitle("W with rho and sigma for different env (rows) and epsilon (cols)")

### sim3 -- repeat sim1 with dfferent rho and sigma, for sanity

df = read.csv("sim3-out.csv")

# time series
ggplot(df[df$period < 4,]) + 
  geom_line(aes(x=t,y=W), color="green") + 
  geom_line(aes(x=t,y=L), color="red") + 
  geom_line(aes(x=t,y=alpha), color="grey") + 
  facet_grid(positive*period~epsilon) + xlim(0,15) + ylim(0,1) +
  ggtitle("W (green) vs L (red) in env (grey) with epsilon (columns)")

# summary plot
df = read.csv("sim3-out-sum.csv")

ggplot(df[df$period %in% c(0, 1, 2, 4, 8),], aes(x=epsilon,y=delta,z=W.ratio)) + 
  geom_contour_filled() + facet_grid(period ~ positive) +
  ggtitle("W with epsilon and delta by env")

ggplot(df[df$period >= 0 & df$delta==0,], aes(x=period, y=W.ratio, color=factor(epsilon))) + 
  geom_line() + facet_wrap(~positive) +
  ggtitle("W with epsilon and env period")

### sim4 -- stochastic environmental

df = read.csv("sim4-out.csv")

# limited number of sample time series
ggplot(df[df$sample < 5,]) + 
  geom_line(aes(x=t,y=W, fill=factor(sample)), color="green") + 
  geom_line(aes(x=t,y=L, fill=factor(sample)), color="red") + 
  geom_line(aes(x=t,y=alpha, fill=factor(sample)), color="grey") + 
  facet_grid(kernel~epsilon) + xlim(0,15) + ylim(0,1) +
  ggtitle("W (green) vs L (red) in env (grey) with epsilon (columns)")

# summary plots with boxplots
df = read.csv("sim4-out-sum.csv")

ggplot(df[df$delta %in% c(0,0.5,0.9),], aes(x=factor(kernel),y=W.ratio,fill=factor(epsilon))) + 
  geom_boxplot()+ facet_grid(~delta, scales="free_y") +
  ggtitle("W with epsilon by env")
