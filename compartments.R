library(ggplot2)
library(ggpubr)

sf = 2

#### sim1 -- normal setup -- how do delta and epsilon (cost and effect of sensing) change performance for different environments

df.ts = read.csv("sim-out.csv")

# time series
g.1.a = ggplot(df.ts[df.ts$period < 4,]) + 
  geom_line(aes(x=t,y=W), color="green") + 
  geom_line(aes(x=t,y=L), color="red") + 
  geom_line(aes(x=t,y=alpha), color="grey") + 
  facet_grid(positive*period~epsilon) + 
  ggtitle("W (green) vs L (red) in env (grey) with epsilon (columns)")

df.sum = read.csv("sim-out-sum.csv")

# summaries of performance
g.1.b = ggplot(df.sum[df.sum$period %in% c(0, 1, 2, 4, 8),], aes(x=epsilon,y=delta,z=W.ratio)) + 
  geom_contour_filled() + facet_grid(period ~ positive) +
  ggtitle("W with epsilon and delta by env")

g.1.c = ggplot(df.sum[df.sum$period >= 0 & df.sum$delta==0,], aes(x=period, y=W.ratio, color=factor(epsilon))) + 
  geom_line() + facet_wrap(~positive) +
  ggtitle("W with epsilon and env period")

png("sim-1-time-series.png", width=800*sf, height=1200*sf, res=72*sf)
print(g.1.a)
dev.off()
png("sim-1-summary.png", width=800*sf, height=400*sf, res=72*sf)
print(ggarrange(g.1.b, g.1.c, labels=c("A", "B"), nrow=1))
dev.off()


### sim2 -- varying rho and sigma (propensity for resting vs seeking)

df.ts = read.csv("sim2-out.csv")

# time series
g.2.a = ggplot(df.ts[df.ts$period < 4 & df.ts$period != 0,]) + 
  geom_line(aes(x=t,y=W), color="green") + 
  geom_line(aes(x=t,y=L), color="red") + 
  geom_line(aes(x=t,y=alpha), color="grey") + 
  facet_grid(positive*period~epsilon) + 
  ggtitle("W (green) vs L (red) in env (grey) with epsilon (columns)")

# summary plot
df.sum = read.csv("sim2-out-sum.csv")

g.2.b = ggplot(df.sum[df.sum$period %in% c(-1, 1, 4, 8),], aes(x=rho,y=sigma,z=W.ratio)) + 
  geom_contour_filled() + facet_grid(period*positive ~ epsilon) +
  ggtitle("W with rho and sigma for different env (rows) and epsilon (cols)")

png("sim-2-summary.png", width=800*sf, height=1200*sf, res=72*sf)
print(g.2.b)
dev.off()

### sim3 -- repeat sim1 with dfferent rho and sigma, for sanity

df.ts = read.csv("sim3-out.csv")

# time series
g.3.a = ggplot(df.ts[df.ts$period < 4,]) + 
  geom_line(aes(x=t,y=W), color="green") + 
  geom_line(aes(x=t,y=L), color="red") + 
  geom_line(aes(x=t,y=alpha), color="grey") + 
  facet_grid(positive*period~epsilon) + xlim(0,15) + ylim(0,1) +
  ggtitle("W (green) vs L (red) in env (grey) with epsilon (columns)")

# summary plot
df.sum = read.csv("sim3-out-sum.csv")

g.3.b = ggplot(df.sum[df.sum$period %in% c(0, 1, 2, 4, 8),], aes(x=epsilon,y=delta,z=W.ratio)) + 
  geom_contour_filled() + facet_grid(period ~ positive) +
  ggtitle("W with epsilon and delta by env")

g.3.c = ggplot(df.sum[df.sum$period >= 0 & df.sum$delta==0,], aes(x=period, y=W.ratio, color=factor(epsilon))) + 
  geom_line() + facet_wrap(~positive) +
  ggtitle("W with epsilon and env period")

png("sim-3-summary.png", width=800*sf, height=400*sf, res=72*sf)
print(ggarrange(g.3.b, g.3.c, labels=c("A", "B"), nrow=1))
dev.off()

### sim4 -- stochastic environmental

df.ts = read.csv("sim4-out.csv")

# limited number of sample time series
g.4.a = ggplot(df.ts[df.ts$sample < 5,]) + 
  geom_line(aes(x=t,y=W, fill=factor(sample)), color="green") + 
  geom_line(aes(x=t,y=L, fill=factor(sample)), color="red") + 
  geom_line(aes(x=t,y=alpha, fill=factor(sample)), color="grey") + 
  facet_grid(kernel~epsilon) + xlim(0,15) + ylim(0,1) +
  ggtitle("W (green) vs L (red) in env (grey) with epsilon (columns)")

# summary plots with boxplots
df.sum = read.csv("sim4-out-sum.csv")

g.4.b = ggplot(df.sum[df.sum$delta %in% c(0,0.5,0.9),], aes(x=factor(kernel),y=W.ratio,fill=factor(epsilon))) + 
  geom_boxplot()+ facet_grid(~delta, scales="free_y") +
  ggtitle("W with epsilon by env")

png("sim-4-both.png", width=800*sf, height=1200*sf, res=72*sf)
ggarrange(g.4.a, g.4.b, nrow=2, labels=c("A", "B"))
dev.off()
