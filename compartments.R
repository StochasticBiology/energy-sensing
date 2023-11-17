library(ggplot2)
library(ggpubr)

sf = 2

#### sim1 -- normal setup -- how do delta and epsilon (cost and effect of sensing) change performance for different environments

df.ts = read.csv("sim-out.csv")

# time series
g.1.a = ggplot(df.ts[df.ts$period < 4,]) + 
  geom_line(aes(x=t,y=S), color="green") + 
  geom_line(aes(x=t,y=F), color="red") + 
  geom_line(aes(x=t,y=alpha), color="grey") + 
  facet_grid(positive*period~epsilon) + 
  ggtitle("S (green) vs F (red) in env (grey) with epsilon (columns)")

g.1.d = ggplot(df.ts[df.ts$period %in% c(0, 2, 8) & df.ts$positive==1,]) + 
  geom_line(aes(x=t,y=S), color="green") + 
  geom_line(aes(x=t,y=F), color="red") + 
  geom_line(aes(x=t,y=alpha), color="grey") + 
  facet_grid(period~epsilon) + theme_light() + ylab("Value") +
  ggtitle("S (green), F (red), alpha (grey) with epsilon (columns) and alpha period (rows)")

g.1.f = ggplot(df.ts[df.ts$period %in% c(0, 2, 8) & df.ts$positive==-1,]) + 
  geom_line(aes(x=t,y=S), color="green") + 
  geom_line(aes(x=t,y=F), color="red") + 
  geom_line(aes(x=t,y=alpha), color="grey") + 
  facet_grid(period~epsilon) + theme_light() + ylab("Value") +
  ggtitle("S (green), F (red), alpha (grey) with epsilon (columns) and alpha period (rows)")

df.sum = read.csv("sim-out-sum.csv")

# summaries of performance
g.1.b = ggplot(df.sum[df.sum$period %in% c(0, 2, 8),], aes(x=epsilon,y=delta,z=S.ratio)) + 
  geom_contour_filled() + facet_grid(period ~ positive) +
  ggtitle("S with epsilon and delta by alpha period (rows) and initial gradient (columns)")

g.1.c = ggplot(df.sum[df.sum$period >= 0 & df.sum$delta==0,], aes(x=period, y=S.ratio, color=factor(epsilon))) + 
  geom_line() + facet_wrap(~positive) + theme_light() +
  ggtitle("S with epsilon and env period (columns)")

df.sum.0 = df.sum[df.sum$delta==0 & df.sum$epsilon == 0,]
df.sum.1 = df.sum[df.sum$delta==0 & df.sum$epsilon == 1,]

df.sum.01 = cbind(df.sum.0, data.frame(S.ratio.1=df.sum.1$S.ratio))
df.sum.01 = df.sum.01[df.sum.01$period != -1,]
g.1.e = ggplot(df.sum.01, aes(x=period, y=S.ratio.1/S.ratio, color=factor(positive))) +
  geom_line() + theme_light() + 
  ggtitle("Relative advantage of costless sensing for different alpha period and initial gradient")

png("sim-1-sensing-advantage.png", width=300*sf, height=200*sf, res=72*sf)
print(g.1.e)
dev.off()

png("sim-1-time-series.png", width=600*sf, height=300*sf, res=72*sf)
print(g.1.d)
dev.off()
png("sim-1-summary.png", width=800*sf, height=400*sf, res=72*sf)
print(ggarrange(g.1.b, g.1.c, labels=c("A", "B"), nrow=1))
dev.off()

png("sim-1-all.png", width=1200*sf, height=600*sf, res=72*sf)
#print(ggarrange(g.1.d, ggarrange(g.1.b, g.1.c, g.1.e, nrow = 1, labels = c("B", "C", "D")), labels = c("A", ""), nrow=2))
print(ggarrange(ggarrange(g.1.d, g.1.f, nrow=1, labels=c("A", "B")),
                ggarrange(g.1.b, g.1.c, g.1.e, nrow = 1, labels = c("C", "D", "E")),
                nrow=2))
dev.off()

### sim2 -- varying rho and sigma (propensity for resting vs seeking)

df.ts = read.csv("sim2-out.csv")

# time series
g.2.a = ggplot(df.ts[df.ts$period < 4 & df.ts$period != 0,]) + 
  geom_line(aes(x=t,y=S), color="green") + 
  geom_line(aes(x=t,y=F), color="red") + 
  geom_line(aes(x=t,y=alpha), color="grey") + 
  facet_grid(positive*period~epsilon) + 
  ggtitle("S (green) vs F (red) in env (grey) with epsilon (columns)")

# summary plot
df.sum = read.csv("sim2-out-sum.csv")

g.2.b = ggplot(df.sum[df.sum$period %in% c(0, 2),], aes(x=rho,y=sigma,z=S.ratio)) + 
  geom_contour_filled() + facet_grid(period*positive ~ epsilon) +
  ggtitle("S with rho and sigma for different env (rows) and epsilon (cols)")

g.2.c = ggplot(df.sum[df.sum$period %in% c(2),], aes(x=rho,y=sigma,z=S.ratio)) + 
  geom_contour_filled() + facet_grid(period*positive ~ epsilon) +
  ggtitle("S with rho and sigma for different env (rows) and epsilon (cols)")
g.2.d = ggplot(df.sum[df.sum$period %in% c(8),], aes(x=rho,y=sigma,z=S.ratio)) + 
  geom_contour_filled() + facet_grid(period*positive ~ epsilon) +
  ggtitle("S with rho and sigma for different env (rows) and epsilon (cols)")


png("sim-2-summary.png", width=1000*sf, height=400*sf, res=72*sf)
print(ggarrange(g.2.c, g.2.d, labels=c("A", "B"), nrow=1))
dev.off()

### sim3 -- repeat sim1 with dfferent rho and sigma, for sanity

df.ts = read.csv("sim3-out.csv")

# time series
g.3.a = ggplot(df.ts[df.ts$period < 4,]) + 
  geom_line(aes(x=t,y=S), color="green") + 
  geom_line(aes(x=t,y=F), color="red") + 
  geom_line(aes(x=t,y=alpha), color="grey") + 
  facet_grid(positive*period~epsilon) + xlim(0,15) + ylim(0,1) +
  ggtitle("S (green) vs F (red) in env (grey) with epsilon (columns)")

# summary plot
df.sum = read.csv("sim3-out-sum.csv")

g.3.b = ggplot(df.sum[df.sum$period %in% c(0, 1, 2, 4, 8),], aes(x=epsilon,y=delta,z=S.ratio)) + 
  geom_contour_filled() + facet_grid(period ~ positive) +
  ggtitle("S with epsilon and delta by env")

g.3.c = ggplot(df.sum[df.sum$period >= 0 & df.sum$delta==0,], aes(x=period, y=S.ratio, color=factor(epsilon))) + 
  geom_line() + facet_wrap(~positive) +
  ggtitle("S with epsilon and env period")

png("sim-3-summary.png", width=800*sf, height=400*sf, res=72*sf)
print(ggarrange(g.3.b, g.3.c, labels=c("A", "B"), nrow=1))
dev.off()

### sim4 -- stochastic environmental

df.ts = read.csv("sim4-out.csv")

# limited number of sample time series
g.4.a = ggplot(df.ts[df.ts$sample < 5,]) + 
  geom_line(aes(x=t,y=S, fill=factor(sample)), color="green") + 
  geom_line(aes(x=t,y=F, fill=factor(sample)), color="red") + 
  geom_line(aes(x=t,y=alpha, fill=factor(sample)), color="grey") + 
  facet_grid(kernel~epsilon) + xlim(0,15) + ylim(0,1) + theme_light() +
  ggtitle("S (green) vs F (red) in env (grey) with epsilon (columns) and random kernel width (rows)")

# summary plots with boxplots
df.sum = read.csv("sim4-out-sum.csv")

g.4.b = ggplot(df.sum[df.sum$delta %in% c(0,0.5,0.9),], aes(x=factor(kernel),y=S.ratio,fill=factor(epsilon))) + 
  geom_boxplot()+ facet_grid(~delta, scales="free_x") + theme_light() +
  ggtitle("S with epsilon by random kernel width and delta (columns)")

png("sim-4-both.png", width=800*sf, height=600*sf, res=72*sf)
ggarrange(g.4.a, g.4.b, nrow=2, labels=c("A", "B"))
dev.off()

