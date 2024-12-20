library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
library(viridis)

# redundant code used to compare different simulation versions
comparison = FALSE

# plots and comparisons for PID project
# ChatGPT used to create some plot stylings

if(comparison == TRUE) {
  
  ##### RK simulations
  
  # optimal_results_with_K0.csv represents stochastic environment with sensing at different sensing costs.
  # optimal_results_with_K01.csv indicates deterministic environment with free sensing.
  # optimal_results_with_K02.csv indicates stochastic environment with free sensing.
  # optimal_results_with_K03.csv indicates deterministic environment with no sensing.
  # optimal_results_with_K04.csv indicates stochastic environment with no sensing.
  # Why are there nonzero Kp,i,d values in K03 (no sensing)?
  # Why are there nonzero Beta values in K01 (free sensing)?
  # Where are the deterministic results for different sensing costs? 
  # Looks like "no sensing" allows K0 (only) to change
  
  df.stoch = read.csv("optimal_results_with_K0.csv")
  df.det = read.csv("optimal_results_with_K0_fixed.csv")
  df.det = read.csv("optimal_results_sensing_costs.csv")
  df = df.det
  
  
  # (o) Null behaviour
  df_null = df[df$K0==1 & df$Kp==0 & df$Ki==0 & df$Kd==0,]
  g.o = ggplot(df_null, aes(x=Phase, y=Frequency, fill=S)) + 
    geom_tile() + facet_wrap(~Beta)
  
  # check that we don't exceed max possible win in static case
  sub0 = df[df$Frequency == 0,]
  ggplot(sub0, aes(x=0.5+0.5*sin(Phase), y=S)) + geom_point() + geom_abline() +
    labs(x = "Max possible", y = "Max observed") 
  
  length(which(abs(df$S+df$L-1)<0.01))/nrow(df)
  not.eq = df[abs(df$S+df$L-1)>0.01,]
  summary(not.eq)
  summary(not.eq[not.eq$S==0,])
  not.eq$K0+not.eq$Kp+not.eq$Ki+not.eq$Kd
  
  # (a) Maximum response value for each set of state variables
  df_max_response <- df %>%
    group_by(Frequency, Beta, Phase) %>%
    summarize(max_response = max(S), .groups = 'drop')
  
  df_improvements = df_max_response %>%
    left_join(df_null, by = c("Frequency" = "Frequency", "Phase" = "Phase", "Beta" = "Beta")) %>%
    rename(null_response = S)
  
  g.a = ggplot(df_max_response, aes(x=Phase, y=Frequency, fill=max_response)) + 
    geom_tile() + facet_wrap(~Beta)
  g.a.1 = ggplot(df_improvements, aes(x=Phase, y=Frequency, fill=max_response-null_response)) + 
    geom_tile() + facet_wrap(~Beta)
  
  # (b) Optimal control variables that give the maximum response for each set of state variables
  df_optimal_controls <- df %>%
    group_by(Frequency, Beta, Phase) %>%
    filter(S == max(S)) %>%
    summarize(
      K0 = mean(K0),
      Kp = mean(Kp),
      Ki = mean(Ki),
      Kd = mean(Kd),
      .groups = 'drop'
    )
  
  g.b.1 = ggarrange(
    ggplot(df_optimal_controls[df_optimal_controls$Beta==0,], aes(x=Phase, y=Frequency, fill=K0)) + geom_tile(),
    ggplot(df_optimal_controls[df_optimal_controls$Beta==0,], aes(x=Phase, y=Frequency, fill=Kp)) + geom_tile(),
    ggplot(df_optimal_controls[df_optimal_controls$Beta==0,], aes(x=Phase, y=Frequency, fill=Ki)) + geom_tile(),
    ggplot(df_optimal_controls[df_optimal_controls$Beta==0,], aes(x=Phase, y=Frequency, fill=Kd)) + geom_tile()
  )
  
  g.b.2 = ggarrange(
    ggplot(df_optimal_controls[df_optimal_controls$Beta==1,], aes(x=Phase, y=Frequency, fill=K0)) + geom_tile(),
    ggplot(df_optimal_controls[df_optimal_controls$Beta==1,], aes(x=Phase, y=Frequency, fill=Kp)) + geom_tile(),
    ggplot(df_optimal_controls[df_optimal_controls$Beta==1,], aes(x=Phase, y=Frequency, fill=Ki)) + geom_tile(),
    ggplot(df_optimal_controls[df_optimal_controls$Beta==1,], aes(x=Phase, y=Frequency, fill=Kd)) + geom_tile()
  )
  
  # (c) Performance of given control set across conditions
  df_mean_performance <- df %>%
    group_by(Beta, K0, Kp, Ki, Kd) %>%
    summarize(mean_response = mean(S), .groups = 'drop')
  
  top.0 = arrange(df_mean_performance[df_mean_performance$Beta==0,], desc(mean_response))
  top.0$rank = 1:nrow(top.0)
  top.p.0 <- top.0 %>%
    pivot_longer(cols = c(K0,Kp,Ki,Kd), names_to = "category", values_to = "value")
  
  # Plot with stacked bars
  nset = 50*4
  g.c.0 = ggplot() +
    geom_bar(data = top.p.0[1:nset,], aes(x = rank, y = value, fill = category), stat = "identity", position = "stack") +
    geom_line(data = unique(top.p.0[1:nset,2:3]), aes(x=rank, y=mean_response)) +
    labs(x = "Row ID", y = "Value", title = "Best PID strategies (free)") +
    theme_minimal()
  
  top.1 = arrange(df_mean_performance[df_mean_performance$Beta==1,], desc(mean_response))
  top.1$rank = 1:nrow(top.1)
  top.p.1 <- top.1 %>%
    pivot_longer(cols = c(K0,Kp,Ki,Kd), names_to = "category", values_to = "value")
  
  # Plot with stacked bars
  nset = 50*4
  g.c.1 = ggplot() +
    geom_bar(data = top.p.1[1:nset,], aes(x = rank, y = value, fill = category), stat = "identity", position = "stack") +
    geom_line(data = unique(top.p.1[1:nset,2:3]), aes(x=rank, y=mean_response)) +
    labs(x = "Row ID", y = "Value", title = "Best PID strategies (costly)") +
    theme_minimal()
  
}

##### IJ simulations

# read and label data
df.i.tmp = read.csv("redo-out-rk.csv")
head(df.i.tmp)
colnames(df.i.tmp) = c("Beta", "Epsilon", "Frequency", "Phase", "K0", "Kp", "Ki", "Kd", "S", "L", "delta", "tend")

# first pull out deterministic set (keeping the rest in df.all)
df.all = df.i.tmp
df.i.tmp = df.i.tmp[df.i.tmp$Epsilon==0,]
df.i = df.i.tmp[df.i.tmp$tend <= 100,1:9]

# when do approaches fail to converge?
non.con = df.i.tmp[df.i.tmp$tend>=100,]
ggplot(non.con, aes(x=K0, y=Ki)) + geom_hex()
#ggplot(non.con, aes(x=Frequency, y=Phase)) + geom_density_2d_filled()
counts_df <- non.con %>%
  count(Beta, Frequency, Phase, name = "count")
g.noncon = ggplot(counts_df, aes(x = Phase, y= Frequency, fill = count)) + geom_tile() + scale_fill_viridis()

# check that we don't exceed max possible win in static case
sub0.i = df.i.tmp[df.i.tmp$Frequency == 0,]
ggplot(sub0.i, aes(x=0.5+0.5*sin(Phase), y=S)) + geom_point() + geom_abline() +
  labs(x = "Max possible", y = "Max observed") 

ggplot(df.i.tmp, aes(x=tend, y=S)) + geom_hex()

# what proportion reached a stable point? 90.1% by t=100. 96% by t=1000.
length(which(abs(df.i.tmp$S+df.i.tmp$L-1)<0.01))/nrow(df.i.tmp)
not.eq = df.i.tmp[abs(df.i.tmp$S+df.i.tmp$L-1)>0.01,]
summary(not.eq)
summary(not.eq[not.eq$S==0,])
not.eq$K0+not.eq$Kp+not.eq$Ki+not.eq$Kd

# (o) Null behaviour
df_null.i = df.i[df.i$K0==1 & df.i$Kp==0 & df.i$Ki==0 & df.i$Kd==0,]
g.o.i = ggplot(df_null.i, aes(x=Phase, y=Frequency, fill=S)) + 
  geom_tile() + facet_wrap(~Beta)

# (a) Maximum response value for each set of state variables
df_max_response.i <- df.i %>%
  group_by(Frequency, Beta, Phase) %>%
  summarize(max_response = max(S), .groups = 'drop')

df_improvements.i = df_max_response.i %>%
  left_join(df_null.i, by = c("Frequency" = "Frequency", "Phase" = "Phase", "Beta" = "Beta")) %>%
  rename(null_response = S)

g.a.i = ggplot(df_max_response.i[df_max_response.i$Frequency>0 & df_max_response.i$Beta %in% c(0,1),], 
               aes(x=Phase, y=Frequency, fill=max_response)) + 
  geom_tile() + facet_wrap(~Beta)
g.a.1.i = ggplot(df_improvements.i[df_improvements.i$Beta %in% c(0,1),], 
                 aes(x=Phase, y=Frequency, fill=max_response-null_response)) + 
  geom_tile() + facet_wrap(~Beta)

g.a.i.all = ggplot(df_max_response.i[df_max_response.i$Frequency>0,], aes(x=Phase, y=Frequency, fill=max_response)) + 
  geom_tile() + facet_wrap(~Beta)
g.a.1.i.all = ggplot(df_improvements.i, aes(x=Phase, y=Frequency, fill=max_response-null_response)) + 
  geom_tile() + facet_wrap(~Beta)

# (b) Optimal control variables that give the maximum response for each set of state variables
df_optimal_controls.i <- df.i %>%
  group_by(Frequency, Beta, Phase) %>%
  filter(S == max(S)) %>%
  summarize(
    K0 = mean(K0),
    Kp = mean(Kp),
    Ki = mean(Ki),
    Kd = mean(Kd),
    .groups = 'drop'
  )

b.plots = function(beta) {
  return(
    ggarrange(
      ggplot(df_optimal_controls.i[df_optimal_controls.i$Beta==beta,], aes(x=Phase, y=Frequency, fill=K0)) + geom_tile() + scale_fill_viridis_b(),
      ggplot(df_optimal_controls.i[df_optimal_controls.i$Beta==beta,], aes(x=Phase, y=Frequency, fill=Kp)) + geom_tile() + scale_fill_viridis_b(),
      ggplot(df_optimal_controls.i[df_optimal_controls.i$Beta==beta,], aes(x=Phase, y=Frequency, fill=Ki)) + geom_tile() + scale_fill_viridis_b(),
      ggplot(df_optimal_controls.i[df_optimal_controls.i$Beta==beta,], aes(x=Phase, y=Frequency, fill=Kd)) + geom_tile() + scale_fill_viridis_b()
    )
  )
}
g.b.1.i = b.plots(0)
g.b.2.i = b.plots(1)
g.b.3.i = b.plots(0.01)
g.b.4.i = b.plots(0.1)

# (c) Performance of given control set across conditions
df_mean_performance.i <- df.i %>%
  group_by(Beta, K0, Kp, Ki, Kd) %>%
  summarize(mean_response = mean(S), .groups = 'drop')

c.plots = function(beta) {
  top.0.i = arrange(df_mean_performance.i[df_mean_performance.i$Beta==beta,], desc(mean_response))
  top.0.i$rank = 1:nrow(top.0.i)
  top.p.0.i <- top.0.i %>%
    pivot_longer(cols = c(K0,Kp,Ki,Kd), names_to = "category", values_to = "value")
  c.title = paste0("Best PID strategies (β = ", round(beta, digits=2), ")", collapse="")
  # Plot with stacked bars
  nset = 50*4
  return(
    ggplot() +
    geom_bar(data = top.p.0.i[1:nset,], aes(x = rank, y = value, fill = category), stat = "identity", position = "stack") +
    geom_line(data = unique(top.p.0.i[1:nset,2:3]), aes(x=rank, y=mean_response)) +
    labs(x = "Row ID", y = "Value", title = c.title) +
    theme_minimal()
  )
}

g.c.0.i = c.plots(0)
g.c.1.i = c.plots(1)
g.c.2.i = c.plots(0.01)
g.c.3.i = c.plots(0.1)


###########

if(comparison == TRUE) {
  # specific output comparison
  head(df[df$K0==0 & df$Kp==0.4 & df$Ki==0 & df$Kd==0,])
  head(df.i.tmp[df.i.tmp$K0==0 & df.i.tmp$Kp==0.4 & df.i.tmp$Ki==0 & df.i.tmp$Kd==0,])
  
  # when do approaches fail to converge?
  non.con = df.i.tmp[df.i.tmp$tend>=100,]
  ggplot(non.con, aes(x=K0, y=Ki)) + geom_hex()
  ggplot(non.con, aes(x=Frequency, y=Phase)) + geom_density_2d_filled()
  counts_df <- non.con %>%
    count(Beta, Frequency, Phase, name = "count")
  g.noncon = ggplot(counts_df, aes(x = Phase, y= Frequency, fill = count)) + geom_tile() + scale_fill_viridis()
  
  summary(non.con)
  hist(non.con$Phase)
  hist(non.con$Frequency)
  hist(non.con$Ki)
  hist(non.con$K0)
  hist(non.con$Kp)
  hist(non.con$Kd)
  
  # compare RK and IJ results over the set that do converge
  comparison <- df.i.tmp %>%
    inner_join(df, by = c("Frequency", "Phase", "Beta",
                          "K0", "Ki", "Kp", "Kd"), suffix = c("_df1", "_df2"))
  
  ggplot(comparison, aes(x=S_df1, y=S_df2)) + geom_point()
  
  ggarrange(
    ggplot(df[df$K0==1 & df$Kp==0.4 & df$Ki==0.4 & df$Kd==0,], 
           aes(x=Phase, y=Frequency, fill=S)) + 
      scale_fill_continuous(limits=c(0,1)) +
      geom_tile() + facet_wrap(~ Beta) + ylim(0,2),
    ggplot(df.i[df.i$K0==1 & df.i$Kp==0.4 & df.i$Ki==0.4 & df.i$Kd==0,], 
           aes(x=Phase, y=Frequency, fill=S)) + 
      scale_fill_continuous(limits=c(0,1)) +
      geom_tile() + facet_wrap(~ Beta) + ylim(0,2),
    nrow=2
  )
  
  # null performance
  ggarrange(g.o+xlim(0,6)+ylim(0,1.5), g.o.i+xlim(0,6)+ylim(0,1.5), nrow=2)
  
  # best performance
  ggarrange(g.a+xlim(0,6)+ylim(0,1.5)+
              scale_fill_gradient(limits=c(0,1)), 
            g.a.i+xlim(0,6)+ylim(0,1.5)+
              scale_fill_gradient(limits=c(0,1)), nrow=2)
  
  # improvement over null
  ggarrange(g.a.1+xlim(0,6)+ylim(0,1.5), g.a.1.i+xlim(0,6)+ylim(0,1.5), nrow=2)
  
  # best parameter choices in challenge space
  ggarrange(g.b.1, g.b.2, g.b.1.i, g.b.2.i)
  
  # best overall parameter choices
  ggarrange(g.c.0, g.c.1, g.c.0.i, g.c.1.i)
}


#######

# analytical picture for null model
ares = function(rho, sigma, omega, phi) {
  xi = sqrt(1+2*rho+rho*rho-2*sigma+2*rho*sigma+sigma**2)
  res= (xi*(0.5*sigma*sigma + (0.5 + rho + 0.5*rho*rho + rho*sigma + 0.5*sigma**2) * omega*omega + 0.5*omega**4 +
              (0.5 + 0.5*rho + 0.5*sigma)*sigma*omega*cos(phi) + sigma*(0.5*sigma - 0.5*omega**2)*sin(phi))) /
    (sqrt(-4*sigma + (rho+sigma+1)**2)*(sigma*sigma + (1+2*rho + rho**2 + 2*rho*sigma + sigma**2)*omega**2 + omega**4))
  return(res)
}

# final figures

g.1 =  ggarrange(ggplot(df_null.i[df_null.i$Beta==0,], aes(x=Phase, y=Frequency, fill=S)) +  
                   geom_tile() + scale_fill_viridis(),
                 ggplot(df.i.tmp[df.i.tmp$Beta==0 &
                                   df.i.tmp$K0 == 1 & df.i.tmp$Kp == 0 & 
                                   df.i.tmp$Kp == 0 & df.i.tmp$Kd == 0,], 
                        aes(x=Phase, y=Frequency, 
                            fill=ares(K0, K0, Frequency, Phase))) + 
                   geom_tile() + labs(fill="S") + scale_fill_viridis(),
                 labels=c("A", "B"),
                 nrow=1
)

g.2 = ggarrange(
  g.a.i+xlim(0,6)+ylim(0,1.5)+
    scale_fill_viridis(limits=c(0,1)) + labs(fill="S*"), 
  g.a.1.i+xlim(0,6)+ylim(0,1.5) +
    scale_fill_viridis() + labs(fill="S*-S0"),
  nrow=2, 
  labels=c("A", "B"))

g.3 = ggarrange(g.b.1.i, g.b.2.i, labels=c("A", "B"))

g.4 = ggarrange(g.c.0.i + ylim(0,3.1) + scale_fill_viridis_d(),
                g.c.1.i + ylim(0,3.1) + scale_fill_viridis_d(),
                labels=c("A", "B"))


###

#### some specific examples
# to compare with RK, red is good, green is bad

plot.time.series = function(tdf) {
  return(ggplot(tdf) + 
           geom_line(aes(x=t, y=env), color="grey") +
           geom_line(aes(x=t, y=stateW), color="darkgreen") +
           geom_line(aes(x=t, y=stateL), color="red") +
           geom_line(aes(x=t, y=stateI), color="blue") +
           geom_line(aes(x=t, y=stateA), color="orange") +
           xlim(0,100) +
           theme_light() )
}

tdf.0a = read.csv("example-0a.csv")
tdf.0b = read.csv("example-0b.csv")
tdf.0c = read.csv("example-0c.csv")
tdf.1 = read.csv("example-1.csv")
tdf.2 = read.csv("example-2.csv")
tdf.eps = read.csv("example-noise.csv")
tdf.issue = read.csv("example-issue.csv")

plot.time.series(tdf.eps) + xlim(0,20)

xmax = 50
g.0 = ggarrange(plot.time.series(tdf.0a) + ggtitle("A. No sensing, flat") + labs(y="Values") + xlim(0,xmax), 
                plot.time.series(tdf.0c) + ggtitle("B. No sensing, good phase") + labs(y="Values") + xlim(0,xmax),
                plot.time.series(tdf.0b) + ggtitle("C. No sensing, bad phase") + labs(y="Values") + xlim(0,xmax),
                plot.time.series(tdf.1) + ggtitle("D. Free sensing, bad phase") + labs(y="Values") + xlim(0,xmax), 
                plot.time.series(tdf.2) + ggtitle("E. Costly sensing, bad phase") + labs(y="Values") + xlim(0,xmax),
                plot.time.series(tdf.eps) + ggtitle("F. Stochastic environment") + labs(y="Values") + xlim(0,xmax),
                nrow=3, ncol=2)

g.0a = ggarrange(
  plot.time.series(tdf.issue) + labs(y = "Values") + xlim(0,8),
  g.noncon + labs(fill="Simulations"),
  labels = c("A", "B")
)

sf = 2
png("fig-1.png", width=600*sf, height=500*sf, res=72*sf)
print(g.0)
dev.off()
png("fig-1a.png", width=600*sf, height=300*sf, res=72*sf)
print(g.0a)
dev.off()
png("fig-2.png", width=600*sf, height=300*sf, res=72*sf)
print(g.1)
dev.off()
png("fig-3.png", width=600*sf, height=600*sf, res=72*sf)
print(g.2)
dev.off()
png("fig-4.png", width=800*sf, height=400*sf, res=72*sf)
print(g.3)
dev.off()
png("fig-5.png", width=600*sf, height=300*sf, res=72*sf)
print(g.4)
dev.off()

#### now do the stochastic case, lazily repeating the same code (fix this)

ggplot(df.all[df.all$K0==1 & df.all$Kp==0 & df.all$Ki==0 & df.all$Kd==0,],
       aes(x=Phase, y=Frequency, fill=S)) + geom_tile() + scale_fill_viridis() + facet_wrap(~Epsilon)

df.i = df.all[df.all$Epsilon>0 & df.all$tend <= 100,1:9]

# (o) Null behaviour
df_null.i = df.i[df.i$K0==1 & df.i$Kp==0 & df.i$Ki==0 & df.i$Kd==0,]
g.o.i = ggplot(df_null.i, aes(x=Phase, y=Frequency, fill=S)) + 
  geom_tile() + facet_grid(Epsilon~Beta)

# (a) Maximum response value for each set of state variables
df_max_response.i <- df.i %>%
  group_by(Frequency, Beta, Epsilon, Phase) %>%
  summarize(max_response = max(S), .groups = 'drop')

df_improvements.i = df_max_response.i %>%
  left_join(df_null.i, by = c("Frequency" = "Frequency", "Phase" = "Phase", "Beta" = "Beta", "Epsilon" = "Epsilon")) %>%
  rename(null_response = S)

g.a.i = ggplot(df_max_response.i[df_max_response.i$Frequency>0 & df_max_response.i$Beta %in% c(0,1) &
                                       df_max_response.i$Epsilon == 0.5,], aes(x=Phase, y=Frequency, fill=max_response)) + 
  geom_tile() + facet_grid(Epsilon~Beta)
g.a.1.i = ggplot(df_improvements.i[df_improvements.i$Beta %in% c(0,1) & df_improvements.i$Epsilon == 0.5,], 
                     aes(x=Phase, y=Frequency, fill=max_response-null_response)) + 
  geom_tile() + facet_grid(Epsilon~Beta)

g.a.i.all = ggplot(df_max_response.i[df_max_response.i$Frequency>0,], aes(x=Phase, y=Frequency, fill=max_response)) + 
  geom_tile() + facet_grid(Epsilon~Beta)
g.a.1.i.all = ggplot(df_improvements.i, aes(x=Phase, y=Frequency, fill=max_response-null_response)) + 
  geom_tile() + facet_grid(Epsilon~Beta)

# (b) Optimal control variables that give the maximum response for each set of state variables
df_optimal_controls.i <- df.i %>%
  group_by(Frequency, Beta, Epsilon, Phase) %>%
  filter(S == max(S)) %>%
  summarize(
    K0 = mean(K0),
    Kp = mean(Kp),
    Ki = mean(Ki),
    Kd = mean(Kd),
    .groups = 'drop'
  )

b.plots.stoch = function(beta, epsilon) {
  return(
    ggarrange(
      ggplot(df_optimal_controls.i[df_optimal_controls.i$Beta==beta & df_optimal_controls.i$Epsilon==epsilon,], aes(x=Phase, y=Frequency, fill=K0)) + geom_tile() + scale_fill_viridis_b(),
      ggplot(df_optimal_controls.i[df_optimal_controls.i$Beta==beta & df_optimal_controls.i$Epsilon==epsilon,], aes(x=Phase, y=Frequency, fill=Kp)) + geom_tile() + scale_fill_viridis_b(),
      ggplot(df_optimal_controls.i[df_optimal_controls.i$Beta==beta & df_optimal_controls.i$Epsilon==epsilon,], aes(x=Phase, y=Frequency, fill=Ki)) + geom_tile() + scale_fill_viridis_b(),
      ggplot(df_optimal_controls.i[df_optimal_controls.i$Beta==beta & df_optimal_controls.i$Epsilon==epsilon,], aes(x=Phase, y=Frequency, fill=Kd)) + geom_tile() + scale_fill_viridis_b()
    )
  )
}
g.b.1.i = b.plots.stoch(0, 0.5)
g.b.2.i = b.plots.stoch(1, 0.5)
g.b.3.i = b.plots.stoch(0, 0.05)
g.b.4.i = b.plots.stoch(1, 0.05)
g.b.5.i = b.plots.stoch(0, 0.2)
g.b.6.i = b.plots.stoch(1, 0.2)

# (c) Performance of given control set across conditions
df_mean_performance.i <- df.i %>%
  group_by(Beta, Epsilon, K0, Kp, Ki, Kd) %>%
  summarize(mean_response = mean(S), .groups = 'drop')

c.plots.stoch = function(beta, epsilon) {
  top.0.i = arrange(df_mean_performance.i[df_mean_performance.i$Beta==beta & df_mean_performance.i$Epsilon==epsilon,], 
                    desc(mean_response))
  top.0.i$rank = 1:nrow(top.0.i)
  top.p.0.i <- top.0.i %>%
    pivot_longer(cols = c(K0,Kp,Ki,Kd), names_to = "category", values_to = "value")
  c.title = paste0("Best PID strategies (β = ", round(beta, digits=2), ", ε = ", round(epsilon, digits=2), ")", collapse="")
  # Plot with stacked bars
  nset = 50*4
  return(
    ggplot() +
    geom_bar(data = top.p.0.i[1:nset,], aes(x = rank, y = value, fill = category), stat = "identity", position = "stack") +
    geom_line(data = unique(top.p.0.i[1:nset,3:4]), aes(x=rank, y=mean_response)) +
    labs(x = "Row ID", y = "Value", title = c.title) +
    theme_minimal() + ylim(0,3.1) + scale_fill_viridis_d()
  )
}

g.c.0.i = c.plots.stoch(0, 0.5)
g.c.1.i = c.plots.stoch(1, 0.5)
g.c.2.i = c.plots.stoch(0.01, 0.5)
g.c.3.i = c.plots.stoch(0.1, 0.5)

# final figures -- stochastic

g.1 =  ggarrange(ggplot(df_null.i[df_null.i$Beta==0,], aes(x=Phase, y=Frequency, fill=S)) +  
                   geom_tile() + scale_fill_viridis(),
                 ggplot(df.i.tmp[df.i.tmp$Beta==0 &
                                   df.i.tmp$K0 == 1 & df.i.tmp$Kp == 0 & 
                                   df.i.tmp$Kp == 0 & df.i.tmp$Kd == 0,], 
                        aes(x=Phase, y=Frequency, 
                            fill=ares(K0, K0, Frequency, Phase))) + 
                   geom_tile() + labs(fill="S") + scale_fill_viridis(),
                 labels=c("A", "B"),
                 nrow=1
)

g.2 = ggarrange(
  g.a.i+xlim(0,6)+ylim(0,1.5)+
    scale_fill_viridis(limits=c(0,1)) + labs(fill="S*"), 
  g.a.1.i+xlim(0,6)+ylim(0,1.5) +
    scale_fill_viridis() + labs(fill="S*-S0"),
  nrow=2, 
  labels=c("A", "B"))

g.3 = ggarrange(g.b.1.i, g.b.2.i, labels=c("A", "B"))

g.4 = ggarrange(g.c.0.i ,
                g.c.1.i ,
                labels=c("A", "B"))

g.s1 = ggarrange(
  g.a.i.all+xlim(0,6)+ylim(0,1.5)+
    scale_fill_viridis(limits=c(0,1)) + labs(fill="S*"), 
  g.a.1.i.all+xlim(0,6)+ylim(0,1.5) +
    scale_fill_viridis() + labs(fill="S*-S0"),
  nrow=2, 
  labels=c("A", "B"))

g.s2 = ggarrange(c.plots.stoch(0,0.05), c.plots.stoch(0.1,0.05), c.plots.stoch(1,0.05),
          c.plots.stoch(0,0.2), c.plots.stoch(0.1,0.2), c.plots.stoch(1,0.2),
          c.plots.stoch(0,0.5), c.plots.stoch(0.1,0.5), c.plots.stoch(1,0.5),
          nrow=3, ncol=3)

###

sf = 2
png("fig-stoch-1.png", width=300*sf, height=600*sf, res=72*sf)
print(g.0)
dev.off()
png("fig-stoch-1a.png", width=600*sf, height=300*sf, res=72*sf)
print(g.0a)
dev.off()
png("fig-stoch-2.png", width=600*sf, height=300*sf, res=72*sf)
print(g.1)
dev.off()
png("fig-stoch-3.png", width=600*sf, height=600*sf, res=72*sf)
print(g.2)
dev.off()
png("fig-stoch-4.png", width=800*sf, height=400*sf, res=72*sf)
print(g.3)
dev.off()
png("fig-stoch-5.png", width=600*sf, height=300*sf, res=72*sf)
print(g.4)
dev.off()
png("fig-stoch-s1.png", width=600*sf, height=800*sf, res=72*sf)
print(g.s1)
dev.off()
png("fig-stoch-s2.png", width=800*sf, height=800*sf, res=72*sf)
print(g.s2)
dev.off()

