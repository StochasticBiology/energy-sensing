library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)

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

df.0 = read.csv("optimal_results_with_K0.csv")
df.01 = read.csv("optimal_results_with_K01.csv")
df.02 = read.csv("optimal_results_with_K02.csv")
df.03 = read.csv("optimal_results_with_K03.csv")
df.04 = read.csv("optimal_results_with_K04.csv")

df = df.03

# (o) Null behaviour
df_null = df[df$K0==1 & df$Kp==0 & df$Ki==0 & df$Kd==0,]
g.o = ggplot(df_null, aes(x=Phase, y=Frequency, fill=S)) + 
               geom_tile() + facet_wrap(~Beta)

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

ggarrange(
  ggplot(df_optimal_controls[df_optimal_controls$Beta==0,], aes(x=Phase, y=Frequency, fill=K0)) + geom_tile(),
  ggplot(df_optimal_controls[df_optimal_controls$Beta==0,], aes(x=Phase, y=Frequency, fill=Kp)) + geom_tile(),
  ggplot(df_optimal_controls[df_optimal_controls$Beta==0,], aes(x=Phase, y=Frequency, fill=Ki)) + geom_tile(),
  ggplot(df_optimal_controls[df_optimal_controls$Beta==0,], aes(x=Phase, y=Frequency, fill=Kd)) + geom_tile()
)

ggarrange(
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
  labs(x = "Row ID", y = "Value", title = "Stacked Bar Plot for Each Row") +
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
  labs(x = "Row ID", y = "Value", title = "Stacked Bar Plot for Each Row") +
  theme_minimal()

##### IJ simulations

df.i.tmp = read.csv("../redo-out-rk.csv")
head(df.i.tmp)
colnames(df.i.tmp) = c("Beta", "Frequency", "Phase", "K0", "Kp", "Ki", "Kd", "S", "L", "delta", "tend")
df.i = df.i.tmp[,1:8]

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

g.a.i = ggplot(df_max_response.i[df_max_response.i$Frequency>0,], aes(x=Phase, y=Frequency, fill=max_response)) + 
  geom_tile() + facet_wrap(~Beta)
g.a.1.i = ggplot(df_improvements.i, aes(x=Phase, y=Frequency, fill=max_response-null_response)) + 
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

ggarrange(
  ggplot(df_optimal_controls.i[df_optimal_controls.i$Beta==0,], aes(x=Phase, y=Frequency, fill=K0)) + geom_tile(),
  ggplot(df_optimal_controls.i[df_optimal_controls.i$Beta==0,], aes(x=Phase, y=Frequency, fill=Kp)) + geom_tile(),
  ggplot(df_optimal_controls.i[df_optimal_controls.i$Beta==0,], aes(x=Phase, y=Frequency, fill=Ki)) + geom_tile(),
  ggplot(df_optimal_controls.i[df_optimal_controls.i$Beta==0,], aes(x=Phase, y=Frequency, fill=Kd)) + geom_tile()
)

ggarrange(
  ggplot(df_optimal_controls[df_optimal_controls$Beta==1,], aes(x=Phase, y=Frequency, fill=K0)) + geom_tile(),
  ggplot(df_optimal_controls[df_optimal_controls$Beta==1,], aes(x=Phase, y=Frequency, fill=Kp)) + geom_tile(),
  ggplot(df_optimal_controls[df_optimal_controls$Beta==1,], aes(x=Phase, y=Frequency, fill=Ki)) + geom_tile(),
  ggplot(df_optimal_controls[df_optimal_controls$Beta==1,], aes(x=Phase, y=Frequency, fill=Kd)) + geom_tile()
)

# (c) Performance of given control set across conditions
df_mean_performance.i <- df.i %>%
  group_by(Beta, K0, Kp, Ki, Kd) %>%
  summarize(mean_response = mean(S), .groups = 'drop')

top.0.i = arrange(df_mean_performance.i[df_mean_performance.i$Beta==0,], desc(mean_response))
top.0.i$rank = 1:nrow(top.0.i)
top.p.0.i <- top.0.i %>%
  pivot_longer(cols = c(K0,Kp,Ki,Kd), names_to = "category", values_to = "value")

# Plot with stacked bars
nset = 50*4
g.c.0.i = ggplot() +
  geom_bar(data = top.p.0.i[1:nset,], aes(x = rank, y = value, fill = category), stat = "identity", position = "stack") +
  geom_line(data = unique(top.p.0.i[1:nset,2:3]), aes(x=rank, y=mean_response)) +
  labs(x = "Row ID", y = "Value", title = "Stacked Bar Plot for Each Row") +
  theme_minimal()

top.1.i = arrange(df_mean_performance.i[df_mean_performance.i$Beta==1,], desc(mean_response))
top.1.i$rank = 1:nrow(top.1.i)
top.p.1.i <- top.1.i %>%
  pivot_longer(cols = c(K0,Kp,Ki,Kd), names_to = "category", values_to = "value")

# Plot with stacked bars
nset = 50*4
g.c.1.i = ggplot() +
  geom_bar(data = top.p.1.i[1:nset,], aes(x = rank, y = value, fill = category), stat = "identity", position = "stack") +
  geom_line(data = unique(top.p.1.i[1:nset,2:3]), aes(x=rank, y=mean_response)) +
  labs(x = "Row ID", y = "Value", title = "Stacked Bar Plot for Each Row") +
  theme_minimal()

###########

# null performance
ggarrange(g.o+xlim(0,6)+ylim(0,1.5), g.o.i+xlim(0,6)+ylim(0,1.5), nrow=2)

# best performance
ggarrange(g.a+xlim(0,6)+ylim(0,1.5), g.a.i+xlim(0,6)+ylim(0,1.5), nrow=2)

# improvement over null
ggarrange(g.a.1+xlim(0,6)+ylim(0,1.5), g.a.1.i+xlim(0,6)+ylim(0,1.5), nrow=2)

# best overall parameter choices
ggarrange(g.c.0, g.c.1, g.c.0.i, g.c.1.i)


#######

df.03[df.03$Frequency == 0.2 & df.03$Beta == 0 & df.03$Phase == 0 &
        df.03$K0 == 1 & df.03$Kp == 0 & df.03$Ki == 0 & df.03$Kd == 0,]

tmp = df.03[df.03$Frequency == 0.2 & df.03$Beta == 0 & df.03$Phase == 0 ,]
tmp[tmp$S == max(tmp$S),]

df_null[df_null$Frequency == 0.2 & df_null$Beta == 0 & df_null$Phase == 0 ,]

df_max_response[df_max_response$Frequency == 0.2 & df_max_response$Beta == 0 & df_max_response$Phase == 0 ,]
