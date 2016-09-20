#Clean list

rm(list = ls())

#Load required packages

require(tidyverse)
require(clipr)
require(plotly)

#=============#
#Exercise time!
#=============#

#Exercise 1/Sheet 4 - Averaging data

pollster_averages <- read_csv("pollster_averages.csv")

pollster_averages <- pollster_averages %>% 
  mutate(clinton_margin_diff=(Clinton_w_3rd-Trump_w_3rd)-(Clinton_wo_3rd-Trump_wo_3rd))

pollster_averages.T = setNames(data.frame(t(pollster_averages[,-1])), as.matrix(pollster_averages[,1]))

pollster_averages.T$Type <- rownames(pollster_averages.T)

pollster_averages.T <- pollster_averages.T %>% 
  select(5,1:4) %>% 
  mutate(average=(CNN+Fox_News+Marist+YouGov)/4)

#Exercise 2/Sheet 5 - Plotting data

states_2012_2016 <- read_csv("states_2012_2016.csv")

states_2012_2016 %>% ggplot(aes(x=`2012`,y=`2016`))+geom_point()

states_2012_2016 %>% ggplot(aes(x=`2012`,y=`2016`))+geom_point()+geom_abline()

#states_2012_2016 %>% plot_ly(x=`2012`,y=`2016`,text=rownames(),mode="markers")

#Exercise 3/Sheet 2 - Charting over time

black_gop_support <- read_csv("black_gop_support.csv")

black_gop_support %>% ggplot(aes(x=year,y=black_gop_support))+geom_point()

black_gop_support %>% ggplot(aes(x=year,y=black_gop_support))+geom_point()+geom_smooth()

#Exercise 4/Sheet 1 - Regression

dem_share_by_state <- read_csv("dem_share_by_state.csv")

reg.lm <- lm(clinton_share_primary ~ obama_share_2012 + black_pct_by_state,data = dem_share_by_state)

summary(reg.lm)

#Not sure what else Harry wants to do with this here...

#Excerise 5/Sheet 3 - Doing your own crosstab analysis

trump_conservatives <- read_tsv("trump-conservatives.tsv")

trump_summary_1 <- trump_conservatives %>% 
  group_by(conservative) %>% 
  summarise(weighted.mean(trump,weight))

trump_summary_2 <- trump_conservatives %>% 
  group_by(conservative) %>% 
  summarise(count=n(),wgt=sum(weight),pct=weighted.mean(trump,weight)) %>% 
  mutate(group_0_1_or_2=ifelse(conservative<=2,1,0),
         group_3=ifelse(conservative==3,1,0),
         group_4=ifelse(conservative==4,1,0)) %>% 
  select(-1) %>% 
  gather("group_type","value",4:6) %>% 
  filter(value==1) %>% 
  group_by(group_type) %>% 
  summarise(n=sum(count),sum(wgt),wtd_pct=weighted.mean(pct,wgt)) %>% 
  select(-`sum(wgt)`)


