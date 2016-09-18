#Clean list
rm(list = ls())

require(tidyverse)
require(clipr)

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


