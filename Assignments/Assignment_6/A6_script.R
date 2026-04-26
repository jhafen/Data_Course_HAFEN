library(dplyr)
library(tidyverse)
library(ggplot2)
dat=read.csv('Data/BioLog_Plate_Data.csv')
str(dat)
view(dat)
dat %>%
  mutate(Sample.type=case_when(Sample.ID %in% c('Clear_Creek', 'Waste_Water') ~ 'water',
                               Sample.ID %in% c('Soil_1', 'Soil_2') ~ 'soil'))%>%
  pivot_longer(cols = 6:8, names_to = 'Time', values_to = 'Abs') %>%
  separate(Time,into=c('z','Time'),sep='_',convert=T) %>%
  select(-z) %>%
  filter(Dilution==0.1) %>%
  ##View()
  ggplot(aes(x=Time,y=Abs,color=Sample.type))+
  geom_smooth(se=FALSE)+
  facet_wrap(~Substrate)+
  ylab('Absorbance')+
  ggtitle('Dilution of 0.1')

library(gapminder)
library(gganimate)
dat %>%
  mutate(Sample.type=case_when(Sample.ID %in% c('Clear_Creek', 'Waste_Water') ~ 'water',
                               Sample.ID %in% c('Soil_1', 'Soil_2') ~ 'soil'))%>%
  pivot_longer(cols = 6:8, names_to = 'Time', values_to = 'Abs') %>%
  separate(Time,into=c('z','Time'),sep='_',convert=T) %>%
  select(-z) %>%
  filter(Substrate=='Itaconic Acid') %>%
  #View()
  group_by(Dilution, Sample.ID, Time) %>%
  summarise(mean_abs=mean(Abs,.groups='drop'))%>%
  ggplot(aes(x=Time,y=mean_abs,color=Sample.ID,group = Sample.ID))+
  geom_line(linewidth = 1)+
  labs(title = 'Itaconic Acid Absorbance',x='Time',y='Mean_Absorbance')+
  facet_wrap(~Dilution,ncol=3)+
  theme_minimal()+
  transition_reveal(Time)
anim_save('Assignments/Assignment_6/A6_anim.gif')
