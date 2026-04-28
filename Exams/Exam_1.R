##read in cleaned_covid_data.csv
df=read.csv('BIOL3100_Exams/Exam_1/data/cleaned_covid_data.csv')

##Subset the data set to just show states that begin with “A” and
##save this as an object called A_states.
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
str(df)
A_States=df %>%
  filter(str_starts(Province_State,'A'))
view(A_States)

##Create a plot of that subset showing Deaths over time, with a
##separate facet for each state.
A_States %>%
  mutate(Last_Update=as.POSIXct(Last_Update)) %>%
  ggplot(aes(x=Last_Update,y=Deaths),scale)+
  geom_point()+
  geom_smooth(method='loess',se=FALSE)+
  facet_wrap(~Province_State,scales='free')

##(Back to the full dataset) Find the “peak” of Case_Fatality_Ratio for
##each state and save this as a new data frame object
##called state_max_fatality_rate.
state_max_fatality_rate=df %>%
  group_by(Province_State) %>%
  summarise(Maximum_Fatality_Ratio=max(Case_Fatality_Ratio,na.rm = T)) %>%
  arrange(desc(Maximum_Fatality_Ratio))

##Use that new data frame from task IV to create another plot.
state_max_fatality_rate %>%
  mutate(Province_State=factor(Province_State,
                               levels=Province_State[order(-Maximum_Fatality_Ratio)])) %>%
  ggplot(aes(x=Province_State,y=Maximum_Fatality_Ratio))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5,hjust=1))+
  labs(x='State',y='Maximum Fatality Rate')

##Using the FULL data set, plot cumulative deaths for the entire US over time
df%>%
  mutate(Last_Update=as.Date(Last_Update)) %>%
  arrange(Last_Update) %>%
  group_by() %>%
  summarise(Date=Last_Update,Total_Deaths=cumsum(Deaths))%>%
  ungroup() %>%
  ggplot(aes(x=Date,y=Total_Deaths))+
  geom_line()+
  labs(title='Covid Deaths in US',x='Time',y='Total Deaths')+
  theme_minimal()
