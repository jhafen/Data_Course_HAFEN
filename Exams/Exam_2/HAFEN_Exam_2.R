##1. Read in the unicef data (10 pts)
df=read.csv('BIOL3100_Exams/Exam_2/unicef-u5mr.csv')

##2. Get it into tidy format (10 pts)
library(skimr)
str(df)
tidydf=df %>%
  pivot_longer(cols=2:67,names_to = 'Year',names_prefix = 'U5MR\\.',
               values_to = 'U5MR',names_transform = list(Year=as.numeric))%>%
  view()

##3. Plot each country’s U5MR over time (20 points)
tidydf%>%
  ggplot(aes(x=Year,y=U5MR,group=CountryName))+
  geom_smooth(method='loess',se=FALSE,na.rm=FALSE,colour='black',linewidth=0.6)+
  facet_wrap(~Continent)

##4. Save this plot as LASTNAME_Plot_1.png (5 pts)
ggsave('HAFEN_Plot_1.png',path = 'Exams/Exam_2/')

##5. Create another plot that shows the mean U5MR for all the countries
## within a given continent at each year (20 pts)
tidydf %>%
  group_by(Continent,Year)%>%
  summarise(Mean_U5MR=mean(U5MR,na.rm=TRUE))%>%
  ggplot(aes(x=Year,y=Mean_U5MR,color=Continent))+
  geom_line(linewidth = 2)

##6. Save that plot as LASTNAME_Plot_2.png (5 pts)
ggsave('HAFEN_Plot_2.png',path = 'Exams/Exam_2/')

##7. Create three models of U5MR (20 pts)
mod1=glm(U5MR~Year,data=tidydf)
mod2=glm(U5MR~Year+Continent,data=tidydf)
mod3=glm(U5MR~Year*Continent,data=tidydf)

##8. Compare the three models with respect to their performance
compare_performance(mod1,mod2,mod3)
plot(compare_performance(mod1,mod2,mod3))
#mod3 appears to be the best model as it produced the best results in the model indices

##9. Plot the 3 models’ predictions like so: (10 pts)
tidydf$mod1=predict(mod1,tidydf)
tidydf$mod2=predict(mod2,tidydf)
tidydf$mod3=predict(mod3,tidydf)
tidydf%>%
  pivot_longer(cols=c(mod1,mod2,mod3),names_to='Model',
               values_to='Predicted_U5MR')%>%
  ggplot(aes(x=Year,y=Predicted_U5MR,color=Continent))+
  geom_line(linewidth=1)+
  facet_wrap(~Model)+
  labs(title='Model Predictions',x='Year',y='Predicted U5MR')

##10. BONUS - Using your preferred model, predict what the U5MR would be for
##Ecuador in the year 2020. The real value for Ecuador for 2020 was 13
##under-5 deaths per 1000 live births. How far off was your model prediction?
ecuador=tibble(CountryName='Ecuador',Year=2020,Continent='Americas',
               Region='South America')
predict(mod3,newdata = ecuador)
13-predict(mod3,newdata = ecuador)
#The predicted value was -10.58018, which is impossible, and with an real
#value of 13 isn't exactly close (difference of ~23), but isn't way off.
mod4=glm(U5MR~Year*CountryName,data=tidydf)
predict(mod4,newdata = ecuador)
#mod 4 gave a value of -22.82782, nearly the difference between the real number
#and the mod3 prediction
mod5=glm(U5MR~Year*Region,data=tidydf)
predict(mod5,newdata = ecuador)
#mod5 was roughly the same as mod3. Further data, such as economic data,
#may help produce a more accurate model