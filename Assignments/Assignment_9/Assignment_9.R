df=read.csv('Data/GradSchool_Admissions.csv')
view(df)
str(df)
adm_mod=glm(data = df,formula=as.logical(admit)~gre,family = 'binomial')
summary(adm_mod)
report(adm_mod)
library(GGally)
df %>%
  ggpairs()
ggpairs(df)

adm_mod1=glm(data = df,formula=as.logical(admit)~gpa,family = 'binomial')
summary(adm_mod1)
report(adm_mod1)

adm_mod2=glm(data=df,formula = as.logical(admit)~gre*gpa,family = 'binomial')
summary(adm_mod2)
report(adm_mod2)
df$pred=predict(adm_mod2, df, type = 'response')
max(df$pred)

df %>%
  ggplot(aes(x=gpa, y=pred,color=factor(rank)))+
  geom_point()+
  geom_smooth()
df %>%
  ggplot(aes(x=gre, y=pred,color=factor(rank)))+
  geom_point()+
  geom_smooth()

adm_mod3=glm(data=df,formula = as.logical(admit)~gre*gpa+rank,
             family = 'binomial')
models=c(adm_mod,adm_mod1,adm_mod2,adm_mod3)
summary(models)
report(models)
df$pred=predict(adm_mod3, df, type = 'response')
max(df$pred)
min(df$pred)
mean(df$pred)

df %>%
  ggplot(aes(x=admit, y=pred,color=factor(rank)))+
  geom_point()+
  geom_smooth()
df %>%
  ggplot(aes(x=gre, y=pred,color=factor(rank)))+
  geom_point()+
  geom_smooth()
##rank appears to be the strongest factor
df %>%
  ggplot(aes(x=gpa, y=pred,color=admit))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~rank)
df %>%
  ggplot(aes(x=gre, y=pred,color=admit))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~rank)
compare_models(adm_mod,adm_mod1,adm_mod2,adm_mod3)
compare_performance(adm_mod,adm_mod1,adm_mod2,adm_mod3) %>%
  plot()
MASS::stepAIC(adm_mod3)

## R was being weird (maybe because I was working on the html at the same time),
##so I reloaded the libraries and models
library(broom)
library(tidyverse)
library(purrr)
library(modelr)
library(tidyverse)
library(easystats)
library(ggplot2)
library(GGally)
library(modelr)
library(dplyr)
adm_mod=glm(data = df,formula=as.logical(admit)~gre,family = 'binomial')

adm_mod1=glm(data = df,formula=as.logical(admit)~gpa,family = 'binomial')

adm_mod2=glm(data=df,formula = as.logical(admit)~gre*gpa,family = 'binomial')

adm_mod3=glm(data=df,formula = as.logical(admit)~gre*gpa+rank,family = 'binomial')

##this didn't seem to work the way I wanted
df %>% 
  gather_predictions(adm_mod,adm_mod1,adm_mod2,adm_mod3) %>% 
  ggplot(aes(x=admit,y=pred,color=model)) +
  geom_segment(aes(x=0,y=0,xend=1,yend=1),linetype=2, color="black",alpha=.5) +
  geom_smooth(method = "glm",se=FALSE) +
  facet_wrap(~rank) +
  theme_minimal() +
  scale_color_viridis_d() +
  labs(title = "Predictions vs observations")
