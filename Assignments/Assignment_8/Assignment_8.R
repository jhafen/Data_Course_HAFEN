library(modelr)
library(easystats)
library(broom)
library(tidyverse)
library(fitdistrplus)
library(skimr)
library(lindia)
library(caret)
library(GGally)
library(patchwork)

df=read.csv('Data/mushroom_growth.csv')
view(df)

#Models
modl=lm(GrowthRate~Light+Species,data=df)
modn=lm(GrowthRate~Nitrogen+Species,data = df)
modh=lm(GrowthRate~Humidity+Species,data=df)
modt=lm(GrowthRate~Temperature+Species,data=df)
mod1=lm(GrowthRate~Nitrogen*Temperature+Species,data=df)
mod2=lm(GrowthRate~Nitrogen*Humidity*Temperature+Species,data=df)
mod3=lm(GrowthRate~Light+Nitrogen+Humidity*Temperature+Species,data=df)
mods=list(modl=modl,modn=modn,modh=modh,modt=modt,mod1=mod1,mod2=mod2,mod3=mod3)

gmodl=glm(GrowthRate~Light+Species,data=df)
gmodn=glm(GrowthRate~Nitrogen+Species,data = df)
gmodh=glm(GrowthRate~Humidity+Species,data=df)
gmodt=glm(GrowthRate~Temperature+Species,data=df)
gmod1=glm(GrowthRate~Nitrogen*Temperature+Species,data=df)
gmod2=glm(GrowthRate~Nitrogen*Humidity*Temperature+Species,data=df)
gmod3=glm(GrowthRate~Light+Nitrogen+Humidity*Temperature+Species,data=df)
gmods=list(gmodl=gmodl,gmodn=gmodn,gmodh=gmodh,gmodt=gmodt,gmod1=gmod1,
           gmod2=gmod2,gmod3=gmod3)

#Model summaries
summary(modl)
summary(modn)
summary(modh)
summary(modt)
summary(mod1)
summary(mod2)
summary(mod3)

summary(gmodl)
summary(gmodn)
summary(gmodh)
summary(gmodt)
summary(gmod1)
summary(gmod2)
summary(gmod3)

#Model plots
#modl
ggplot(df, aes(x=Light,y=GrowthRate,color=Species)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()
#modn
ggplot(df, aes(x=Nitrogen,y=GrowthRate,color=Species)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()
#modh
ggplot(df, aes(x=Humidity,y=GrowthRate,color=Species)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()
#modt
ggplot(df, aes(x=Temperature,y=GrowthRate,color=Species)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()

#gmodl
ggplot(df, aes(x=Light,y=GrowthRate,color=Species)) + 
  geom_point() + 
  geom_smooth(method = "glm") +
  theme_minimal()
#gmodn
ggplot(df, aes(x=Nitrogen,y=GrowthRate,color=Species)) + 
  geom_point() + 
  geom_smooth(method = "glm") +
  theme_minimal()
#gmodh
ggplot(df, aes(x=Humidity,y=GrowthRate,color=Species)) + 
  geom_point() + 
  geom_smooth(method = "glm") +
  theme_minimal()
#gmodt
ggplot(df, aes(x=Temperature,y=GrowthRate,color=Species)) + 
  geom_point() + 
  geom_smooth(method = "glm") +
  theme_minimal()

#model diagnostics
gg_diagnose(modl)
gg_diagnose(modn)
gg_diagnose(modh)
gg_diagnose(modt)
gg_diagnose(mod1)
gg_diagnose(mod2)
gg_diagnose(mod3)

##glm models didn't work
gg_diagnose(gmodl)
gg_diagnose(gmodn)
gg_diagnose(gmodh)
gg_diagnose(gmodt)
gg_diagnose(gmod1)
gg_diagnose(gmod2)
gg_diagnose(gmod3)

#model comp
anova(modl,modn)
anova(modl,modh)
anova(modl,modt)
anova(modl,mod1)
anova(modl,mod2)
anova(modl,mod3)
anova(modl,gmodl)
anova(gmod2,gmod3)
anova(mod2,mod3)

#best fit by residuals
mean(modl$residuals^2)
mean(modn$residuals^2)
mean(modh$residuals^2)
mean(modt$residuals^2)
mean(mod1$residuals^2)
mean(mod2$residuals^2)
mean(mod3$residuals^2)

mean(gmodl$residuals^2)
mean(gmodn$residuals^2)
mean(gmodh$residuals^2)
mean(gmodt$residuals^2)
mean(gmod1$residuals^2)
mean(gmod2$residuals^2)
mean(gmod3$residuals^2)
##mod3 is best but still not great, glm and lm give same residual values

##None of the models are particularly good, but mod3 is the best based on the
##mean residuals

#Predictions
dfmodl = add_predictions(df,modl)
dfmodl
dfmodn = add_predictions(df,modn)
dfmodn
dfmodh = add_predictions(df,modh)
dfmodh
dfmodt = add_predictions(df,modt)
dfmodt
dfmod1 = add_predictions(df,mod1)
dfmod1
dfmod2 = add_predictions(df,mod2)
dfmod2
dfmod3 = add_predictions(df,mod3)
dfmod3

modpred=gather_predictions(df, modl,modn,modh,modt,mod1,mod2,mod3)
modpred
skim(modpred)
names(modpred)

#plots of models
p1=ggplot(dfmod3,aes(x=Nitrogen,y=GrowthRate,color=Species))+
  geom_point(aes(y=GrowthRate),alpha=0.5,size=2)+
  geom_point(aes(y=pred),color='black')+
  theme_bw()
p1

p1+geom_segment(aes(y=GrowthRate,xend=Nitrogen,yend=pred),linetype=2,
                color='black',alpha=.5)
ggplot(modpred,aes(x=GrowthRate,color=Species))+
  geom_point(aes(y=GrowthRate),alpha=.25)+
  geom_point(aes(y=pred),color='black')+
  facet_wrap(~model)+
  theme_bw()

map(mods,performance) %>% reduce(full_join)
df %>% 
  gather_residuals(modl,modn,modh,modt,mod1,mod2,mod3) %>% 
  ggplot(aes(x=model,y=resid,fill=model)) +
  geom_boxplot(alpha=.5) +
  geom_point() + 
  theme_minimal()

df %>% 
  gather_predictions(modl,modn,modh,modt,mod1,mod2,mod3) %>% 
  ggplot(aes(x=Nitrogen,y=GrowthRate)) +
  geom_point(size=3) +
  geom_point(aes(y=pred,color=model)) +
  geom_smooth(aes(y=pred,color=model)) +
  theme_minimal()



newdf = data.frame(Nitrogen = c(15,50,55,60,65))
hyp_preds=data.frame(Nitrogen = newdf$Nitrogen,pred = pred)
df$PredictionType="Real"
hyp_preds$PredictionType="Hypothetical"
fullpreds=full_join(df,hyp_preds)
ggplot(fullpreds,aes(x=Nitrogen,y=pred,color=PredictionType)) +
  geom_point() +
  geom_point(aes(y=GrowthRate),color="Black",alpha=0.25) +
  theme_minimal()


#Loess model plots
#modl
ggplot(df, aes(x=Light,y=GrowthRate,color=Species)) + 
  geom_point() + 
  geom_smooth(method = "loess") +
  theme_minimal()
#modn
ggplot(df, aes(x=Nitrogen,y=GrowthRate,color=Species)) + 
  geom_point() + 
  geom_smooth(method = "loess") +
  theme_minimal()
#modh
ggplot(df, aes(x=Humidity,y=GrowthRate,color=Species)) + 
  geom_point() + 
  geom_smooth(method = "loess") +
  theme_minimal()
#modt
ggplot(df, aes(x=Temperature,y=GrowthRate,color=Species)) + 
  geom_point() + 
  geom_smooth(method = "loess") +
  theme_minimal()

##Non-linear relationships
nlr=read.csv('Data/non_linear_relationship.csv')
view(nlr)
ggplot(nlr,aes(x=predictor,y=response))+
  geom_point()+
  geom_smooth(method='lm')+
  theme_minimal()
expmod=nls(response~a*exp(b*predictor),data=nlr,start=list(a = 1, b = 1))
summary(expmod)
plot(nlr$predictor, nlr$response,
     main = "Exponential Model Fit",
     xlab = "Predictor (x)",
     ylab = "Response (y)",
     pch = 19,
     col = "blue")
xval=seq(min(nlr$predictor),max(nlr$predictor),length.out = 100)
yval=predict(expmod,newdata=data.frame(predictor=xval))
lines(xval,yval,col="red",lwd = 2)
rsq=1-sum(resid(expmod)^2)/sum((nlr$response-mean(nlr$response))^2)
cat("Approximate R-squared for nls model:", rsq, "\n")
