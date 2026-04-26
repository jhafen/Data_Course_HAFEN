library(ggplot2)
library(tidyverse)
library(dplyr)
atz=read.csv("Assignments/Assignment_4/atrazine_assay.csv")
atz%>%
  ggplot(aes(x=Time,y=absorbance,color=Enzyme))+
  geom_point()+
  geom_smooth(method = lm,se=F)
