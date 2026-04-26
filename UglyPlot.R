install.packages('ggimage')
install.packages('ggforce')
install.packages('patchwork')
install.packages('ggpubr')
install.packages('gganimate')
library(ggplot2)
library(tidyverse)
library(gapminder)
library(gganimate)
library(gifski)
library(av)
library(dplyr)
library(GGally)
library(ggimage)
library(ggforce)
library(patchwork)
library(ggpubr)
view(gapminder)
install.packages('jpeg')
library(jpeg)



names(gapminder)
gapminder %>%
  ggplot(aes(x=country,
             y=year,
             color=gdpPercap,
             shape = continent))+
  geom_point(color='#3CFF00', size = 41)+
  theme(axis.text = element_text(angle = 206),
        axis.title.x = element_text(angle = 109),axis.title.y = element_text(angle = 180),
        plot.background = element_rect(fill='#3CFF00'),
        panel.background=element_rect(fill = '#FF00FF'),
        axis.title = element_text(size = 11,color = '#FF00FF'),
        axis.text.x = element_text(size = 3,color = '#FF00FF'),
        axis.text.y = element_text(size = 6,color = '#FF00FF'),
        plot.title = element_text(size = 15,color='#FF00FF'))+
  geom_text(aes(label=lifeExp))+
  transition_time(pop)+
  labs(title='Mess')
anim_save('uglyplot_jh.gif')
