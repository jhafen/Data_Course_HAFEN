ut_rel=read.csv('Assignments/Assignment_7/Utah_Religions_by_County.csv')
view(ut_rel)
tidyUtRel=ut_rel %>%
  pivot_longer(cols = -c(County, Pop_2010, Religious),
               names_to = 'Religion',
               values_to = 'Proportion') %>%
  select(-Religious)
view(tidyUtRel)
custom_14_colors <- c(
  "#4E79A7","#F28E2B","#E15759","#76B7B2","#59A14F","#EDC948","#B07AA1",
  "#FF9DA7","#9C755F","#BAB0AC","#E1B939","#8CD17D","#86BCB6","#BCBDDC")
tidyUtRel %>%
  ggplot(aes(x=County,y=Proportion, fill=Religion)) +
  geom_bar(stat='identity')+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))+
  scale_fill_manual(values = custom_14_colors)+
  labs(title = "Religous Identification by County in Utah")
#Q1
##linear model
tidyUtRel %>%
  ggplot(aes(x=Pop_2010,y=Proportion,color=County))+
  geom_point(size=1.5)+
  geom_smooth(method = "lm",se=FALSE,aes(group = Religion))+
  facet_wrap(~Religion,scales='free_y')+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))+
  guides(color = guide_legend(override.aes = list(linetype = 0)))
ggsave('lm_UTrelxCounty.png',width=10,height = 15, dpi=300)

#loess regression
tidyUtRel %>%
  ggplot(aes(x=Pop_2010,y=Proportion,color=County))+
  geom_point(size=1.5)+ #increases size of point
  geom_smooth(method = "loess",se=FALSE,aes(group = Religion))+ #trendline follows religion, not county
  facet_wrap(~Religion,scales='free_y')+ #splits into individual plots for religion and scales to max proportion
  theme_minimal()+ #gets rid of gray background
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))+ #adjusts axis label
  guides(color = guide_legend(override.aes = list(linetype = 0))) #gets rid of line in legend
ggsave('loess_UTrelxCounty.png',width=10,height = 15, dpi=300)
## There is not much of a correlation. The trendlines are produced, but
## the points are not particularly close to the trendlines

#Q2
ut_rel2=ut_rel %>%
  pivot_longer(cols = -c(County, Pop_2010, Religious,Non.Religious),
               names_to = 'Religion',
               values_to = 'Proportion') %>%
  select(-Religious) %>%
  view()

ut_rel2 %>%
  ggplot(aes(x=Proportion, y=Non.Religious, color=Religion))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, aes(group = Religion)) +
  facet_wrap(~Religion,scales='free')+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))+
  guides(color = guide_legend(override.aes = list(linetype = 0)))
ggsave('lm_RelvsNon.png',width=10,height = 15, dpi=300)
ut_rel2 %>%
  ggplot(aes(x=Proportion, y=Non.Religious, color=Religion))+
  geom_point()+
  geom_smooth(method = 'loess', se = FALSE, aes(group = Religion)) +
  facet_wrap(~Religion,scales='free')+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))+
  guides(color = guide_legend(override.aes = list(linetype = 0)))
ggsave('loess_RelvsNon.png',width=10,height = 15, dpi=300)
## There appears to be a loose negative correlation with LDS and
## maybe a positive one with Episcopal
## No others appear to have a correlation