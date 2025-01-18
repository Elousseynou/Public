library(tidyverse)
library(ratdat)
?complete_old
summary(complete_old)
head(complete_old)
str(complete_old)
###la structure des donnees str
###c'est le temps d'uttiliser ggplot
library(ggplot2)

ggplot(complete_old, mapping=aes(x=weight,y=hindfoot_length))+ geom_point(alpha=0.1)

complete_old<-filter(complete_old, !is.na(weight) & !is.na(hindfoot_length))
ggplot(complete_old, mapping=aes(x=weight,y=hindfoot_length, color=plot_type))+ geom_point(alpha=0.1,)
### is.na des donnees manquante et ! c'est l'inverse donne les donnees non manquante

complete_old<-filter(complete_old, !is.na(weight) & !is.na(hindfoot_length))
ggplot(complete_old,mapping=(x=weight,y=hindfoot_length; shape =sexe)) +
  geom_point(alpha=0.1)
scale_color_viridis_d()
scale_x_log10()
ggplot (complete_old, mapping=aes(x=plot_type, y= hindfoot_length))+
geom_boxplot()+ 
  scale_x_discrete(labels= label_wrap_gen(width=10))+ 
  geom_point()

ggplot (complete_old, mapping=aes(x=plot_type, y= hindfoot_length))+
  geom_boxplot(outlier.shape = NA, fill=NA)+ 
  scale_x_discrete(labels= label_wrap_gen(width=10))+ 
  geom_jitter(alpha=0.1, aes(color=plot_type))+
  geom_boxplot(outlier.shape = NA)

ggplot (complete_old, mapping=aes(x=plot_type, y= hindfoot_length))+
  scale_x_discrete(labels= label_wrap_gen(width=10))+ 
  geom_jitter(alpha=0.1, aes(color=plot_type))+
  geom_violin(fill=NA)

ggplot (complete_old, mapping=aes(x=plot_type, y= hindfoot_length))+
  geom_boxplot(outlier.shape = NA, fill=NA)+ 
  scale_x_discrete(labels= label_wrap_gen(width=10))+ 
  geom_jitter(alpha=0.1, aes(color=plot_type))+
  geom_boxplot(outlier.shape = NA)+
  facet_wrap(vars(sex), ncol=1)
  theme_bw() +
  theme(legend.position = ",=none") +
  labs(x="plot_types", y="hindfoot_length(mm)")

  ggplot (complete_old, mapping=aes(x=plot_type, y= hindfoot_length))+
    geom_boxplot(outlier.shape = NA, fill=NA)+ 
    scale_x_discrete(labels= label_wrap_gen(width=10))+ 
    geom_jitter(alpha=0.1, aes(color=plot_type))+
    geom_boxplot(outlier.shape = NA)+
    facet_wrap(vars(sex), nrow=1)
  theme_bw() +
    theme(legend.position = ",=none") +
    labs(x="plot_types", y="hindfoot_length(mm)")
  
Plot_final<-ggplot (complete_old, mapping=aes(x=plot_type, y= hindfoot_length))+
geom_boxplot(outlier.shape = NA, fill=NA)+ 
scale_x_discrete(labels= label_wrap_gen(width=10))+ 
geom_jitter(alpha=0.1, aes(color=plot_type))+
geom_boxplot(outlier.shape = NA)+
facet_wrap(vars(sex), ncol=1)+
theme_bw() +
    theme(legend.position = ",=none") +
    labs(x="plot_types", y="hindfoot_length(mm)")
Plot_final

ggsave(filename = "figure/plot_final.png", plot=Plot_final, height = 6, width = 8)

ggsave(filename = "Figures/plot_final.png", plot=Plot_final, height = 6, width = 8)



surveys<-read.csv("donnees/Raws/surveys_complete_77_89.csv")
view(surveys)
str(surveys)
select(surveys, plot_id, species_id)
select(surveys, c(3,4))
select(surveys, plot_id, species_id)
select(surveys, c(3,4))
select (surveys, where(is.numeric(anyNA)))

filter(surveys, species)
filter(surveys, year===1988)        
filter(surveys, year==1988 & species_id%in%c("RM","DM"))
filter(surveys, year==1980:1985
### Select and filter
surveys80_85<-filter(surveys, year>=1980 & year<=1985)
select(surveys80_85, year, month, species_id, plot_id)


select(filter(surveys, year>=1980 & year<=1985), year, month,species_id, plot_id))
###ctr siftM pour avoir %>% %>% %>% %>% 
surveys %>% 
  filter(year==1980:1985) %>% 
  select(year,month,species_id,plot_id)

surveys %>% 
  filter(year==1988) %>% 
  select(record_id, month, species_id)
1+1

###Mutate %>%
surveys %>% 
  filter(!is.na(weight))
  mutate(weight_Kg=weight/1000, weight_lbs=weight_Kgx2.2")%>% 
  relocate(weight_Kg, .after=record_id) %>% 
  relocate(weight_lbs, .after=weight_kg))
  
1+1))
surveys %>% 
mutate(date=ymd(paste(year,monthn day, sep="-")) %>% 
relocate(date, .after=year)


surveys %>% 
group_by(sex,year) %>% 
summarize(mean.weight=mean(weight, na.rm=TRUE),
count=n())

surveys %>%
mutate(date=ymd(paste(year, month,day, sep="-") %>%
filter(!is.na(sex)) %>% 
  group_by(sex,date) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=date, y=count,color=sex))+
  geom_line()






