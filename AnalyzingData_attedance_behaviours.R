#### Senescence Analysis: Attendance and Behaviours ####
#### Made: 26.07.2023 ####
#### Last Edited: 23.11.2024 ####
#### Author Elliot Howard-Spink ####

#### Description ####
#This script looks at the rate of attendance at experimental nut cracking sites for chimpanzees at Bossou
#It also looks at what behaviours focal old-aged individuals engaged in at both sites.

#Clear workspace
rm(list=ls())

#### Import Packages ####
library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyr)
library(lme4)
library(ggtext) #For making plot labels bold

#### Directory ####
getwd()
setwd('Senesence_Master_JSub/') #Set as master directory

#### ATTENDANCE ####

#### Import Attendance Data ####
Attendance<-read.csv('Data/Attendance_Data.csv')
str(Attendance)

#Scale year
Attendance$Year_Scaled<- scale(Attendance$Year, center = TRUE, scale = TRUE)
#Interaction model
glm1<-glmer(Attendance ~  Year_Scaled*Old + (1|Individual), offset = log(Field_Days), family = 'poisson', data = Attendance)
summary(glm1)
#No interaction
glm_1_without_interaction<-glmer(Attendance ~ Year_Scaled + Old + (1|Individual), offset = log(Field_Days), family = 'poisson', data = Attendance) 
#Null model
glm_null<-glmer(Attendance ~ Old + (1|Individual),  offset = log(Field_Days), family = 'poisson', data = Attendance) 

#Confirm interaction is better using AIC
AIC(glm1,glm_1_without_interaction, glm_null) #Significantly better with interaction term.


#See model summary for interaction model
summary(glm1)

#We can see this by the mean difference in attendance rate in 1999 between young and old individuals being lower than in later years

#1999
Attendance[Attendance$Year == '1999',] %>%
  filter(Old == 'Y') %>%
  {mean(.$Attendance_Rate)}
Attendance[Attendance$Year == '1999',] %>%
  filter(Old == 'N') %>%
  {mean(.$Attendance_Rate)}
0.7428571 -  0.6825397
#20.3% difference between old and young

#2016
Attendance[Attendance$Year == '2016',] %>%
  filter(Old == 'Y') %>%
  {mean(.$Attendance_Rate)}
Attendance[Attendance$Year == '2016',] %>%
  filter(Old == 'N') %>%
  {mean(.$Attendance_Rate)}
0.525641 - 0.3230769
#20.3% difference between old and young

#Visualize Data
Attendance$Old<-factor(Attendance$Old, levels = c('Y','N'))
attendance_plot<-ggplot(Attendance, aes(x = Year, y = Attendance_Rate)) +
  geom_jitter(aes(color = Old), width = 0.2) + geom_line(aes(linetype = Individual, color = Old)) + theme_light() +
  scale_linetype_manual(name = 'Individual', values = c("solid", "dashed", "dotted", "blank", "dotdash", "longdash", "blank", "twodash", "blank", "1F", "3F", "blank", "6F", "12345678"), #set so that only individuals present in multiple years have a line
                        labels = c('**Fana**', 'Fanle', 'FOAF', 'Fotaiu', 'JEJE', '**Jire**', 'Kai', 'PELEY', 'Pili', '**TUA**', '**Velu**', 'Vuavua', '**Yo**', 'YOLO')) +
  xlab('Year') + ylab('Attendance Rate (Encounters per Day)') + labs(tag = "a.") +
  scale_color_manual(name = 'Above 30 Years',values = c('red','black'), labels=c("Y", "N")) +
  theme(legend.text = element_markdown())
attendance_plot


#Visualize just the focal old-age individuals.
just_old_df<-Attendance[Attendance$Old == 'Y',] #Just old individuals
just_old_df<-just_old_df[just_old_df$Individual != 'FOAF',] #Remove Foaf - not sampled longitudinally
just_old_df<-just_old_df[just_old_df$Individual != 'KIA',] #Remove Kia - not sampled longitudinally

old_attendance_plot<-ggplot(just_old_df, aes(x = Year, y = Attendance_Rate)) +
  geom_jitter(aes(color = Individual), width = 0.2) + geom_line(aes(color = Individual)) + theme_light() + labs(tag = "b.") +
  xlab('Year') + ylab('Attendance Rate (Encounters per Day)') + scale_color_manual(name = 'Individual', values = c('#332288','#AA4499','#DDCC77','#44AA99', '#88CCEE'), labels = c('Fana','Jire','TUA','Velu','Yo')) 
old_attendance_plot


#### Summarize Data ####

#All individuals
length(Attendance[Attendance$Field_Season == '1999-2000',]$Individual) #11 individuals
length(Attendance[Attendance$Field_Season == '2004-2005',]$Individual) #7 individuals
length(Attendance[Attendance$Field_Season == '2008-2009',]$Individual) #10 individuals
length(Attendance[Attendance$Field_Season == '2011-2012',]$Individual) #9 individuals
length(Attendance[Attendance$Field_Season == '2016-2017',]$Individual) #7 individuals

#Elderly
length(Attendance[Attendance$Field_Season == '1999-2000',][Attendance[Attendance$Field_Season == '1999-2000',]$Old == 'Y',]$Individual) #6
length(Attendance[Attendance$Field_Season == '2004-2005',][Attendance[Attendance$Field_Season == '2004-2005',]$Old == 'Y',]$Individual) #5
length(Attendance[Attendance$Field_Season == '2008-2009',][Attendance[Attendance$Field_Season == '2008-2009',]$Old == 'Y',]$Individual) #5
length(Attendance[Attendance$Field_Season == '2011-2012',][Attendance[Attendance$Field_Season == '2011-2012',]$Old == 'Y',]$Individual) #6
length(Attendance[Attendance$Field_Season == '2016-2017',][Attendance[Attendance$Field_Season == '2016-2017',]$Old == 'Y',]$Individual) #5



#### Behaviour ####

#Import data
behaviour<-read.csv('Data/longform_behaviour_proportions_all.csv')

#Filter data
#Get total times observed for each individual
behaviour$Time.minutes<-(behaviour$TIME)/60

#Remove 'total time' for plotting
behaviour <- behaviour[behaviour$BEHAVIOUR != 'TOTALTIME',]
#As 2016 is from a different location (outdoor lab location 2, the Salon), we remove it from the analysis
behaviour <- behaviour[behaviour$YEAR != 2016,]
behaviour


#Visualize

#Name labeller
labeller = labeller(ID = 
                      c("FANA" = "Fana",
                        "JIRE" = "Jire",
                        "TUA" = "TUA",
                        "VELU" = "Velu",
                        "YO" = "Yo"))

facet_behaviours<-ggplot(behaviour) +
  geom_col(aes(x = factor(YEAR), y = PROPORTION, fill = factor(BEHAVIOUR, levels = c(setdiff(BEHAVIOUR, "NUTCRACKING"), "NUTCRACKING"))), position = 'fill') + 
  facet_wrap(~ID, ncol = 5, labeller = labeller) +
  labs(tag = "c.") +
  xlab('Year') +
  ylab('Proportion of Total Time') +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),strip.background =element_rect(fill="black"),plot.title = element_text(hjust = 0.5), legend.position = 'right') + 
  scale_fill_manual(name = 'Behaviour', values = c('#AA4499','#88CCEE','#DDCC77','#44AA99'), labels = c('Eating Fruit','Drinking','Other','Nuts & Stones')) 
facet_behaviours

### What are the changes in key behaviours for Velu and Fana?
#Velu
behaviour[behaviour$ID == 'VELU',]
54.3-18.8 #Eating Fruit
25.8-0 #Drinking
4.96-51.5 #46.54% reduction in nut cracking
#Fana
behaviour[behaviour$ID == 'FANA',]
0.517 - 0.134 #Other
0.156-0.623 #46.7 reduction in nut cracking.

#Other individuals
behaviour[behaviour$ID == 'JIRE',]
0.54734512 - 0.60230177 #Nut Cracking
behaviour[behaviour$ID == 'TUA',]
0.56210740 - 0.49310507 #Nut Cracking
behaviour[behaviour$ID == 'YO',]
0.7454677166 - 0.6565464102 #Nut Cracking


#### Make Combined Plot ####
#Plots have been labelled a, b, c above

display<- 'aaaabbbb
aaaacccc'
combined_plot<-wrap_plots(attendance_plot,old_attendance_plot,facet_behaviours, design = display) 
combined_plot
#saved by export at 1000 width and 800 height.



