#### Senescence Analysis: Metrics Per Nut ####
#### Made: 31.07.2023 ####
#### Last Edited: 23.11.2024 ####
#### Author Elliot Howard-Spink ####

###Clear workspace
rm(list=ls())

###Packages
library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)
library(patchwork)
library(ggeffects)

#Set directory
getwd()
setwd('Senesence_Master_JSub/')

#### Import Data ####
nut_data<-read.csv('Data/metrics_per_nut_with_nuttype.csv')
head(nut_data)
str(nut_data)


#### Filter data ####
#Remove pandas nut - this was only one occasion with Yo, during an isolated case where panda nuts were provided. We have omitted this instance of nut provisioning from this study.
nut_data<-nut_data[nut_data$Nut_Type != 'pandas',] #Remove pandas nut.
#One of Yo's data points is zero for time - this will be incorrect. Remove.
nut_data<-nut_data[nut_data$Total_Time != 0,] 
nut_data


#### Visualizing Data ####
#Visualize number of nuts cracked of each type for each individual.
ggplot() +
  geom_bar(data = nut_data, aes(x = Year, fill = Nut_Type)) +
  facet_wrap(~ID, ncol = 5) + theme_light()

#1601 nuts cracked (sequences documented in their entirety).
table(nut_data$Nut_Type)
length(nut_data$Nut_Type)
#63 coula nuts, and 1538 oil-palm.


#For one year (2011) three individuals (Jire, Tua, Yo) crack coula nuts.
#This would be difficult to try and model around as this nut type is highly correlated with year.
#We therefore will look for effects of increasing year (and therefore aging) in oil-palm nut cracking efficiency only.
#We will look at coula nuts as a specific case in which individuals generalized to a different nut-type in 2011 (see further down).





#### Oil-Palm Nut Data ####
#Remove coula nuts.
OP_nut_data<-nut_data[nut_data$Nut_Type == 'oil-palm',]
#How many encounters was this data collected over?
length(table(OP_nut_data$PAB_Year))
#Over 41 encounters.
OP_nut_data$Year_Factor<-as.factor(OP_nut_data$Year)


#### Visualizing Oil-Palm Nut Data ####

## Labels for names.
labeller = labeller(ID = 
                      c("FANA" = "Fana",
                        "JIRE" = "Jire",
                        "TUA" = "TUA",
                        "VELU" = "Velu",
                        "YO" = "Yo"))

## Function for plotting.
boxplot_years<-function(data_f, X, Y,  jitter_width, jitter_height, alpha_point, alpha_box, color_box, xlabel,ylabel){
  ploT<-ggplot(data = data_f, aes(x = X, y =Y)) +
    #geom_jitter(width = jitter_width, height = jitter_height, alpha = alpha_point) + 
    theme_light()  + facet_wrap(~ID, ncol = 5, labeller = labeller) +
    geom_boxplot(data = data_f, aes(x = X, y = Y), alpha = alpha_box,color = color_box) +
    xlab(xlabel) +
    ylab(ylabel) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), strip.background = element_rect(fill = "black"))
  return(ploT)
}

#Globals.
alpha_points = 0.7
alpha_points = 0.3
alpha_boxes = 0.6
color_boxes = '#332288'
jitter_widths = 0.2
jitter_heights = 0.1

#time
time_years<-boxplot_years(data_f = OP_nut_data, X = OP_nut_data$Year_Factor, Y = OP_nut_data$Total_Time, jitter_width = jitter_widths, 
                          jitter_height = jitter_heights, alpha_point = alpha_points, alpha_box = alpha_boxes, color_box = color_boxes, xlabel = 'Year', ylabel = 'Time (s)')
time_years
#Difference detected for Yo and Velu, and also perhaps Jire.

#actions
actionEvents_years<-boxplot_years(data_f = OP_nut_data, X = OP_nut_data$Year_Factor, Y = OP_nut_data$Action_Events,  jitter_width = jitter_widths, 
                                  jitter_height = jitter_heights, alpha_point = alpha_points, alpha_box = alpha_boxes, color_box = color_boxes, xlabel = '', ylabel = 'Actions')
actionEvents_years
#Difference detected for Yo and Velu - perhaps Jire.

#action_types
actionTypes_years<-boxplot_years(data_f = OP_nut_data, X = OP_nut_data$Year_Factor, Y = OP_nut_data$Action_Types,  jitter_width = jitter_widths, 
                                 jitter_height = jitter_heights, alpha_point = alpha_points, alpha_box = alpha_boxes, color_box = color_boxes, xlabel = '', ylabel = 'Action Types')
actionTypes_years
#Difference detected for Velu, Maybe Jire, Maybe Yo.

#strikes
strikes_years<-boxplot_years(data_f = OP_nut_data, X = OP_nut_data$Year_Factor, Y = OP_nut_data$Strikes, jitter_width = jitter_widths, 
                             jitter_height = jitter_heights, alpha_point = alpha_points, alpha_box = alpha_boxes, color_box = color_boxes, xlabel = 'Year', ylabel = 'Strikes')
strikes_years
#Difference detected for Yo and Velu.

#Nut (Re)ositions.
reposition_years<-boxplot_years(data_f = OP_nut_data, X = OP_nut_data$Year_Factor, Y = OP_nut_data$Nut_Positioning, jitter_width = jitter_widths, 
              jitter_height = jitter_heights, alpha_point = alpha_points, alpha_box = alpha_boxes, color_box = color_boxes, xlabel = 'Year', ylabel = 'Nut (Re)positions')
reposition_years
#No evidence of a difference.

#Reorient Stones.
reorient_years<-boxplot_years(data_f = OP_nut_data, X = OP_nut_data$Year_Factor, Y = OP_nut_data$Reorient_Stones, jitter_width = jitter_widths, 
              jitter_height = jitter_heights, alpha_point = alpha_points, alpha_box = alpha_boxes, color_box = color_boxes, xlabel = 'Year', ylabel = 'Stone-Tool Reorientations')
reorient_years
#Maybe a difference for Velu.

#Tool Changes.
toolchanges_years<-boxplot_years(data_f = OP_nut_data, X = OP_nut_data$Year_Factor, Y = OP_nut_data$Tool_Changes, jitter_width = jitter_widths, 
              jitter_height = jitter_heights, alpha_point = alpha_points, alpha_box = alpha_boxes, color_box = color_boxes, xlabel = 'Year', ylabel = 'Tool Changes')
toolchanges_years
#No evidence of a difference.

#Make a combined plot for all metrics (for supplementary).
time_years
p1<-actionEvents_years + theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
p2<-actionTypes_years + theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
p3<-strikes_years + theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
p4<-reposition_years + theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
p5<-reorient_years 
p6<-toolchanges_years 

combined_all_OP<-time_years / (p1 + p2) / (p3 + p4) / (p5 + p6)
combined_all_OP







#### Modelling OP Data for key Metrics ####

#We are modelling four metrics:
#Time - lmer.
#Number of actions - glmer, poisson.
#Number of action types - glmer, poisson.
#Number of strikes - glmer, poisson.

#Scale year so that it isn't modelled from year zero.
OP_nut_data$Year_Scaled <- scale(OP_nut_data$Year, center = TRUE, scale = TRUE)
#Log Time
OP_nut_data$Log_Time<-log(OP_nut_data$Total_Time)

#Fit model with random slope and compare with no effect of year.
Time_Slope_Model<-lmer(Log_Time ~ Year_Scaled + (Year_Scaled|ID) + (1|PAB) , data=OP_nut_data, REML = F)
Time_Null_Model<-lmer(Log_Time ~ (1|ID) + (1|PAB) , data=OP_nut_data, REML = F)
AIC(Time_Null_Model,Time_Slope_Model) #Having an effect of year significantly increases the explanatory power of the model.

#look at model.
summary(Time_Slope_Model)
coef(Time_Slope_Model)

#Check assumptions.
qqnorm(resid(Time_Slope_Model))
qqline(resid(Time_Slope_Model))
plot(resid(Time_Slope_Model))

#Plot model.
pred_Time<-ggpredict(Time_Slope_Model, terms = c("Year_Scaled"))
GI = 2.65690
GS = 0.07700 

#Model.
timemodel_plot<-ggplot() +
  geom_point(data = OP_nut_data,                      # adding the raw data (scaled values)
             aes(x = Year_Scaled, y = Log_Time, fill = ID), shape = 21, size = 2,show.legend = FALSE, alpha = 0.7, color = 'grey4') + 
  geom_abline(aes(intercept = 2.613867, slope = 0.06438108, color = 'Fana')) + #Fana
  geom_abline(aes(intercept = 2.508346, slope = 0.08898057, color = 'Jire')) + #Jire
  geom_abline(aes(intercept = 2.327689, slope = 0.03626724, color = 'Tua')) + #Tua
  geom_abline(aes(intercept = 2.634699, slope = 0.06774197, color = 'Velu')) + #Velu
  geom_abline(aes(intercept = 3.199903, slope = 0.12765093, color = 'Yo')) + #Yo
  geom_abline(aes(intercept = GI, slope = GS, color = 'Baseline')) + #Global
  labs(tag = '',x = "Scaled Year", y = "Log Time (s)") + 
  #ylim(0,4) +
  scale_fill_manual(values = c('FANA' = "#332288",'JIRE' = '#AA4499','TUA' = '#DDCC77','VELU' = '#44AA99','YO' = '#88CCEE')) +
  scale_colour_manual(name = 'Model',values=c('Fana' = "#332288",'Jire' = '#AA4499','Tua' = '#DDCC77','Velu' = '#44AA99','Yo' = '#88CCEE','Baseline' = 'black'),
                      breaks = c('Fana','Jire','Tua','Velu','Yo','Baseline')) +
  #ggtitle("Model Prediction: Duration of Tool Selection Vs. Year (Scaled)") +
  theme_light() 
timemodel_plot

## Look at the %change in mean time between first and last year for each individual to evaluate.

percent_change_time<-function(name,earliest_year,latest_year){
  
  mean_early<-mean(OP_nut_data[OP_nut_data$ID == name & OP_nut_data$Year == earliest_year,]$Total_Time)
  print(paste('mean in', earliest_year, sep = ' '))
  print(mean_early)
  
  mean_late<-mean(OP_nut_data[OP_nut_data$ID == name & OP_nut_data$Year == latest_year,]$Total_Time)
  print(paste('mean in', latest_year, sep = ' '))
  print(mean_late)
  
  perc_change<-(((mean_late-mean_early)/mean_early)*100)
  print('Percentage Change:')
  print(perc_change)
  
  print('Actual time change (s):')
  print(mean_late-mean_early)
}

#Yo
percent_change_time('YO','1999','2016')

#Velu
percent_change_time('VELU','1999','2016')

#Jire
percent_change_time('JIRE','1999','2016')

#Fana
percent_change_time('FANA','1999','2016')

#Tua
percent_change_time('TUA','1999','2008')


##Number of Actions - glmer
actions_slope_glmer<-glmer(Action_Events ~ Year_Scaled + (Year_Scaled|ID) + (1|PAB), data=OP_nut_data, family = poisson)
action_null_glmer<-glmer(Action_Events ~ (1|ID) + (1|PAB), data=OP_nut_data, family = poisson)
AIC(action_null_glmer,actions_slope_glmer) #Slope significantly better than null, and fixed slope model.
summary(actions_slope_glmer)
coef(actions_slope_glmer)

#Function
percent_change_actions<-function(name,earliest_year,latest_year){
  
  med_early<-median(OP_nut_data[OP_nut_data$ID == name & OP_nut_data$Year == earliest_year,]$Action_Events)
  print(paste('Median in', earliest_year, sep = ' '))
  print(med_early)
  
  med_late<-median(OP_nut_data[OP_nut_data$ID == name & OP_nut_data$Year == latest_year,]$Action_Events)
  print(paste('Median in', latest_year, sep = ' '))
  print(med_late)
  
  perc_change<-(((med_late-med_early)/med_early)*100)
  print('Percentage Change:')
  print(perc_change)
  
  print('Actual change:')
  print(med_late-med_early)
}

#Yo
percent_change_actions('YO','1999','2016') 

#Velu
percent_change_actions('VELU','1999','2016') 

#Jire
percent_change_actions('JIRE','1999','2016') 

#Fana
percent_change_actions('FANA','1999','2016')

#Tua
percent_change_actions('TUA','1999','2008') 




#### Strikes ####
strikes_slope_glmer<-glmer(Strikes ~ Year_Scaled + (Year_Scaled|ID) + (1|PAB), data=OP_nut_data, family = poisson)
strikes_null_glmer<-glmer(Strikes ~ (1|ID) + (1|PAB), data=OP_nut_data, family = poisson)
AIC(strikes_null_glmer,strikes_slope_glmer) #Significantly better than null
summary(strikes_slope_glmer)
coef(strikes_slope_glmer)

## Function
percent_change_strikes<-function(name,earliest_year,latest_year){
  
  med_early<-median(OP_nut_data[OP_nut_data$ID == name & OP_nut_data$Year == earliest_year,]$Strikes)
  print(paste('Median in', earliest_year, sep = ' '))
  print(med_early)
  
  med_late<-median(OP_nut_data[OP_nut_data$ID == name & OP_nut_data$Year == latest_year,]$Strikes)
  print(paste('Median in', latest_year, sep = ' '))
  print(med_late)
  
  perc_change<-(((med_late-med_early)/med_early)*100)
  print('Percentage Change:')
  print(perc_change)
  
  print('Actual change:')
  print(med_late-med_early)
}

#Yo
percent_change_strikes('YO','1999','2016') 

#Velu
percent_change_strikes('VELU','1999','2016') 

#Jire
percent_change_strikes('JIRE','1999','2016') 

#Fana
percent_change_strikes('FANA','1999','2016') 

#Tua
percent_change_strikes('TUA','1999','2008') 



#Also have to do stone-tool reorientations.
Reorient_Stones_slope_glmer<-glmer(Reorient_Stones ~ Year_Scaled + (Year_Scaled|ID) + (1|PAB), data=OP_nut_data, family = poisson)
Reorient_Stones_null_glmer<-glmer(Reorient_Stones ~ (1|ID) + (1|PAB), data=OP_nut_data, family = poisson)
AIC(Reorient_Stones_null_glmer,Reorient_Stones_slope_glmer) #No difference



#### Making Figure 4
time_years
p1<-boxplot_years(data_f = OP_nut_data, X = OP_nut_data$Year_Factor, Y = OP_nut_data$Action_Events,  jitter_width = jitter_widths, 
                  jitter_height = jitter_heights, alpha_point = alpha_points, alpha_box = alpha_boxes, color_box = color_boxes, xlabel = 'Year', ylabel = 'Actions')
p3<-strikes_years

time_years + p1 + p3 + plot_annotation(tag_levels = c('a'), tag_suffix = '.')
#Exported at 1100 x 600




#### Coula Nut and Oil-Palm Nut Cracking Compared ####
### This section is quite long and repetitive - a summary of all outputs is in the supplementary materials.
get_medians<-function(data, individual){
  years<-c()
  time_mean<-c()
  time_sd<-c()
  actions_med<-c()
  actions_iqr<-c()
  actiontype_med<-c()
  actiontype_iqr<-c()
  strikes_med<-c()
  strikes_iqr<-c()
  nutposi_med<-c()
  nutposi_iqr<-c()
  reorient_med<-c()
  reorient_iqr<-c()
  changes_med<-c()
  changes_iqr<-c()
  data_f<-data[data$ID == individual,]
  
  years<-append(years,'2008')
  years<-append(years,'2011')
  years<-append(years,'2016')
  
  time_mean<-append(time_mean,mean(data_f[data_f$Year_Factor == '2008',]$Total_Time))
  time_mean<-append(time_mean,mean(data_f[data_f$Year_Factor == '2011',]$Total_Time))
  time_mean<-append(time_mean,mean(data_f[data_f$Year_Factor == '2016',]$Total_Time))
  time_sd<-append(time_sd,sd(data_f[data_f$Year_Factor == '2008',]$Total_Time))
  time_sd<-append(time_sd,sd(data_f[data_f$Year_Factor == '2011',]$Total_Time))
  time_sd<-append(time_sd,sd(data_f[data_f$Year_Factor == '2016',]$Total_Time))
  
  actions_med<-append(actions_med,median(data_f[data_f$Year_Factor == '2008',]$Action_Events))
  actions_med<-append(actions_med,median(data_f[data_f$Year_Factor == '2011',]$Action_Events))
  actions_med<-append(actions_med,median(data_f[data_f$Year_Factor == '2016',]$Action_Events))
  actions_iqr<-append(actions_iqr,IQR(data_f[data_f$Year_Factor == '2008',]$Action_Events))
  actions_iqr<-append(actions_iqr,IQR(data_f[data_f$Year_Factor == '2011',]$Action_Events))
  actions_iqr<-append(actions_iqr,IQR(data_f[data_f$Year_Factor == '2016',]$Action_Events))
  
  actiontype_med<-append(actiontype_med,median(data_f[data_f$Year_Factor == '2008',]$Action_Type))
  actiontype_med<-append(actiontype_med,median(data_f[data_f$Year_Factor == '2011',]$Action_Type))
  actiontype_med<-append(actiontype_med,median(data_f[data_f$Year_Factor == '2016',]$Action_Type))
  actiontype_iqr<-append(actiontype_iqr,IQR(data_f[data_f$Year_Factor == '2008',]$Action_Type))
  actiontype_iqr<-append(actiontype_iqr,IQR(data_f[data_f$Year_Factor == '2011',]$Action_Type))
  actiontype_iqr<-append(actiontype_iqr,IQR(data_f[data_f$Year_Factor == '2016',]$Action_Type))
  
  strikes_med<-append(strikes_med,median(data_f[data_f$Year_Factor == '2008',]$Strikes))
  strikes_med<-append(strikes_med,median(data_f[data_f$Year_Factor == '2011',]$Strikes))
  strikes_med<-append(strikes_med,median(data_f[data_f$Year_Factor == '2016',]$Strikes))
  strikes_iqr<-append(strikes_iqr,IQR(data_f[data_f$Year_Factor == '2008',]$Strikes))
  strikes_iqr<-append(strikes_iqr,IQR(data_f[data_f$Year_Factor == '2011',]$Strikes))
  strikes_iqr<-append(strikes_iqr,IQR(data_f[data_f$Year_Factor == '2016',]$Strikes))
  
  nutposi_med<-append(nutposi_med,median(data_f[data_f$Year_Factor == '2008',]$Nut_Positioning))
  nutposi_med<-append(nutposi_med,median(data_f[data_f$Year_Factor == '2011',]$Nut_Positioning))
  nutposi_med<-append(nutposi_med,median(data_f[data_f$Year_Factor == '2016',]$Nut_Positioning))
  nutposi_iqr<-append(nutposi_iqr,IQR(data_f[data_f$Year_Factor == '2008',]$Nut_Positioning))
  nutposi_iqr<-append(nutposi_iqr,IQR(data_f[data_f$Year_Factor == '2011',]$Nut_Positioning))
  nutposi_iqr<-append(nutposi_iqr,IQR(data_f[data_f$Year_Factor == '2016',]$Nut_Positioning))
  
  reorient_med<-append(reorient_med,median(data_f[data_f$Year_Factor == '2008',]$Reorient_Stones))
  reorient_med<-append(reorient_med,median(data_f[data_f$Year_Factor == '2011',]$Reorient_Stones))
  reorient_med<-append(reorient_med,median(data_f[data_f$Year_Factor == '2016',]$Reorient_Stones))
  reorient_iqr<-append(reorient_iqr,IQR(data_f[data_f$Year_Factor == '2008',]$Reorient_Stones))
  reorient_iqr<-append(reorient_iqr,IQR(data_f[data_f$Year_Factor == '2011',]$Reorient_Stones))
  reorient_iqr<-append(reorient_iqr,IQR(data_f[data_f$Year_Factor == '2016',]$Reorient_Stones))
  
  changes_med<-append(changes_med,median(data_f[data_f$Year_Factor == '2008',]$Tool_Changes))
  changes_med<-append(changes_med,median(data_f[data_f$Year_Factor == '2011',]$Tool_Changes))
  changes_med<-append(changes_med,median(data_f[data_f$Year_Factor == '2016',]$Tool_Changes))
  changes_iqr<-append(changes_iqr,IQR(data_f[data_f$Year_Factor == '2008',]$Tool_Changes))
  changes_iqr<-append(changes_iqr,IQR(data_f[data_f$Year_Factor == '2011',]$Tool_Changes))
  changes_iqr<-append(changes_iqr,IQR(data_f[data_f$Year_Factor == '2016',]$Tool_Changes))
  
  
  r_data<-as.data.frame(cbind(years,time_mean,time_sd,actions_med,actions_iqr,actiontype_med,actiontype_iqr,
                      strikes_med,strikes_iqr,nutposi_med,nutposi_iqr,reorient_med,reorient_iqr,changes_med,changes_iqr))
  
  return(r_data)
}


#Make year a factor
nut_data$Year_Factor<-as.factor(nut_data$Year)

### Yo ###
nut_data[nut_data$ID == 'YO',]
yo_nuts<-nut_data[nut_data$ID == 'YO',]
table(yo_nuts$Year)

#Number of nuts cracked in each year
yo_data_op<-OP_nut_data[OP_nut_data$ID == 'YO',]
length(yo_data_op[yo_data_op$Year_Factor == '2008',]$Strikes) #in 2008 - 141
length((yo_nuts[yo_nuts$Nut_Type == 'coula',]$Strikes)) #in 2011 - 20
length(yo_data_op[yo_data_op$Year_Factor == '2016',]$Strikes) #in 2016 - 33

#Time Duration
mean(yo_data_op[yo_data_op$Year_Factor == '2008',]$Total_Time) #2008
sd(yo_data_op[yo_data_op$Year_Factor == '2008',]$Total_Time)
mean((yo_nuts[yo_nuts$Year_Factor == '2011',]$Total_Time)) #2011
sd((yo_nuts[yo_nuts$Year_Factor == '2011',]$Total_Time))
mean(yo_data_op[yo_data_op$Year_Factor == '2016',]$Total_Time) #2016
sd(yo_data_op[yo_data_op$Year_Factor == '2016',]$Total_Time)

#Median Actions
median(yo_data_op[yo_data_op$Year_Factor == '2008',]$Action_Events)
IQR(yo_data_op[yo_data_op$Year_Factor == '2008',]$Action_Events)
median((yo_nuts[yo_nuts$Year_Factor == '2011',]$Action_Events))
IQR((yo_nuts[yo_nuts$Year_Factor == '2011',]$Action_Events))
median(yo_data_op[yo_data_op$Year_Factor == '2016',]$Action_Events)
IQR(yo_data_op[yo_data_op$Year_Factor == '2016',]$Action_Events)

#Median Action Types
median(yo_data_op[yo_data_op$Year_Factor == '2008',]$Action_Types)
IQR(yo_data_op[yo_data_op$Year_Factor == '2008',]$Action_Types)
median((yo_nuts[yo_nuts$Year_Factor == '2011',]$Action_Types))
IQR((yo_nuts[yo_nuts$Year_Factor == '2011',]$Action_Types))
median(yo_data_op[yo_data_op$Year_Factor == '2016',]$Action_Types)
IQR(yo_data_op[yo_data_op$Year_Factor == '2016',]$Action_Types)

#Median strikes
median(yo_data_op[yo_data_op$Year_Factor == '2008',]$Strikes) #2008 - OP
IQR(yo_data_op[yo_data_op$Year_Factor == '2008',]$Strikes)
median((yo_nuts[yo_nuts$Year_Factor == '2011',]$Strikes)) #2011 - coula
IQR((yo_nuts[yo_nuts$Year_Factor == '2011',]$Strikes))
median(yo_data_op[yo_data_op$Year_Factor == '2016',]$Strikes) #2016 - OP
IQR(yo_data_op[yo_data_op$Year_Factor == '2016',]$Strikes)

#Nut repositions
median(yo_data_op[yo_data_op$Year_Factor == '2008',]$Nut_Positioning)
IQR(yo_data_op[yo_data_op$Year_Factor == '2008',]$Nut_Positioning)
median((yo_nuts[yo_nuts$Year_Factor == '2011',]$Nut_Positioning))
IQR((yo_nuts[yo_nuts$Year_Factor == '2011',]$Nut_Positioning))
median(yo_data_op[yo_data_op$Year_Factor == '2016',]$Nut_Positioning)
IQR(yo_data_op[yo_data_op$Year_Factor == '2016',]$Nut_Positioning)

#Tool Reorientations
median(yo_data_op[yo_data_op$Year_Factor == '2008',]$Reorient_Stones)
IQR(yo_data_op[yo_data_op$Year_Factor == '2008',]$Reorient_Stones)
median((yo_nuts[yo_nuts$Year_Factor == '2011',]$Reorient_Stones))
IQR((yo_nuts[yo_nuts$Year_Factor == '2011',]$Reorient_Stones))
median(yo_data_op[yo_data_op$Year_Factor == '2016',]$Reorient_Stones)
IQR(yo_data_op[yo_data_op$Year_Factor == '2016',]$Reorient_Stones)

#Tool Changes
median(yo_data_op[yo_data_op$Year_Factor == '2008',]$Tool_Changes)
IQR(yo_data_op[yo_data_op$Year_Factor == '2008',]$Tool_Changes)
median((yo_nuts[yo_nuts$Year_Factor == '2011',]$Tool_Changes))
IQR((yo_nuts[yo_nuts$Year_Factor == '2011',]$Tool_Changes))
median(yo_data_op[yo_data_op$Year_Factor == '2016',]$Tool_Changes)
IQR(yo_data_op[yo_data_op$Year_Factor == '2016',]$Tool_Changes)



### Tua ###
## Tua disappeared before 2016, so only has data for 2008 and 2011.
tua_data<-nut_data[nut_data$ID == 'TUA',]
tua_data

#Total time
mean(tua_data[tua_data$Year_Factor == '2008',]$Total_Time)
sd(tua_data[tua_data$Year_Factor == '2008',]$Total_Time)
mean(tua_data[tua_data$Year_Factor == '2011',]$Total_Time)
sd(tua_data[tua_data$Year_Factor == '2011',]$Total_Time)

#Actions
median(tua_data[tua_data$Year_Factor == '2008',]$Action_Events)
IQR(tua_data[tua_data$Year_Factor == '2008',]$Action_Events)
median(tua_data[tua_data$Year_Factor == '2011',]$Action_Events)
IQR(tua_data[tua_data$Year_Factor == '2011',]$Action_Events)

#Action Types
median(tua_data[tua_data$Year_Factor == '2008',]$Action_Types)
IQR(tua_data[tua_data$Year_Factor == '2008',]$Action_Types)
median(tua_data[tua_data$Year_Factor == '2011',]$Action_Types)
IQR(tua_data[tua_data$Year_Factor == '2011',]$Action_Types)

#Strikes
median(tua_data[tua_data$Year_Factor == '2008',]$Strikes)
IQR(tua_data[tua_data$Year_Factor == '2008',]$Strikes)
median(tua_data[tua_data$Year_Factor == '2011',]$Strikes)
IQR(tua_data[tua_data$Year_Factor == '2011',]$Strikes)

#Nut positions
median(tua_data[tua_data$Year_Factor == '2008',]$Nut_Positioning)
IQR(tua_data[tua_data$Year_Factor == '2008',]$Nut_Positioning)
median(tua_data[tua_data$Year_Factor == '2011',]$Nut_Positioning)
IQR(tua_data[tua_data$Year_Factor == '2011',]$Nut_Positioning)

#Stone reorientations
median(tua_data[tua_data$Year_Factor == '2008',]$Reorient_Stones)
IQR(tua_data[tua_data$Year_Factor == '2008',]$Reorient_Stones)
median(tua_data[tua_data$Year_Factor == '2011',]$Reorient_Stones)
IQR(tua_data[tua_data$Year_Factor == '2011',]$Reorient_Stones)

#Tool changes
median(tua_data[tua_data$Year_Factor == '2008',]$Tool_Changes)
IQR(tua_data[tua_data$Year_Factor == '2008',]$Tool_Changes)
median(tua_data[tua_data$Year_Factor == '2011',]$Tool_Changes)
IQR(tua_data[tua_data$Year_Factor == '2011',]$Tool_Changes)




#Jire

#For Jire, we need to split the 2011 data based on which species of nut she was cracking
#The other estimates for Jire (from the 2008 and 2016 data) are calculated in the next section.
jire_data<-nut_data[nut_data$ID == 'JIRE',]
jire_data$Year_Factor<-as.factor<-jire_data$Year
jire_data_2011<-jire_data[jire_data$Year_Factor == '2011',]

length(jire_data_2011[jire_data_2011$Nut_Type == 'coula',]$Total_Time)
length(jire_data_2011[jire_data_2011$Nut_Type == 'oil-palm',]$Total_Time)

mean(jire_data_2011[jire_data_2011$Nut_Type == 'coula',]$Total_Time)
sd(jire_data_2011[jire_data_2011$Nut_Type == 'coula',]$Total_Time)
mean(jire_data_2011[jire_data_2011$Nut_Type == 'oil-palm',]$Total_Time)
sd(jire_data_2011[jire_data_2011$Nut_Type == 'oil-palm',]$Total_Time)

median(jire_data_2011[jire_data_2011$Nut_Type == 'coula',]$Action_Events)
IQR(jire_data_2011[jire_data_2011$Nut_Type == 'coula',]$Action_Events)
median(jire_data_2011[jire_data_2011$Nut_Type == 'oil-palm',]$Action_Events)
IQR(jire_data_2011[jire_data_2011$Nut_Type == 'oil-palm',]$Action_Events)

median(jire_data_2011[jire_data_2011$Nut_Type == 'coula',]$Action_Types)
IQR(jire_data_2011[jire_data_2011$Nut_Type == 'coula',]$Action_Types)
median(jire_data_2011[jire_data_2011$Nut_Type == 'oil-palm',]$Action_Types)
IQR(jire_data_2011[jire_data_2011$Nut_Type == 'oil-palm',]$Action_Types)

median(jire_data_2011[jire_data_2011$Nut_Type == 'coula',]$Strikes)
IQR(jire_data_2011[jire_data_2011$Nut_Type == 'coula',]$Strikes)
median(jire_data_2011[jire_data_2011$Nut_Type == 'oil-palm',]$Strikes)
IQR(jire_data_2011[jire_data_2011$Nut_Type == 'oil-palm',]$Strikes)

median(jire_data_2011[jire_data_2011$Nut_Type == 'coula',]$Nut_Positioning)
IQR(jire_data_2011[jire_data_2011$Nut_Type == 'coula',]$Nut_Positioning)
median(jire_data_2011[jire_data_2011$Nut_Type == 'oil-palm',]$Nut_Positioning)
IQR(jire_data_2011[jire_data_2011$Nut_Type == 'oil-palm',]$Nut_Positioning)

median(jire_data_2011[jire_data_2011$Nut_Type == 'coula',]$Reorient_Stones)
IQR(jire_data_2011[jire_data_2011$Nut_Type == 'coula',]$Reorient_Stones)
median(jire_data_2011[jire_data_2011$Nut_Type == 'oil-palm',]$Reorient_Stones)
IQR(jire_data_2011[jire_data_2011$Nut_Type == 'oil-palm',]$Reorient_Stones)

median(jire_data_2011[jire_data_2011$Nut_Type == 'coula',]$Tool_Changes)
IQR(jire_data_2011[jire_data_2011$Nut_Type == 'coula',]$Tool_Changes)
median(jire_data_2011[jire_data_2011$Nut_Type == 'oil-palm',]$Tool_Changes)
IQR(jire_data_2011[jire_data_2011$Nut_Type == 'oil-palm',]$Tool_Changes)



#Other data for jire
length(jire_data[jire_data$Year_Factor == '2008',]$Tool_Changes)
length(jire_data[jire_data$Year_Factor == '2016',]$Tool_Changes)

mean(jire_data[jire_data$Year_Factor == '2008',]$Total_Time)
sd(jire_data[jire_data$Year_Factor == '2008',]$Total_Time)
mean(jire_data[jire_data$Year_Factor == '2016',]$Total_Time)
sd(jire_data[jire_data$Year_Factor == '2016',]$Total_Time)

median(jire_data[jire_data$Year_Factor == '2008',]$Action_Events)
IQR(jire_data[jire_data$Year_Factor == '2008',]$Action_Events)
median(jire_data[jire_data$Year_Factor == '2016',]$Action_Events)
IQR(jire_data[jire_data$Year_Factor == '2016',]$Action_Events)

median(jire_data[jire_data$Year_Factor == '2008',]$Action_Types)
IQR(jire_data[jire_data$Year_Factor == '2008',]$Action_Types)
median(jire_data[jire_data$Year_Factor == '2016',]$Action_Types)
IQR(jire_data[jire_data$Year_Factor == '2016',]$Action_Types)

median(jire_data[jire_data$Year_Factor == '2008',]$Strikes)
IQR(jire_data[jire_data$Year_Factor == '2008',]$Strikes)
median(jire_data[jire_data$Year_Factor == '2016',]$Strikes)
IQR(jire_data[jire_data$Year_Factor == '2016',]$Strikes)

median(jire_data[jire_data$Year_Factor == '2008',]$Nut_Positioning)
IQR(jire_data[jire_data$Year_Factor == '2008',]$Nut_Positioning)
median(jire_data[jire_data$Year_Factor == '2016',]$Nut_Positioning)
IQR(jire_data[jire_data$Year_Factor == '2016',]$Nut_Positioning)

median(jire_data[jire_data$Year_Factor == '2008',]$Reorient_Stones)
IQR(jire_data[jire_data$Year_Factor == '2008',]$Reorient_Stones)
median(jire_data[jire_data$Year_Factor == '2016',]$Reorient_Stones)
IQR(jire_data[jire_data$Year_Factor == '2016',]$Reorient_Stones)

median(jire_data[jire_data$Year_Factor == '2008',]$Tool_Changes)
IQR(jire_data[jire_data$Year_Factor == '2008',]$Tool_Changes)
median(jire_data[jire_data$Year_Factor == '2016',]$Tool_Changes)
IQR(jire_data[jire_data$Year_Factor == '2016',]$Tool_Changes)




#We additionally find some data for Velu. This is used for the text in the manuscript.
#Median strikes
Velu_data_op<-OP_nut_data[OP_nut_data$ID == 'VELU',]
median(Velu_data_op[Velu_data_op$Year_Factor == '1999',]$Strikes)
length(Velu_data_op[Velu_data_op$Year_Factor == '1999',]$Strikes)
median(Velu_data_op[Velu_data_op$Year_Factor == '2016',]$Strikes)
length(Velu_data_op[Velu_data_op$Year_Factor == '2016',]$Strikes)

#Median actions
median(Velu_data_op[Velu_data_op$Year_Factor == '1999',]$Action_Events)
median(Velu_data_op[Velu_data_op$Year_Factor == '2016',]$Action_Events)

#Time
mean(Velu_data_op[Velu_data_op$Year_Factor == '1999',]$Total_Time)
mean(Velu_data_op[Velu_data_op$Year_Factor == '2016',]$Total_Time)




#### Plotting Coula & Oil-Palm Comparison ####
#globals
alpha_points = 0.7
alpha_points = 0.3
alpha_boxes = 0.6
color_boxes = 'purple'


#Function for plotting
boxplot_coulaoil_years<-function(data_f, X, Y, Shape, alpha_point, alpha_box,color_point, color_box, xlabel,ylabel){
  ploT<-ggplot(data = data_f, aes(x = X, y =Y)) +
    geom_point(aes(color = Nut_Type), alpha = alpha_point) + theme_light()  + facet_wrap(~ID, ncol = 3) +
    geom_boxplot(data = data_f, aes(x = X, y = Y,color = Nut_Type), alpha = alpha_box) +
    xlab(xlabel) +
    ylab(ylabel) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), strip.background = element_rect(fill = "black")) +
    scale_color_manual(name = 'Nut Type',values = c('red','grey1'), labels=c("coula", "oil-palm"))
  return(ploT)
}

#To plot this data, we will only plot the individuals who had cracked both types of nuts (Tua, Jire and Yo)
nut_data_TAJRYO<-nut_data[nut_data$ID != 'FANA',]
nut_data_TAJRYO<-nut_data_TAJRYO[nut_data_TAJRYO$ID != 'VELU',]
#Restrict the plots to only be around years 2008, 2011 and 2016 so we can see before and after the coula nuts for comparison.
nut_data_TAJRYO<-nut_data_TAJRYO[nut_data_TAJRYO$Year_Factor != '1999',]
nut_data_TAJRYO<-nut_data_TAJRYO[nut_data_TAJRYO$Year_Factor != '2004',]

#Replace names for Jire and Yo to be capital and lowercase.
nut_data_TAJRYO[nut_data_TAJRYO$ID == 'JIRE',]$ID<-'Jire'
nut_data_TAJRYO[nut_data_TAJRYO$ID == 'YO',]$ID<-'Yo'

#Plot
time_coulaoil<-boxplot_coulaoil_years(data_f = nut_data_TAJRYO, X = nut_data_TAJRYO$Year_Factor, Y = nut_data_TAJRYO$Total_Time, 
                                      alpha_point = alpha_points, alpha_box = alpha_boxes, 
                                      color_point = nut_data_TAJRYO$Nut_Type,color_box = color_boxes, xlabel = 'Year', ylabel = 'Time (s)')
time_coulaoil
events_coulaoil<-boxplot_coulaoil_years(data_f = nut_data_TAJRYO, X = nut_data_TAJRYO$Year_Factor, Y =nut_data_TAJRYO$Action_Events, 
                                        alpha_point = alpha_points, alpha_box = alpha_boxes, 
                                        color_point = nut_data_TAJRYO$Nut_Type,color_box = color_boxes, xlabel = '', ylabel = 'Actions')

actiontype_coulaoil<-boxplot_coulaoil_years(data_f = nut_data_TAJRYO, X = nut_data_TAJRYO$Year_Factor, Y = nut_data_TAJRYO$Action_Types, 
                                            alpha_point = alpha_points, alpha_box = alpha_boxes, 
                                            color_point = nut_data_TAJRYO$Nut_Type,color_box = color_boxes, xlabel = '', ylabel = 'Action Types')

strikes_coulaoil<-boxplot_coulaoil_years(data_f = nut_data_TAJRYO, X = nut_data_TAJRYO$Year_Factor, Y = nut_data_TAJRYO$Strikes, 
                                         alpha_point = alpha_points, alpha_box = alpha_boxes, 
                                         color_point = nut_data_TAJRYO$Nut_Type,color_box = color_boxes, xlabel = '', ylabel = 'Strikes')

positions_coulaoil<-boxplot_coulaoil_years(data_f = nut_data_TAJRYO, X = nut_data_TAJRYO$Year_Factor, Y = nut_data_TAJRYO$Nut_Positioning,
                                           alpha_point = alpha_points, alpha_box = alpha_boxes, 
                                           color_point = nut_data_TAJRYO$Nut_Type,color_box = color_boxes, xlabel = '', ylabel = 'Nut (re)positions')

reorient_coulaoil<-boxplot_coulaoil_years(data_f = nut_data_TAJRYO, X = nut_data_TAJRYO$Year_Factor, Y = nut_data_TAJRYO$Reorient_Stones,
                                          alpha_point = alpha_points, alpha_box = alpha_boxes, 
                                          color_point = nut_data_TAJRYO$Nut_Type,color_box = color_boxes, xlabel = 'Year', ylabel = 'Tool Reorientations')

toolchanges_coulaoil<-boxplot_coulaoil_years(data_f = nut_data_TAJRYO, X = nut_data_TAJRYO$Year_Factor, Y = nut_data_TAJRYO$Tool_Changes, 
                                             alpha_point = alpha_points, alpha_box = alpha_boxes, 
                                             color_point = nut_data_TAJRYO$Nut_Type,color_box = color_boxes, xlabel = 'Year', ylabel = 'Tool Changes')

#saved as width 1000, height 800
display<- '
aabbcc
aaddee
aaffgg'
nut_type_combined<-wrap_plots(time_coulaoil,
                              events_coulaoil+ theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()),
                              actiontype_coulaoil+ theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()),
                              strikes_coulaoil+ theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()),
                              positions_coulaoil+ theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()),
                              reorient_coulaoil,toolchanges_coulaoil,design = display) + plot_layout(guides = "collect") + plot_annotation(tag_levels = c('a'), tag_suffix = '.')

nut_type_combined 






