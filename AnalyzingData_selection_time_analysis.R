#### Senescence Analysis: Stone Tool Selection  ####
#### Made: 24.07.2023 ####
#### Last Edited: 23.11.2024 ####
#### Author Elliot Howard-Spink ####


#Clear workspace
rm(list=ls())

#### Directory ####
getwd()
setwd("Senesence_Master_JSub")

#Packages
library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)
library(patchwork)
library(sjPlot) #For later Model Plot
library("RColorBrewer")

#### Import Data ####
selection_time_df<-read.csv('Data/tool_selection_times_with_available_tools.csv')
selection_time_df<-selection_time_df %>%
  filter(Type != 'None') #Remove any records where a stone tool was not actually taken - they are not included in this analysis



#### Manipulate Data ####
#Make ID and type a factor
selection_time_df$ID<-as.factor(selection_time_df$ID)
selection_time_df$Type<-as.factor(selection_time_df$Type)
#remove incomplete data
selection_time_df <- selection_time_df %>% #There is 1 NA
  filter(Previously_Taken != 'NA')
#Make an additional column that has the year data was collected.
#Save categorical 'Year' as 'Field_Season'
#Rename 'Type' as 'Stones_Selected'
names(selection_time_df)<-c('PAB','ID','Stones_Selected','Time','Field_Season','Previously_Taken')
#Use Field_Season to get initial year - save into new column 'Year'
for(i in 1:length(selection_time_df$Field_Season)){
  year <- selection_time_df$Field_Season[i]
  selection_time_df$Year[i]<-as.integer(substr(year,1, 4))
}
selection_time_df$Year<-as.numeric(selection_time_df$Year)
#set Field_Season as factor
selection_time_df$Field_Season<-as.factor(selection_time_df$Field_Season)
#drop unused levels
droplevels(selection_time_df)




#### Summarize ####

#Structure of data
str(selection_time_df)

#Total Sample for each individual
table(selection_time_df$ID)

#And by year:
table(selection_time_df[selection_time_df['Field_Season'] == '1999_2000',]$ID) #1999_2000
table(selection_time_df[selection_time_df['Field_Season'] == '2004_2005',]$ID) #2004_2005
table(selection_time_df[selection_time_df['Field_Season'] == '2008_2009',]$ID) #2008_2009
table(selection_time_df[selection_time_df['Field_Season'] == '2011_2012',]$ID) #2011_2012
table(selection_time_df[selection_time_df['Field_Season'] == '2016_2017',]$ID) #2016-2017



#### Visualize Data ####

#Histogram of time data
ggplot(data = selection_time_df, aes(x = Time, fill = ID)) +
  geom_histogram(aes(fill = ID)) +
  facet_wrap(~ID) +
  theme_light() + theme(strip.background = element_rect(fill = "black")) 
#Time data seems to have a slightly skewed distribution for some individuals (e.g. Tua and Velu)
#This can be made more normal through a log transformation:
selection_time_df$Log_Time<-log(selection_time_df$Time)
ggplot(data = selection_time_df, aes(x = Log_Time, fill = ID)) +
  geom_histogram(aes(fill = ID)) +
  facet_wrap(~ID) +
  theme_light() + theme(strip.background = element_rect(fill = "black")) 


#### Modelling ####

#Rescale the explanatory variable 'year'
selection_time_df$Year_Scaled <- scale(selection_time_df$Year, center = TRUE, scale = TRUE)
head(selection_time_df) #Now contains a scaled parameter.


#Given senescence is known to have varying effects on individuals, including for both cognitive and physical systems, I will fit models with a random slope for age.
#This is also supported by visual inspection of the data which suggests slopes of different directions between individuals.
#For model selection, I will start with the most complex model (Fixed effects: Year, Stones_Selected, Previously_Taken), and a random slope
#When comparing fixed effects (i.e. the effect of field season), I will use ML to fit models. Once we have the optimum model, I will re-fit the model using REML to ensure that estimates of variance explained
#by grouping variables is accurate (see https://ourcodingclub.github.io/tutorials/mixed-models/#what accessed 25.07.23)
#I will then compare my random slope model with a fixed slope model to ensure that the additional complexity is warranted.
#The most complex model: FIXED: Year, Stones Selected, Stones Previously Taken; RANDOM: ID (inc. slope) and PAB (excluding slope).
#A model which drops Year as both a fixed and random effect. 
#A completely null model which assumes no influence of any fixed effects 

Model_Year_ToolNo_Previous <- lmer(Log_Time ~ Year_Scaled + Stones_Selected + Previously_Taken + (1 + Year_Scaled||ID) + (1|PAB), data = selection_time_df, REML = F) #most complex
Model_ToolNo_Previous <- lmer(Log_Time ~ Stones_Selected + Previously_Taken + (1|ID)+ (1|PAB), data = selection_time_df, REML = F) #drop year
Model_Null<-lmer(Log_Time ~ 1 + (1|ID)+ (1|PAB), data = selection_time_df, REML = F) 
AIC(Model_Year_ToolNo_Previous,Model_ToolNo_Previous,Model_Null) 

#Set REML to true when comparing random effects
Model_Year_ToolNo_Previous <- lmer(Log_Time ~ Year_Scaled + Stones_Selected + Previously_Taken + (1 + Year_Scaled||ID) + (1|PAB), data = selection_time_df, REML = T)
model_fixed_slope<-lmer(Log_Time ~ Year_Scaled + Stones_Selected + Previously_Taken + (1|ID) + (1|PAB), data = selection_time_df, REML = T)
AIC(Model_Year_ToolNo_Previous,model_fixed_slope) #Model with random slopes is much better

#For model summary,
#RESET TO REML FALSE TO COMPARE FIXED EFFECTS
Model_Year_ToolNo_Previous <- lmer(Log_Time ~ Year_Scaled + Stones_Selected + Previously_Taken + (1 + Year_Scaled||ID) + (1|PAB), data = selection_time_df, REML = F)
model_fixed_slope<-lmer(Log_Time ~ Year_Scaled + Stones_Selected + Previously_Taken + (1|ID) + (1|PAB), data = selection_time_df, REML = F)

#Best model is therefore random slope model, which accounts for stones taken during the selection event, number of stones previously removed from the matrix, random slope for ID and ID intercept, and encounter (PAB) as a random factor.
summary(Model_Year_ToolNo_Previous)
coef(Model_Year_ToolNo_Previous)

#### Evaluate Model and Assumptions ####
#We will be continuing with the model with all fixed effects

#check assumptions - qqplot
qqnorm(resid(Model_Year_ToolNo_Previous)) 
qqline(resid(Model_Year_ToolNo_Previous))
#That looks good.

plot(Model_Year_ToolNo_Previous)
#Evenly dispersed



#### PLOTS ####

#Prepare names for facet
labeller = labeller(ID = 
                      c("FANA" = "Fana",
                        "JIRE" = "Jire",
                        "TUA" = "TUA",
                        "VELU" = "Velu",
                        "YO" = "Yo"))


#Plotting data for each individual
stone_selection_over_years<-ggplot(selection_time_df, aes(x = Year, y = Log_Time, color = Previously_Taken)) +
  geom_smooth(method = 'lm', se = T, aes(group = 1), alpha = 0.4, color = 'black') +
  geom_point(aes(shape = Stones_Selected)) + 
  theme_light() +
  theme(panel.background = element_rect(fill = "white"),
        plot.margin = margin(0, 0, 0.5, 0, "cm")) +
  #scale_x_continuous(breaks=c(1998,2000,2002,2004,2006,2008,2012,2014,2016)) + #I don't want to change the axis, but I'm leaving this here in case I want to later.
  facet_wrap(~ID, ncol = 5, labeller = labeller) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.3), strip.background = element_rect(fill = "black")) +
  ylab(expression(Log~Time~(s))) + 
  xlab('Year') + 
  labs(tag = "a.  ") +
  scale_color_gradientn(name = 'Prior Stones Removed', colours = c("midnightblue", "magenta2", "lightpink1", "orange")) + 
  scale_shape_manual(name = 'Stones Selected',values = c(1,2), labels=c("One", "Two")) 
stone_selection_over_years



#Models predicted for the observed data
#model summary
summary(Model_Year_ToolNo_Previous)
#Global Intercept
GI <- 1.85559
#Global Slope for Year
GS <- 0.13714
#coef
coef(Model_Year_ToolNo_Previous)

Model_prediction_plot<-ggplot() +
  geom_point(data = selection_time_df,                      # adding the raw data (scaled values)
             aes(x = Year_Scaled, y = Log_Time, fill = ID), shape = 21, size = 2,show.legend = FALSE, alpha = 0.7, color = 'grey4') + 
  geom_abline(aes(intercept = 1.925030, slope = 0.26910761, color = 'Fana')) + #Fana
  geom_abline(aes(intercept = 1.802579, slope = 0.16995580, color = 'Jire')) + #Jire
  geom_abline(aes(intercept = 1.385767, slope = -0.17443754, color = 'TUA')) + #Tua
  geom_abline(aes(intercept = 1.756572, slope = 0.06966496, color = 'Velu')) + #Velu
  geom_abline(aes(intercept = 2.407988, slope = 0.35142374, color = 'Yo')) + #Yo
  geom_abline(aes(intercept = GI, slope = GS, color = 'Baseline')) + #Global
  labs(tag = 'b.',x = "Scaled Year", y = "Log Time (s)") + 
  ylim(0,4) +
  scale_fill_manual(values = c('FANA' = "#332288",'JIRE' = '#AA4499','TUA' = '#DDCC77','VELU' = '#44AA99','YO' = '#88CCEE')) +
  scale_colour_manual(name = 'Model',values=c('Fana' = "#332288",'Jire' = '#AA4499','TUA' = '#DDCC77','Velu' = '#44AA99','Yo' = '#88CCEE','Baseline' = 'black'),
                      breaks = c('Fana','Jire','TUA','Velu','Yo','Baseline')) +
  #ggtitle("Model Prediction: Duration of Tool Selection Vs. Year (Scaled)") +
  theme_light() 
Model_prediction_plot


#saved at width 1000 height 500
design<-'AAAABB'
patch<-wrap_plots(stone_selection_over_years,Model_prediction_plot, design = design) 
patch
