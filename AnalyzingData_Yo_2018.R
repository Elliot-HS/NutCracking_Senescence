#### Senescence Analysis: Yo 2018 Duration ####
#### Made: 31.07.2024 ####
#### Last Edited: 23.11.2024 ####
#### Author Elliot Howard-Spink ####

#Clear workspace
rm(list=ls())

##Set directory
getwd()
setwd('Senesence_Master_JSub')

#### Import data
data<-read.csv('Data/Yo_2018.csv')

#Estimate mean duration of oil-palm nut cracking
#Block over encounters first, and then find the mean of the blocks.
mean(data[data$Encounter == '9',]$Duration..s.)
mean(data[data$Encounter == '11',]$Duration..s.)
mean(c(67.75, 80.58333)) #74.2s

sd(c(67.75, 80.58333))

# % change
((74.2-27.4)/27.4)*100
