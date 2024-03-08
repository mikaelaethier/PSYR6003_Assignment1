#Setting the working directory 
setwd("/Users/mikaelaethier/Desktop/Dalhousie/PSYR6003/PSYR6003_Assignment1/")
##Load tidyverse and other libraries necessary for data cleaning and manipulation
library(tidyverse)
library(readxl)
library(haven)
library(dplyr)
###Loading the avengers dataset 
avengers <- read_csv("avengers.csv")
####Viewing the dataset 
view(avengers)
#2. Subsetting the data to include only complete, clean cases 
##adding an ID column in order to better identify participants 
avengers2<-mutate(avengers, ID=row_number())
view(avengers2)
###Counting the total number of cases in the dataset
count(avengers2)
#####Removing any incomplete cases after visually inspecting for erroneous cases 
avengersclean<-na.omit(avengers2)
view(avengersclean)
######counting the number of participants in the remaining dataset
count(avengersclean)
#######Create combateffectiveness variable consisting of a sum score of agility, speed, strength, and willpower
newavengers<-mutate(avengersclean, CombatEffectiveness=agility+speed+strength+willpower)
view(newavengers)
#3. Creating a new copy of the dataset in both SPSS and csv formats that only includes the avengers who did not have a superpower and have died. 
##Filtering only avengers who have died and had no superpowers 
deadavenger_nopowers<-filter(newavengers,died=="yes", superpower=="no")
view(deadavenger_nopowers)
write.csv(deadavenger_nopowers,"deadavenger_nopowers.csv",row.names=F,na="")
write_sav(deadavenger_nopowers, "deadavenger_nopowers.sav")
###Summarizing the dataset to understand the properties of combat effectiveness, kills, and injuries including the mean, SD, and range values for the overall sample.
OverallAvengers<-summarise(deadavenger_nopowers,avg_combateffectiveness = mean(CombatEffectiveness), sd_combateffectiveness = sd(CombatEffectiveness), min_combateffectiveness=min(CombatEffectiveness), max_combateffectiveness=max(CombatEffectiveness), 
          avg.kills = mean(kills), sd_kills = sd(kills), min_kills=min(kills), max_kills=max(kills),
          avg.injuries = mean(injuries), sd_injuries = sd(injuries), min_injuries=min(injuries), max_injuries=max(injuries))
####creating new datasets including either north or south avengers
NorthAvengers<-filter(deadavenger_nopowers, north_south=="north")
SouthAvengers<-filter(deadavenger_nopowers, north_south=="south")
#####Summarizing the dataset to understand combat effectiveness, kills, and injuries for North and south battlefields 
NorthAvengers_Descriptives<-summarise(NorthAvengers,avg_combateffectiveness = mean(CombatEffectiveness), sd_combateffectiveness = sd(CombatEffectiveness), min_combateffectiveness=min(CombatEffectiveness), max_combateffectiveness=max(CombatEffectiveness), 
          avg.kills = mean(kills), sd_kills = sd(kills), min_kills=min(kills), max_kills=max(kills),
          avg.injuries = mean(injuries), sd_injuries = sd(injuries), min_injuries=min(injuries), max_injuries=max(injuries))
SouthAvengers_Descriptives<-summarise(SouthAvengers,avg_combateffectiveness = mean(CombatEffectiveness), sd_combateffectiveness = sd(CombatEffectiveness), min_combateffectiveness=min(CombatEffectiveness), max_combateffectiveness=max(CombatEffectiveness), 
          avg.kills = mean(kills), sd_kills = sd(kills), min_kills=min(kills), max_kills=max(kills),
          avg.injuries = mean(injuries), sd_injuries = sd(injuries), min_injuries=min(injuries), max_injuries=max(injuries))
#8. Conducting a power analysis for an independent samples t-test 
##Loading the pwr package for the power analysis, and the TOSTER package for the equivalence testing
library(pwr)
library(TOSTER)
###Conducting the power analysis to determine the necessary sample size to achieve an effect of d=0.2, at a two-sided alpha level of 0.05, with 80% power
pwr.t.test(n = NULL, d = .2, sig.level = 0.05, power = .8, 
           type = c("two.sample"),
           alternative = c("two.sided"))
#9. Conducting an equivalence test 
##Determining the power to detect a zero effect: alpha = 0.05, n=394 per group as determined by the previous power analysis, equivalence bounds of Cohen's d = -0.2 and Cohen's d = 0.2, assuming true effect = 0
### Was unsure whether I should be using the sample size from the previous question or the total sample size collected due to question wording 
powerTOSTtwo(N=394, alpha=0.05, low_eqbound_d=-0.2, high_eqbound_d=0.2)

###Determining the effect size and confidence interval if the t-test statistic is 4.25 
####Using the total sample size excluding incomplete cases of 812, was unsure due to question wording as to whether it would be this sample or the sample achieved from the previous questions
#####loading the effectsize library to conduct the t_to_d function 
library(effectsize)
#####creating new objects for the t statistic and the total sample size 
t<-4.24
n<-812 
######determining the effect size based on the t statistic. 
t_to_d(t, df_error = n-2, paired = FALSE)
