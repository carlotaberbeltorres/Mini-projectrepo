#clear the working environment
rm(list=ls())

#set working directory to where I hae=ve saved the downloaded data to
setwd("C:/Users/cb821/Desktop/miniproject")

#download and require packages that may be needed, these are ones I could recall from previous work
install.packages("arm")
library(arm)
library(plyr)
library(car)
install.packages("ggbeeswarm")
library(ggbeeswarm)
library(ggplot2)
library(lattice)
install.packages("lawstat")
library(lawstat)
install.packages("outliers")
library(outliers)
install.packages("tidyverse")
library(tidyverse)
library(lmerTest)
library(HH)
library(FSA)
library(rstatix)
require(lmtest)
require(lme4)
require(usdm)
require(psych)
require(lmerTest)
require(sjPlot)
require(dplyr)


#load the data
#file was modified on excel so I will have emailed you a copy as 
#I had to add a different column onto the original data set
fishdata <- read.csv("fishdata.csv")
# check if family is a factor
str(fishdata)
# family is not factor so have to convert it
fishdata$Family <- as.factor(fishdata$Family)

# load environmental data 
environmentdata <- read.csv("Site_summary_Hollarsmith_et_al_Jbio2020.csv")
str(environmentdata)

#wrangle the data so that the two data files are merged into one and 
#the observation data is converted into binary presence/absence data
summary<-fishdata%>%filter(Group=="Fish")%>%group_by(Location, Video_code, Video_name, Family)%>%summarise(Count=n())
summary$Count<- ifelse(summary$Count>0, 1, 0)
summary2<- summary%>%pivot_wider(names_from = Family, values_from=Count)
summary2[is.na(summary2)]<- 0
joined<- merge(summary2, environmentdata, by=c("Video_code", "Video_name"), all=TRUE)
final<- na.omit(joined)
str(final)

#check fish data for NAs
colSums(is.na(final))
#no NAs were found
#check environmental data for NAs
colSums(is.na(environmentdata))
# no NAs were found

#Model 1 plotting Location as a random effect and only using Depth, Temperature and Habitat
#Poisson distribution
M1 <- glmer(as.numeric(Serranidae)~ scale(Depth_avg)+scale(Temp_avg)+as.factor(Habitat_biotic) +
                (1|Location), data=final, family="poisson")
summary(M1)
plot(M1)
plot_model(M1, show.values = TRUE, show.intercept = TRUE)
#model shows no significant results

#Model 2 using Location as a random effect and only using Depth, Temperature and substrate
#Poisson distribution
M2 <- glmer(as.numeric(Serranidae)~scale(Depth_avg)+scale(Temp_avg)+as.factor(Substrate_abiotic)+
              (1|Location), data=final, family = "poisson")
summary(M2)
plot(M2)
plot_model(M2, show.values = TRUE, show.intercept = TRUE)
#model shows no significant results

#Model 3 using Location as a random effect and using all four variables 
#Poisson distribution
M3 <- glmer(as.numeric(Serranidae)~ scale(Depth_avg)+scale(Temp_avg)+as.factor(Habitat_biotic) +
              as.factor(Substrate_abiotic) + (1|Location), data=final, family="poisson")
summary(M3)
plot(M3)
plot_model(M3, show.values = TRUE, show.intercept = TRUE)
#Model showed no significant results however it says it is rank deficient. Too many  variables modeled
#Model 3 will not be used as it discards some of observations

#qqplot to look at how the data points are distributed
qqnorm(resid(M2))
qqline(resid(M2))
# data seems reasonably distributed 

#Let's compare model 1 and model 2 to see how they differ
install.packages("MuMIn")
library(MuMIn)
model.sel(M1,M2)
# their AICs only differ by 0.1 therefore I will not discard the higher model 
#as I want to account all four variables

#emmeans to look at the comparisons between the different substrates and habitats
require(emmeans)
emmeans(M1, list(pairwise ~ Habitat_biotic), adjust = "tukey")
emmeans(M2, list(pairwise ~ Substrate_abiotic), adjust = "tukey")
# no significnt results as standard error is too high - sample size possibly too small.

#let's plot the table for model 1 and 2 and export it onto my working directory
tab_model(M1,M2)
tab_model(M1, M2, CSS = list(css.depvarhead = '+color: blue;'), file="m1m2.doc")

#plotting the results

library(ggplot2)
data(final, package="carData")

#converting binary data into presence and absence data
final <- mutate(final, Presence = ifelse(Serranidae == "1", "Present", "Absent"))

final$Serranidae <- as.factor(final$Serranidae)

# simple scatterplot with a line of best fit fitted
deptem <- ggplot(data = final, 
      mapping = aes(x = Depth_avg,
           y = Temp_avg)) +
    geom_point(mapping = aes(colour = Presence), size = 3) +
  geom_smooth(method = "lm", colour="indianred3")+
  labs(x = "Depth average (m)", y = "Temperature average (°C)",  title = "Figure.1")

#calling the patchwork package to merge all my figures into one so it is easier to report
library(tidyverse)
install.packages("patchwork")
library(patchwork)

substrate <-ggplot(data=final, 
       mapping = aes(x = Substrate_abiotic,
           fill = Presence)) + 
  geom_bar(position = "dodge", mapping = aes(colour=Presence))+
  labs(x = "Substrate", y = "Serranidae count",  title = "Figure.2")


habitat <- ggplot(data=final, 
                  mapping = aes(x = Habitat_biotic,
                                fill = Presence)) + 
  geom_bar(position = "dodge", mapping = aes(colour=Presence))+
  labs(x = "Habitat", y = "Serranidae count",  title = "Figure.3")

#combine both of the barplots into one plot 
subshab <- substrate + habitat
deptem+subshab

#references needed for my  report 
citation()
citation("lme4")
citation("ggplot2")
