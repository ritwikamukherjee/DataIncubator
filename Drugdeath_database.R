#Accidental Drug Related Deaths 2012-2018
rm(list = ls())
setwd("C:/Users/Ritwika Mukherjee/Downloads")
dat = read.csv("Accidental_Drug_Related_Deaths_2012-2018.csv")
str(dat)
library(tidyverse)
library(dplyr)
library(reshape2)
library(ggplot2)

table(dat$Age)
table(dat$Sex)
datnew <- subset(dat, Sex != "" & Sex != "Unknown") #selecting only males and females in the data
table(datnew$Sex)
table(datnew$Race)
datnew <- subset(dat, Race != "" & Race != "Unknown")
table(datnew$Race)
with(datnew, table(Age,Sex,Race))

table(datnew$MannerofDeath)
datnew$MannerofDeath <- tolower(datnew$MannerofDeath)
datnew <- subset(datnew, MannerofDeath != "" & MannerofDeath != "pending" & MannerofDeath != "natural")
table(datnew$MannerofDeath) #only taking accidents int account
summary=dplyr::count(datnew,Age,Sex,Race)
summary

datnew <- datnew %>% select(Race, Age, Sex) %>%
  gather(key = action, value = claim, 2, 3) %>% 
  count(race, action, claim) %>% 
  mutate(action = ifelse(action == "state_claim_made", "state", "civil")) %>%
  mutate(x = as.numeric(reorder(interaction(race, action), 1:n())))


library(MASS)
head(datnew)

# One suggested strategy for linear discriminant analysis (LDA) to see if there is a correlation of deaths with race, sex, and age
# Start by testing whether groups differ significantly using MANOVA
# Classify based on variables (e.g., with LDA) only if there is an overall significant difference

Y<-dplyr::select(datnew,Age, Race, Sex)
head(Y)

