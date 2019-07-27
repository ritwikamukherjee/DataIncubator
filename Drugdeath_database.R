#Accidental Drug Related Deaths 2012-2018
rm(list = ls())
setwd("C:/Users/Ritwika Mukherjee/Downloads")
dat = read.csv("Accidental_Drug_Related_Deaths_2012-2018.csv")
str(dat)
library(tidyverse)
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggthemes)

table(dat$Age)
table(dat$Sex)
datnew <- subset(dat, Sex != "" & Sex != "Unknown") #selecting only males and females in the data
table(datnew$Sex)
table(datnew$Race)
datnew <- subset(datnew, Race != "" & Race != "Unknown" & Race !="Other" & Race !="Native American, Other" & Race !="Hawaiian")
table(datnew$Race)
with(datnew, table(Age,Sex,Race))

table(datnew$MannerofDeath)
datnew$MannerofDeath <- tolower(datnew$MannerofDeath)
datnew <- subset(datnew, MannerofDeath != "" & MannerofDeath != "pending" & MannerofDeath != "natural")
table(datnew$MannerofDeath) #only taking accidents int account
summary=dplyr::count(datnew,Age,Sex,Race)
summary

# datnew <- datnew %>% select(Race, Age, Sex) %>%
#   gather(key = action, value = claim, 2, 3) %>% 
#   count(race, action, claim) %>% 
#   mutate(action = ifelse(action == "state_claim_made", "state", "civil")) %>%
#   mutate(x = as.numeric(reorder(interaction(race, action), 1:n())))


library(MASS)
head(datnew)

# One suggested strategy for linear discriminant analysis (LDA) to see if there is a correlation of deaths with race, sex, and age
# Start by testing whether groups differ significantly using MANOVA
# Classify based on variables (e.g., with LDA) only if there is an overall significant difference

Y<-dplyr::select(datnew,Age, Race, Sex)
head(Y)
#labs <- c(paste(seq(10, 80, by = 10), seq(10 + 10 - 1, 80 +10 - 1, by = 10),
      #                          sep = "-"), paste(90, "+", sep = "")) #create age bins
labs <- c(paste(seq(0, 80, by = 10), seq(0 + 10 - 1, 90 - 1, by = 10),
                sep = "-"), paste(90, "+", sep = ""))
labs
Y$AgeGroup <- cut(Y$Age, breaks = c(seq(0, 90, by = 10), Inf), labels = labs, right = FALSE)
Y
dplyr::count(Y, Race, Sex, AgeGroup)
#Make new data frame with the necessary data to compare age, sex, and race of drug accidents
Ynew=Y %>%
  count(Race, Sex, AgeGroup) %>%
  mutate(x = as.numeric(reorder(interaction(Race, Sex), 1:n())))
Ynew

breaks = sort(c(unique(Ynew$x), seq(min(Ynew$x) + .5, 
                                   max(Ynew$x) + .5, 
                                   length(unique(Ynew$Sex))
)))

labels = unlist(
  lapply(unique(Ynew$Race), function(i) c("Male", paste0("\n", i), "Female"))
)

ggplot(Ynew, aes(x = x, y = n, fill = factor(AgeGroup))) +
  geom_col(show.legend = T) + 
  ggthemes::theme_few() +
  #scale_fill_manual(name = NULL,
   #                 values = c("gray75", "gray25"),
    #                breaks= c("0", "1"),
     #               labels = c("false", "true")
  #) +
  scale_x_continuous(breaks = breaks, labels = labels) +
  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(title = "Drug Accidents", y = "Count")
