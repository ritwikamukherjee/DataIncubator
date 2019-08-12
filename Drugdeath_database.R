#Accidental Drug Related Deaths 2012-2018
rm(list = ls())
setwd("C:/Users/Ritwika Mukherjee/Desktop/DataIncubator")
dat = read.csv("Accidental_Drug_Related_Deaths_2012-2018.csv")
str(dat)
library(tidyverse)
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(lme4) # for running mixed models
library(car) # for statistical hypothesis tests
library(bbmle)

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

#Y$Populationratio <- NA
# for(i in 1:nrow(Y)){
#   if (is.null(Y[i]) || Y[i] == '') {Y$Populationratio == 0}
#   else if (Y$Race[i] == "White") {Y$Populationratio[i] == 0.816 } 
#   else if (Y$Race[i] == "Black"){ Y$Populationratio[i] == 0.091 }
#   else if (Y$Race[i] == "Hispanic, White"){ Y$Populationratio[i] == 0.002 }
#   else if (Y$Race[i] == "Hispanic, Black"){ Y$Populationratio[i] == 0.002 }
#   else if (Y$Race[i] == "Chinese"){ Y$Populationratio[i] == 0.006 }
#   else if (Y$Race[i] == "Asian, Other"){ Y$Populationratio[i] == 0.01 }
#   else if (Y$Race[i] == "Asian Indian"){ Y$Populationratio[i] == 0.007 }
#   else {Y$Populationratio[i] == 0.001}
# }
# 

#Make new data frame with the necessary data to compare age, sex, and race of drug accidents
Ynew=Y %>%
  count(Race, Sex, AgeGroup) %>%
  mutate(x = as.numeric(reorder(interaction(Race, Sex), 1:n()))) %>%
  mutate(g = ifelse(Race == "White",0.816,
            ifelse(Race == "Hispanic, White",0.002,
            ifelse(Race == "Hispanic, Black",0.002, 
            ifelse(Race == "Chinese",0.006,     
              ifelse(Race == "Asian, Other",0.01, 
                     ifelse(Race == "Asian Indian",0.007,
              ifelse(Race == "Black", 0.091, NA))))))))  %>%
 mutate(y= n/(g*sum(n))) #normalize the count to the demograph. The mutation of g is pooled from data of Baltimore population. 
                     ###https://www.infoplease.com/us/comprehensive-census-data-state/demographic-statistics-40
Ynew


breaks = sort(c(unique(Ynew$x), seq(min(Ynew$x) + .5, 
                                   max(Ynew$x) + .5, 
                                   length(unique(Ynew$Sex))
)))

labels = unlist(
  lapply(unique(Ynew$Race), function(i) c("Male", paste0("\n", i), "Female"))
)

drugaccidents<-ggplot(Ynew, aes(x = x, y = y, fill = factor(AgeGroup))) +
  geom_col(show.legend = T) + 
  ggthemes::theme_few() +
  #scale_fill_manual(name = NULL,
   #                 values = c("gray75", "gray25"),
    #                breaks= c("0", "1"),
     #               labels = c("false", "true")
  #) +
  scale_x_continuous(breaks = breaks, labels = labels) +
  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(title = "Drug Accidents", y = "Count of accidents normalized to population")
ggsave("DrugAccidents.png", plot = drugaccidents, height = 5 , width= 10,units="in",  dpi=600)


#### Discussing the relationship of the drugs with the age group, race, and sex of the population
D<-dplyr::select(datnew, Heroin, Cocaine, Fentanyl, FentanylAnalogue, Oxycodone, Oxymorphone, Ethanol, Hydrocodone,
                 Benzodiazepine, Methadone,Amphet, Tramad, Morphine_NotHeroin, Hydromorphone, Other,OpiateNOS, AnyOpioid)
head(D)

D=data.frame(ifelse(D == "Y",1,0))
D<-data.frame(cbind("Race"=Y$Race,"Sex"=Y$Sex,"Agegroups"= Y$AgeGroup, D))
str(D)
D<-D%>%mutate(x = as.numeric(reorder(interaction(Race, Sex,Agegroups), 1:n()))) 

library(ropls)
Dselection<-dplyr::select(D, -Race, -Sex, -Agegroups, -x) #X data
Dselection<-Dselection[, colSums(Dselection != 0) > 0]

#PLSDA doesn't work on binary data. Using Multiple correspondence analysis (MCA) instead.
# drug.plsda <-
#   opls(
#     Dselection, #X data
#     D$Sex, #Y data
#     plotL = FALSE, #suppresses default plotting
#     predI = 1, #make one predictive axis
#     orthoI = 1, #and one orthogonal axis
#     #permI = 200
#     ) #use 200 permutations to generate a p-value #not working


#Using MCA which is similar to PCA but on quantitative data. 
#install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")
D<-dplyr::select(datnew, Heroin, Cocaine, Fentanyl, FentanylAnalogue, Oxycodone, Oxymorphone, Ethanol, Hydrocodone,
                 Benzodiazepine, Methadone,Amphet, Tramad, Morphine_NotHeroin, Hydromorphone, Other,OpiateNOS, AnyOpioid)

res.mca <- MCA(D, graph = FALSE)
print(res.mca)
fviz_mca_biplot(res.mca)
fviz_mca_biplot(res.mca, 
                #repel = TRUE, 
                ggtheme = theme_minimal()) ###Fentanyl and Other are different from other drugs. Least correlated. But also the correlations are not strong.

#Only 1.3% described in dimension
# Contributions of rows to dimension 1
fviz_contrib(res.mca, choice = "var", axes = 1, top = 15) 
#### Oxycodon, Oxymorphone, Heroin,Benzodiazepine, Morphine,Opiate, Cocaine, Oxycodon are the biggest contributors to the first dimension of correspondence
# Contributions of rows to dimension 2
fviz_contrib(res.mca, choice = "var", axes = 2, top = 15)
# Total contribution to dimension 1 and 2
fviz_contrib(res.mca, choice = "var", axes = 1:2, top = 15)
biplot1<-fviz_mca_var(res.mca, col.var = "contrib",
   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
   repel = TRUE, # avoid text overlapping 
   ggtheme = theme_minimal())
ggsave("Biplot1.png", plot = biplot1, height = 10 , width= 10,units="in",  dpi=600) #ugly biplot. Hard to see.



D<-dplyr::select(D, -FentanylAnalogue, -Morphine_NotHeroin)
res2.mca <- MCA(D, graph = FALSE)
fviz_mca_biplot(res2.mca, 
                #repel = TRUE, 
                ggtheme = theme_minimal()) ###Correlation increased. 
# Contributions of rows to dimension 1
fviz_contrib(res2.mca, choice = "var", axes = 1, top = 8) #selecting the top 8 contributors
#### Oxycodon, Oxymorphone, Heroin,Benzodiazepine, Cocaine, Oxycodon are the biggest contributors to the first dimension of correspondence
# Contributions of rows to dimension 2
fviz_contrib(res2.mca, choice = "var", axes = 2, top = 15)
# Total contribution to dimension 1 and 2
fviz_contrib(res2.mca, choice = "var", axes = 1:2, top = 15)
biplot2<-fviz_mca_var(res2.mca, col.var = "contrib",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            repel = TRUE, # avoid text overlapping 
            ggtheme = theme_minimal())

#Oxycodone and Oxymorphone are the biggest contributors. However, there is no directionality towards age, race, or sex yet.
ggsave("Biplot2.png", plot = biplot2, height = 10 , width= 10,units="in",  dpi=600)


#######Try plotting with an interaction of Sex and Race
D<-dplyr::select(datnew, Heroin, Cocaine, Fentanyl, FentanylAnalogue, Oxycodone, Oxymorphone, Ethanol, Hydrocodone,
                 Benzodiazepine, Methadone,Amphet, Tramad, Morphine_NotHeroin, Hydromorphone, Other,OpiateNOS, AnyOpioid)
D<-data.frame(cbind("Race"=Y$Race,"Sex"=Y$Sex,"Agegroups"= Y$AgeGroup, D))
str(D)
D<-D%>%mutate(x = as.numeric(reorder(interaction(Race, Sex), 1:n()))) 
str(D)
D<-dplyr::select(D,-Race, -Sex, -Agegroups,-Other, -FentanylAnalogue,-Morphine_NotHeroin )
D$x<-factor(D$x)
res3.mca <- MCA(D, graph = FALSE)
fviz_mca_biplot(res3.mca)
fviz_contrib(res3.mca, choice = "var", axes = 1:2, top = 5) #Oxycodon and Oxymorphone are the biggest contributors

# data frame with variable coordinates
cats = apply(D, 2, function(x) nlevels(as.factor(x)))
cats
mca1_vars_df = data.frame(res3.mca$var$coord, Variable = rep(names(cats), cats))
# data frame with observation coordinates
mca1_obs_df = data.frame(res3.mca$ind$coord)

# plot of variable categories
ggplot(data=mca1_vars_df, 
       aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR")


#####Location of death affected by sex and race groups
L<-dplyr::select(datnew, Race, Sex, Location)
L$Place <- NA
L$Place <- Loc$Location
for(i in 1:nrow(L)){
  if(L$Place[i] == "" | L$Place[i] == "Other") { L$Place[i] <- "Other" } else if
  (L$Place[i] == "Nursing Home" | L$Place[i] == "Convalescent Home"| L$Place[i] == "Hospice" ) { L$Place[i] <- "Nursing Home" }
}
head(L)
Loc=dplyr::count(L,Place,Sex,Race)

L1 <- Loc %>%
   mutate(x = as.numeric(reorder(interaction(Race, Sex), 1:n())))%>%
   mutate(g = ifelse(Race == "White",0.816,
                  ifelse(Race == "Hispanic, White",0.002,
                         ifelse(Race == "Hispanic, Black",0.002, 
                                ifelse(Race == "Chinese",0.006,     
                                       ifelse(Race == "Asian, Other",0.01, 
                                              ifelse(Race == "Asian Indian",0.007,
                                                     ifelse(Race == "Black", 0.091, NA))))))))  %>%
  mutate(y= n/(g*sum(n))) #normalize the count to the demograph. The mutation of g is pooled from data of Baltimore population. 
###https://www.infoplease.com/us/comprehensive-census-data-state/demographic-statistics-40
L1

breaks = sort(c(unique(L1$x), seq(min(L1$x) + .5, 
                                    max(L1$x) + .5, 
                                    length(unique(L1$Sex))
)))

labels = unlist(
  lapply(unique(L1$Race), function(i) c("Male", paste0("\n", i), "Female"))
)

drugaccidentsLoc<-ggplot(L1, aes(x = x, y = y, fill = factor(Place))) +
  geom_col(show.legend = T) + 
  ggthemes::theme_few() +
  #scale_fill_manual(name = NULL,
  #                 values = c("gray75", "gray25"),
  #                breaks= c("0", "1"),
  #               labels = c("false", "true")
  #) +
  scale_x_continuous(breaks = breaks, labels = labels) +
  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(title = "Drug Deaths based on Location", y = "Count of accidents normalized to population")

drugaccidentsLoc
ggsave("Drugdeathlocation.png", plot = drugaccidentsLoc, height = 10 , width= 10,units="in",  dpi=600)

Locrace=dplyr::count(L,Place,Race)
Lrace <- Locrace %>%
  mutate(g = ifelse(Race == "White",0.816,
                    ifelse(Race == "Hispanic, White",0.002,
                           ifelse(Race == "Hispanic, Black",0.002, 
                                  ifelse(Race == "Chinese",0.006,     
                                         ifelse(Race == "Asian, Other",0.01, 
                                                ifelse(Race == "Asian Indian",0.007,
                                                       ifelse(Race == "Black", 0.091, NA))))))))  %>%
  mutate(y= n/(g*sum(n))) #normalize the count to the demograph. The mutation of g is pooled from data of Baltimore population. 
###https://www.infoplease.com/us/comprehensive-census-data-state/demographic-statistics-40
Lrace

drugaccidentsLocrace<-ggplot(Lrace, aes(x = Race, y = y, fill = factor(Place))) +
  geom_col(show.legend = T) + 
  ggthemes::theme_few() +
  #scale_fill_manual(name = NULL,
  #                 values = c("gray75", "gray25"),
  #                breaks= c("0", "1"),
  #               labels = c("false", "true")
  #) +
  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(title = "Drug Deaths based on Location", y = "Count of accidents normalized to population")

drugaccidentsLocrace
ggsave("Drugdeathlocation2.png", plot = drugaccidentsLocrace, height = 10 , width= 10,units="in",  dpi=600)

Deaths=xtabs(formula=n~Place, data=Lrace)  #68.36% All population died in residence 
                                              ##or random alleyways instead of at cared for places
#DeathsPlace
#Convalescent Home           Hospice          Hospital      Nursing Home 
#0                 0                 0              1586                 5 
#Other         Residence 
#786              2652 
Deaths=xtabs(formula=n~Place+Race, data=Lrace)   #55.81% Black; 61.8% Hispanic White; 70.70% White are found dead in non hospital locations


####Taking only 2018 data and counting deaths. Last two years
Z<-datnew
class(datnew$Date)
Z$Date2 <- as.Date( as.character(datnew$Date), "%m/%d/%Y")
Zrecent <- subset(Z, Z$Date2 > as.Date("2018-01-01"),)
str(Zrecent)

Znew=Zrecent %>%
  dplyr::count(Race, Sex) %>%
  mutate(x = as.numeric(reorder(interaction(Race, Sex), 1:n()))) %>%
  mutate(g = ifelse(Race == "White",0.816,
                    ifelse(Race == "Hispanic, White",0.002,
                           ifelse(Race == "Hispanic, Black",0.002, 
                                  ifelse(Race == "Chinese",0.006,     
                                         ifelse(Race == "Asian, Other",0.01, 
                                                ifelse(Race == "Asian Indian",0.007,
                                                       ifelse(Race == "Black", 0.091, NA))))))))  %>%
  mutate(y= n/(g*sum(n))) #normalize the count to the demograph. The mutation of g is pooled from data of Baltimore population. 
###https://www.infoplease.com/us/comprehensive-census-data-state/demographic-statistics-40
Znew
breaks = sort(c(unique(Znew$x), seq(min(Znew$x) + .5, 
                                  max(Znew$x) + .5, 
                                  length(unique(Znew$Sex))
)))
labels = unlist(
  lapply(unique(Znew$Race), function(i) c("Male", paste0("\n", i), "Female"))
)
Drugdeaths2017_2018<-ggplot(Znew, aes(x =x , y = y)) +
  geom_col(show.legend = T) + 
  ggthemes::theme_few() +
  #scale_fill_manual(name = NULL,
  #                 values = c("gray75", "gray25"),
  #                breaks= c("0", "1"),
  #               labels = c("false", "true")
  #) +
  scale_x_continuous(breaks = breaks, labels = labels) +
  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(title = "Drug Accidents", y = "Count of accidents normalized to population")
### 1001 deaths in 2018 out of 3,572,665. That is 28 deaths in 100,000 involving 
### drugs in Connecticut, which is twofold higher than the national rate of 
###14.6 deaths per 100,000 persons.
ggsave("Drugdeaths2018.png", plot = Drugdeaths2017_2018, height = 10 , width= 10,units="in",  dpi=600)

Z2017 <- subset(Z, Z$Date2 < as.Date("2018-01-01") & Z$Date2 > as.Date("2017-01-01") ,)
Z2016 <- subset(Z, Z$Date2 < as.Date("2017-01-01") & Z$Date2 > as.Date("2016-01-01") ,)
Z2015 <- subset(Z, Z$Date2 < as.Date("2016-01-01") & Z$Date2 > as.Date("2015-01-01") ,)
Z2014 <- subset(Z, Z$Date2 < as.Date("2015-01-01") & Z$Date2 > as.Date("2014-01-01") ,)
Z2013 <- subset(Z, Z$Date2 < as.Date("2014-01-01") & Z$Date2 > as.Date("2013-01-01") ,)
Z2012 <- subset(Z, Z$Date2 < as.Date("2013-01-01") & Z$Date2 > as.Date("2012-01-01") ,)
countyears<-c(as.integer(count(Z2012)),as.integer(count(Z2013)),as.integer(count(Z2014)),as.integer(count(Z2015)),as.integer(count(Z2016)),as.integer(count(Z2017)))
years<-2012:2017
heroindeaths<-c(150,220,300,380,450,425)
syntheticdeaths<-c(20,55,76,200,79,686) #data from CDC WONDER
total<-data.frame(years,countyears,heroindeaths,syntheticdeaths)
Yeardeaths<-ggplot(total,aes(x = years , y = countyears, fill=years))+
  geom_bar(stat="identity") +
  labs(title = "Drug Deaths based on Year", y = "Count of drug accidents")
Yeardeaths<-Yeardeaths + 
  geom_point(data=total, aes(x=years, y=syntheticdeaths,color="green"))+
  geom_line(data=total, aes(x=years, y=syntheticdeaths,color="green"))+
  geom_point(data=total, aes(x=years, y=heroindeaths,color="yellow"))+
  geom_line(data=total, aes(x=years, y=heroindeaths,color="yellow"))

Yeardeaths<-Yeardeaths +scale_color_manual("Opioid deaths", values=c(green="green",yellow="yellow"),
                               labels=c("Synthetic Opioid","Heroin"))

ggsave("Yeardeaths.png", plot = Yeardeaths, height = 10 , width= 10,units="in",  dpi=600)
#In 2017, Connecticut providers wrote 48.0 opioid prescriptions for every 100 persons (Figure 2) compared to the average U.S. rate of 58.7 opioid prescriptions. 
#Prescription rates are decreasing.Yet overdosage is increasing
#####Looking at strong opioids for data in the year 2018
Zopioid<-dplyr::select(Zrecent, Race, Sex,Heroin, Cocaine, Fentanyl, FentanylAnalogue, Oxycodone, Oxymorphone, Ethanol, Hydrocodone,
                 Benzodiazepine, Methadone,Amphet, Tramad, Morphine_NotHeroin, Hydromorphone)

Zopioid<-Zopioid%>%
  mutate(x = as.numeric(reorder(interaction(Race, Sex), 1:n())))  %>%
  mutate(Morphine = ifelse(Morphine_NotHeroin == "Y", 'Y',
                 ifelse(Morphine_NotHeroin == "YES", 'Y','')))%>%
  mutate(Fentanyl = ifelse(Fentanyl == "Y", 'Y',
                           ifelse(Fentanyl == "YES", 'Y','')))
Zopioid$Morphine<-as.factor(Zopioid$Morphine)
Zopioid$Fentanyl<-as.factor(Zopioid$Fentanyl)
str(Zopioid)
Zopioid<-dplyr::select(Zopioid, -Morphine_NotHeroin)
str(Zopioid)
Zopioid.active<- dplyr::select(Zopioid, -Race,-Sex,-x) 
str(Zopioid.active)
summary(Zopioid.active)
for (i in 1:14) {
  plot(Zopioid.active[,i], main=colnames(Zopioid.active)[i],
       ylab = "Count", col="steelblue", las = 2)
}
### summary(Zopioid.active)
#Heroin  Cocaine Fentanyl FentanylAnalogue Oxycodone Oxymorphone Ethanol Hydrocodone Benzodiazepine
#:618    :664    :253     :755             :940      :989        :751    :987        :738         
#Y:383   Y:337   Y:748    Y:246            Y: 61     Y: 12       Y:250   Y: 14       Y:263         
#Methadone Amphet  Tramad  Hydromorphone Morphine
#:916      :945    :964    :994          :999   
#Y: 85     Y: 56   Y: 37   Y:  7         Y:  2 


#Morphine, Tramad, Amphet, Hydrocodone, Oxymorphone,and Hydromorphone are very low in frequency and are removed from analysis
Zopioid<-dplyr::select(Zrecent, Race, Sex,Heroin, Cocaine, Fentanyl, FentanylAnalogue, Oxycodone, Ethanol,
                       Benzodiazepine, Methadone)

Zopioid<-Zopioid%>%
  mutate(x = as.numeric(reorder(interaction(Race, Sex), 1:n())))  %>%
  mutate(Fentanyl = ifelse(Fentanyl == "Y", 'Y',
                           ifelse(Fentanyl == "YES", 'Y','')))
Zopioid$Fentanyl<-as.factor(Zopioid$Fentanyl)
Zopioid.active<- dplyr::select(Zopioid, -Race,-Sex,-x) 
for (i in 1:ncol(Zopioid.active)) {
  plot(Zopioid.active[,i], main=colnames(Zopioid.active)[i],
       ylab = "Count", col="steelblue", las = 2)
}
Zopioid.inactive<-dplyr::select(Zopioid,Sex)
str(Zopioid.active)
summary(Zopioid.active)
Opioid.mca <- MCA(Zopioid.active, graph = FALSE)
print(Opioid.mca)
fviz_mca_biplot(Opioid.mca)#Rows (individuals) are represented by blue points and columns (variable categories) by red triangles.
fviz_screeplot(Opioid.mca, addlabels = TRUE, ylim = c(0, 45))#19.4% data explained by first dimension and 14.5% from the second
fviz_contrib(Opioid.mca, choice = "var", axes = 1:2, top = 8)#Methadone, Fentanyl and analogue, and Benzodiazepine explain most of the differences
fviz_mca_var(Opioid.mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
    #####Methadone, Heroin,Benzodiazepine most correlated with second dimension
    #####Cocaine, Oxycodone,Fentanyl and it analogue most correlated with first dimension
Opioid.desc <- dimdesc(Opioid.mca, axes = c(1,2))
Opioid.desc[[1]]
fviz_mca_ind(Opioid.mca, 
             label = "none", # hide individual labels
             habillage = "Methadone", # color by groups 
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal()) 
ggsave("IndividualMCA2018_Methadone.png", plot = last_plot(), height = 10 , width= 10,units="in",  dpi=600)
# fviz_mca_ind(Opioid.mca, col.ind = "cos2", 
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              ggtheme = theme_minimal())
var <- get_mca_var(Opioid.mca)
library("corrplot")
corrplot(var$contrib, is.corr = FALSE)
ggsave("Correlation.png", plot = last_plot(), height = 10 , width= 10,units="in",  dpi=600)

SexMCA<-fviz_mca_ind(Opioid.mca, label = "none", habillage=Zrecent$Sex)
RaceMCA<-fviz_mca_ind(Opioid.mca, label = "none", habillage=Zrecent$Race)
ggsave("SexMCA2018.png", plot = SexMCA, height = 10 , width= 10,units="in",  dpi=600)
ggsave("RaceMCA2018.png", plot = RaceMCA, height = 10 , width= 10,units="in",  dpi=600)

ZopioidALL<-dplyr::select(Z, Race, Sex,Heroin, Cocaine, Fentanyl, FentanylAnalogue, Oxycodone, Ethanol,
                       Benzodiazepine, Methadone)

ZopioidALL<-ZopioidALL%>%
  mutate(x = as.numeric(reorder(interaction(Race, Sex), 1:n())))  %>%
  mutate(Fentanyl = ifelse(Fentanyl == "Y", 'Y',
                           ifelse(Fentanyl == "YES", 'Y','')))
ZopioidALL$Fentanyl<-as.factor(ZopioidALL$Fentanyl)
ZopioidALL.active<- dplyr::select(ZopioidALL, -Race,-Sex,-x) 
OpioidALLYEARS.mca <- MCA(ZopioidALL.active, graph = FALSE)
fviz_mca_biplot(OpioidALLYEARS.mca)#Rows (individuals) are represented by blue points and columns (variable categories) by red triangles.
fviz_screeplot(OpioidALLYEARS.mca, addlabels = TRUE, ylim = c(0, 45))#19.4% data explained by first dimension and 14.5% from the second
fviz_contrib(OpioidALLYEARS.mca, choice = "var", axes = 1:2, top = 8)#Methadone, Fentanyl and analogue, and Benzodiazepine explain most of the differences
fviz_mca_var(OpioidALLYEARS.mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
var <- get_mca_var(OpioidALLYEARS.mca)
corrplot(var$contrib, is.corr = FALSE)
SexMCAall<-fviz_mca_ind(OpioidALLYEARS.mca, label = "none", habillage=Z$Sex,addEllipses = TRUE, ellipse.level = 0.95)
RaceMCAall<-fviz_mca_ind(OpioidALLYEARS.mca, label = "none", habillage=Z$Race,addEllipses = TRUE, ellipse.level = 0.95)
ggsave("SexMCA_Allyrs.png", plot = SexMCAall, height = 10 , width= 10,units="in",  dpi=600)
ggsave("RaceMCA_Allyears.png", plot = RaceMCAall, height = 10 , width= 10,units="in",  dpi=600)
