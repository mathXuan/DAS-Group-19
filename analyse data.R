#load package
library(ggplot2)
library(tidyverse)
library(dplyr)

#load data
setwd("E:/Data Analysis Skills/group study2")
animals<-read.csv("dataset19.csv")

#Plot the density function (dependent variable y) to determine which GLM the model should use
#We first examine what density function y(time spent in the shelter) obeys
#unique(animals$time_at_shelter)
#ncol(animals)
#The density function of the population
p<-ggplot(animals, aes(x = time_at_shelter))    
p + geom_density(color = "black", fill = "gray")
#Density function (by animal)
p<-ggplot(animals, aes(x = time_at_shelter))+ 
  geom_density(aes(color = animal_type))

#Observe the distribution of each variable (by box plot and bar chart)
#1.Dependent variable y
summary(animals$time_at_shelter)
table(animals$time_at_shelter,animals$animal_type)
#Draw a boxplot to see how long the animals stay (by species)
ggplot(data = animals, mapping = aes(x = factor(animal_type), y = time_at_shelter)) +
  geom_boxplot(fill = "steelblue") +
  labs(x = "kind of animals", y = "time stay in shelter",
       title = "different animals in shelter")  
#Use a bar chart to see the time of different kinds of animals
ggplot(data = animals, mapping = aes(x = factor(animal_type), y = time_at_shelter, fill = animal_type)) +
  geom_col(position = "dodge") +
  labs(x = "kind of animals", y = "time stay in shelter",
       title = "different animals in shelter") 

#Separate out each animal species
animals_used1<-animals%>%
  filter(animals$animal_type=="DOG")

#View(animals)
#View(animals_used3)
#Dog
ggplot(data = animals_used1, mapping = aes(x = time_at_shelter)) +
  geom_histogram(binwidth=1,fill="#69b3a2", 
                 color="#e9ecef", alpha=0.9)+
  theme_bw()+
  labs(x = "time stay in the shelter", y = "sum of different days",
       title = "Dog's the summary of time in shelter") 

#unique(animals$animal_type)
#
animals_used2<-animals%>%
  filter(animals$animal_type=="CAT")

ggplot(data = animals_used2, mapping = aes(x = time_at_shelter)) +
  geom_histogram(binwidth=1,fill="#69b3a2", 
                 color="#e9ecef", alpha=0.9)+
  theme_bw()+
  labs(x = "time stay in the shelter", y = "sum of different days",
       title = "CAT's the summary of time in shelter") 

#BIRD
animals_used3<-animals%>%
  filter(animals$animal_type=="BIRD")

ggplot(data = animals_used3, mapping = aes(x = time_at_shelter)) +
  geom_histogram(binwidth=1,fill="#69b3a2", 
                 color="#e9ecef", alpha=0.9)+
  theme_bw()+
  labs(x = "time stay in the shelter", y = "sum of different days",
       title = "BIRD's the summary of time in shelter") 
#WILDLIFE
animals_used4<-animals%>%
  filter(animals$animal_type=="WILDLIFE")

ggplot(data = animals_used4, mapping = aes(x = time_at_shelter)) +
  geom_histogram(binwidth=15,fill="#69b3a2", 
                 color="#e9ecef", alpha=0.9)+
  theme_bw()+
  labs(x = "time stay in the shelter", y = "sum of different days",
       title = "WILDLIFE's the summary of time in shelter") 


#2.Study the months in a bar chart
ggplot(data = animals, mapping = aes(x = factor(month), y = time_at_shelter, fill = month)) +
  geom_col(position = "dodge") +
  labs(x = "month", y = "time stay in shelter",
       title = "the summary of months in time in shelter") 

ggplot(data = animals,  mapping = aes(x = factor(month))) + 
  geom_bar(stat = 'count',fill = "steelblue")+
  labs(x = "month", y = "time stay in shelter",
       title = "summary of month in time in shelter")

#Provide accurate data
table(animals$time_at_shelter,animals$month)


#3.Study intake_type
ggplot(data = animals, mapping = aes(x = factor(intake_type), y = time_at_shelter, fill = intake_type)) +
  geom_col(position = "dodge") +
  labs(x = "intake_type", y = "time stay in shelter",
       title = "the summary of intake_type in time in shelter") 

ggplot(data = animals,  mapping = aes(x = factor(intake_type))) + 
  geom_bar(stat = 'count',fill = "steelblue")+
  labs(x = "intake_type", y = "time stay in shelter",
       title = "summary of intake_type in time in shelter") 

#4.study outcome_type
ggplot(data = animals,  mapping = aes(x = factor(outcome_type))) + 
  geom_bar(stat = 'count',fill = "steelblue")+
  labs(x = "outcome_type", y = "time stay in shelter",
       title = "the summary of outcome_type in time in shelter") 

#Study with boxplot
ggplot(data = animals, mapping = aes(x = factor(outcome_type), y = time_at_shelter)) +
  geom_boxplot(fill = "steelblue") +
  labs(x = "outcome_type", y = "time stay in shelter",
       title = "summary of outcome_type in time in shelter") 

#5.study chip_status
ggplot(data = animals,  mapping = aes(x = factor(chip_status))) + 
  geom_bar(stat = 'count',fill = "steelblue")+
  labs(x = "chip_status", y = "time stay in shelter",
       title = "the summary of chip_status in time in shelter")

#Study with boxplot
ggplot(data = animals, mapping = aes(x = factor(chip_status), y = time_at_shelter)) +
  geom_boxplot(fill = "steelblue") +
  labs(x = "chip_status", y = "time stay in shelter",
       title = "summary of ochip_status in time in shelter")
#6.study year
ggplot(data = animals, mapping = aes(x = factor(year), y = time_at_shelter)) +
  geom_boxplot(fill = "steelblue") +
  labs(x = "year", y = "time stay in shelter",
       title = "summary of year in time in shelter")




#Formal analysis
library(skimr)
library(moderndive)
library(sjPlot)
library(stats)
library(jtools)
skim(animals)

#ggpairs(animals)
#The month and year are separated, and the time_at_shelter does not change much under the month variable, so only the year is used as the explanatory variable.
animals$month <- as.character(animals$month)
animals$year <- as.character(animals$year)

#Fitting a generalised linear regression model for the Counts datatype, given all variables
#full model: use all variables
mod.loglinear <- glm(time_at_shelter ~ year + animal_type + intake_type + outcome_type 
                 + chip_status, data = animals, family = poisson())
#print the regression coefficient and the p-value
#Analyse the result, considering the values of z statistic and p-value, to provide the following treatment.
#1.no significant difference between WILDLIFE and BIRD when animal_type data is processed, so consider grouping them together
#2.no fit to animal_type
#3.no fit to year
#4.no fit to year based on treatment 1
#5.no fit to year based on treatment 2
summary(mod.loglinear)
AIC(mod.loglinear)

#1
animals$animal_type_alternative <- if_else(animals$animal_type == "BIRD" | animals$animal_type == "WILDLIFE","NOT PET","PET")
mod.loglinear1 <- glm(time_at_shelter ~ year + animal_type_alternative + intake_type + outcome_type 
                     + chip_status, data = animals, family = poisson())
summary(mod.loglinear1)

#2
mod.loglinear2 <- glm(time_at_shelter ~ year + intake_type + outcome_type 
                      + chip_status, data = animals, family = poisson())
summary(mod.loglinear_alt2)


#3
mod.loglinear3 <- glm(time_at_shelter ~ animal_type + intake_type + outcome_type 
                      + chip_status, data = animals, family = poisson())
summary(mod.loglinear3)

#4
mod.loglinear4 <- glm(time_at_shelter ~ animal_type_alternative + intake_type + outcome_type 
                      + chip_status, data = animals, family = poisson())
summary(mod.loglinear4)

#5
mod.loglinear5 <- glm(time_at_shelter ~ intake_type + outcome_type 
                      + chip_status, data = animals, family = poisson())
summary(mod.loglinear5)

#model selection: Minimize AIC
AIC(mod.loglinear)
AIC(mod.loglinear1,mod.loglinear2,mod.loglinear3,mod.loglinear4,mod.loglinear5)
#Considering the complexity of all models and the significance of the coefficients, method 4 was chosen.
summary(mod.loglinear4)

#interpret the model
#log—odds
confint(mod.loglinear4)

install.packages("sjPlot")
library(sjPlot)
plot_model(mod.loglinear4, show.values = TRUE, transform = NULL,
           title = "Log-Odds", show.p = FALSE)
#Add logmean column to the original dataset
animals <- animals %>%
  mutate(logmean = predict(mod.loglinear4))

#RR_—— rate ratio
mod.loglinear4 %>%
  coef() %>%
  exp()

plot_model(mod.loglinear4, show.values = TRUE,, show.p = FALSE)
#Add rate-ratio column to the original dataset
animals <- animals %>%
  mutate(rate-ratio = exp(logodds))

#COUNTS
#Add the probability column to the original dataset
#In the Poisson distribution, since the outcome variable is not the probability, it is interpreted as E(y)=u=n*theta
#in this case without considering the different exposures, so counts=u.
animals <- animals %>%
  mutate(counts_pred = fitted(mod.loglinear4))

#Visualize the fit results
plot_model(mod.loglinear4,type = "pred")
