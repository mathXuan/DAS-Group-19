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

ggplot(data = animals, mapping = aes(x = factor(month), y = time_at_shelter)) +
  geom_boxplot(fill = "steelblue") +
  labs(x = "month", y = "time stay in shelter",
       title = "summary of month in time in shelter")

#Provide accurate data
table(animals$time_at_shelter,animals$month)


#3.Study intake_type
ggplot(data = animals, mapping = aes(x = factor(intake_type), y = time_at_shelter, fill = intake_type)) +
  geom_col(position = "dodge") +
  labs(x = "intake_type", y = "time stay in shelter",
       title = "the summary of intake_type in time in shelter") 

ggplot(data = animals, mapping = aes(x = factor(intake_type), y = time_at_shelter)) +
  geom_boxplot(fill = "steelblue") +
  labs(x = "intake_type", y = "time stay in shelter",
       title = "summary of intake_type in time in shelter") 

#4.study outcome_type
ggplot(data = animals, mapping = aes(x = factor(outcome_type), y = time_at_shelter, fill = month)) +
  geom_col(position = "dodge") +
  labs(x = "outcome_type", y = "time stay in shelter",
       title = "the summary of outcome_type in time in shelter") 

#Study with boxplot
ggplot(data = animals, mapping = aes(x = factor(outcome_type), y = time_at_shelter)) +
  geom_boxplot(fill = "steelblue") +
  labs(x = "outcome_type", y = "time stay in shelter",
       title = "summary of outcome_type in time in shelter") 

#5.study chip_status
ggplot(data = animals, mapping = aes(x = factor(chip_status), y = time_at_shelter, fill = chip_status)) +
  geom_col(position = "dodge") +
  labs(x = "chip_status", y = "time stay in shelter",
       title = "the summary of chip_status in time in shelter") 

#Study with boxplot
ggplot(data = animals, mapping = aes(x = factor(chip_status), y = time_at_shelter)) +
  geom_boxplot(fill = "steelblue") +
  labs(x = "chip_status", y = "time stay in shelter",
       title = "summary of ochip_status in time in shelter")


#Formal analysis
library(skimr)
library(moderndive)
library(sjPlot)
library(stats)
library(jtools)
skim(animals)

#ggpairs(animals)
#month和year两个变量完全分离，且month变量下time_at_shelter变化不大，只使用year作为explanatory variable
animals$month <- as.character(animals$month)
animals$year <- as.character(animals$year)

#考虑所有变量，拟合进counts数据型下的genralised linear regression model
#所有变量均为分类变量
mod.loglinear <- glm(time_at_shelter ~ year + animal_type + intake_type + outcome_type 
                 + chip_status, data = animals, family = poisson())
#输出模型的回归系数以及p-value
#对结果进行分析，考虑z statistic和p-value的值，提供以下处理方法：
#1. 对animal_type数据进行处理，WILDLIFE和BIRD之间似乎没有显著差异考虑归为一类
#2. 不对animal_type进行拟合
#3. 不对year进行拟合
#4. 在1的处理上不对year进行拟合
#5. 在2的处理上不对year进行拟合
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
#综合考虑模型的复杂度以及系数的显著性选择4方法进行处理
summary(mod.loglinear4)

#解释模型
#log—odds
confint(mod.loglinear4)

install.packages("sjPlot")
library(sjPlot)
plot_model(mod.loglinear4, show.values = TRUE, transform = NULL,
           title = "Log-Odds", show.p = FALSE)
#在原数据集上添加logmean列
animals <- animals %>%
  mutate(logmean = predict(mod.loglinear4))
#查看animals的新增列，根据lab上的相关内容进行解释

#RR_—— rate ratio
mod.loglinear4 %>%
  coef() %>%
  exp()

plot_model(mod.loglinear4, show.values = TRUE,, show.p = FALSE)
#在原数据集上添加rate-ratio列
animals <- animals %>%
  mutate(rate-ratio = exp(logodds))

#COUNTS
#在原数据集上添加probability列
#notes:Poisson分布和二项分布的区别
#在Poisson分布中,由于结果不是概率,所以解释为E(y)=u=n*theta,在这个案例中,不考虑不同的exposure,所以counts=u
animals <- animals %>%
  mutate(counts_pred = fitted(mod.loglinear4))

#将拟合结果可视化
plot_model(mod.loglinear4,type = "pred")
