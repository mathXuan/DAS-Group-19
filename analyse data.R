#读取包
library(ggplot2)
library(tidyverse)
library(dplyr)

#读取数据
setwd("E:/【A】格拉课件/Data Analysis Skills/group study2")
animals<-read.csv("dataset19.csv")

#通过绘制密度函数（因变量y）判断模型应该用哪种GLM
#我们首先研究y(在收容所的时间)服从何种密度函数
#unique(animals$time_at_shelter)
#ncol(animals)
#总体的密度函数
p<-ggplot(animals, aes(x = time_at_shelter))    
p + geom_density(color = "black", fill = "gray")
#密度函数（按动物分类）
p<-ggplot(animals, aes(x = time_at_shelter))+ 
  geom_density(aes(color = animal_type))

#观察每种变量的分布（通过箱线图和柱状图）
#1.因变量y
summary(animals$time_at_shelter)
table(animals$time_at_shelter,animals$animal_type)
#绘制箱线图看动物呆的时间（以种类分类）
ggplot(data = animals, mapping = aes(x = factor(animal_type), y = time_at_shelter)) +
  geom_boxplot(fill = "steelblue") +
  labs(x = "kind of animals", y = "time stay in shelter",
       title = "different animals in shelter")  
#用柱状图分别看不同种类动物时间
ggplot(data = animals, mapping = aes(x = factor(animal_type), y = time_at_shelter, fill = animal_type)) +
  geom_col(position = "dodge") +
  labs(x = "kind of animals", y = "time stay in shelter",
       title = "different animals in shelter") 

#2.用柱状图研究月份
ggplot(data = animals, mapping = aes(x = factor(month), y = time_at_shelter, fill = month)) +
  geom_col(position = "dodge") +
  labs(x = "month", y = "time stay in shelter",
       title = "the summary of months in time in shelter") 

ggplot(data = animals, mapping = aes(x = factor(month), y = time_at_shelter)) +
  geom_boxplot(fill = "steelblue") +
  labs(x = "month", y = "time stay in shelter",
       title = "summary of month in time in shelter")

#提供准确数据
table(animals$time_at_shelter,animals$month)

#把时间分成冬天和夏天
#但是通过箱线图可以发现不同月份时间无明显变化，可忽略变成两个季节
#View(animals)
last_time <- list(rep(0,nrow(animals)))
#class(last_time)
change_time<-function(x){
  for (i in 1:length(x)) {
    if(x[i]<=6){
      last_time[i]=1
    }else{
      last_time[i]=0
    }
  }
} 
last_time<-change_time(animals$month)
#length(animals$month)
#把数据增加到最后一列
animals_used<-animals%>%
  mutate(time=last_time)

#3.研究intake_type
ggplot(data = animals, mapping = aes(x = factor(intake_type), y = time_at_shelter, fill = intake_type)) +
  geom_col(position = "dodge") +
  labs(x = "intake_type", y = "time stay in shelter",
       title = "the summary of intake_type in time in shelter") 

ggplot(data = animals, mapping = aes(x = factor(intake_type), y = time_at_shelter)) +
  geom_boxplot(fill = "steelblue") +
  labs(x = "intake_type", y = "time stay in shelter",
       title = "summary of intake_type in time in shelter") 

#4.研究outcome_type
ggplot(data = animals, mapping = aes(x = factor(outcome_type), y = time_at_shelter, fill = month)) +
  geom_col(position = "dodge") +
  labs(x = "outcome_type", y = "time stay in shelter",
       title = "the summary of outcome_type in time in shelter") 

#用箱线图研究
ggplot(data = animals, mapping = aes(x = factor(outcome_type), y = time_at_shelter)) +
  geom_boxplot(fill = "steelblue") +
  labs(x = "outcome_type", y = "time stay in shelter",
       title = "summary of outcome_type in time in shelter") 

#5.研究chip_status
ggplot(data = animals, mapping = aes(x = factor(chip_status), y = time_at_shelter, fill = chip_status)) +
  geom_col(position = "dodge") +
  labs(x = "chip_status", y = "time stay in shelter",
       title = "the summary of chip_status in time in shelter") 

#用箱线图研究
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
