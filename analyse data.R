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




