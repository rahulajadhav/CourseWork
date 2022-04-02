library(dplyr)
library(corrplot)
library(ggplot2)
library(tidyverse)
library(FSA)
library(gridExtra)


heart_failure_prediction <- read.csv2("heart.csv",header=TRUE,sep=",")
heart_failure_prediction

corrplot(cor(heart_failure_prediction[sapply(heart_failure_prediction,is.numeric)]),method = "number",type = "upper")

heart_failure_prediction <- heart_failure_prediction%>%
  mutate(
    Sex = factor(Sex, levels = c('F','M'), labels = c('F', 'M')),
    ChestPainType = factor(ChestPainType, levels = c('ASY','ATA','NAP','TA'), labels = c('asymptomatic', 'atypical angina', 'non-anginal pain', 'typical angina')), 
    FastingBS = factor(FastingBS, levels = 0:1, labels = c('>120', '<=120')),
    RestingECG = factor(RestingECG, levels = c('Normal','LVH','ST'), labels = c('normal','left hypertrophy','ST-T abnormality')),
    ExerciseAngina = factor(ExerciseAngina, levels = c('N','Y'), labels = c('No', 'Yes')),
    ST_Slope = factor(ST_Slope, levels = c('Up','Flat','Down'), labels = c('Up','Down','Flat')),
    HeartDisease = factor(ifelse(HeartDisease == 0, 'Negative', 'Positive')),
  )  


summary(heart_failure_prediction)
str(heart_failure_prediction)
head(heart_failure_prediction)

x <- filterD(heart_failure_prediction,Sex=="F",HeartDisease=="Positive")

attach(heart_failure_prediction)


ggplot(heart_failure_prediction, aes(Sex, fill=HeartDisease)) + 
  geom_bar() +
  labs(fill="Disease", x="Gender", y="Number of patients")
  

ggplot(heart_failure_prediction, aes(Age, fill=HeartDisease)) + 
  geom_histogram(binwidth=1) +
  labs(fill="Disease Diagnosis", x="Age", y="Number of patients")


ggplot(heart_failure_prediction, aes(x = ChestPainType, fill = HeartDisease)) +
  geom_bar() +
  labs(title='Heart Disease Diagnosis by Chest Pain Results', x = 'Chest Pain', y = 'Diagnosis Count', fill = 'Heart Disease Diagnosis')

ggplot(heart_failure_prediction,aes(x=Age,y=Cholesterol,color=HeartDisease, size=Cholesterol))+
  geom_point(alpha=0.7)+xlab("Age") +
  ylab("Cholestoral")+
  guides(fill = guide_legend(title = "Gender"))


heart_failure_prediction <- heart_failure_prediction %>% 
  mutate(
  workout = sample(c("Yes","No"), 918, replace = TRUE)
  )

p1 <- ggplot(heart_failure_prediction, aes(Age, fill=HeartDisease)) + 
  geom_histogram(binwidth=1) +
  labs(fill="Disease Diagnosis", x="Age", y="Number of patients")

p2 <- ggplot(heart_failure_prediction, aes(Age, fill=workout)) + 
  geom_histogram(binwidth=1) +
  labs(fill="Workout", x="Age", y="Number of patients")


grid.arrange(p1, p2, nrow = 1)
