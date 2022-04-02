#importing libraries
packages<-(c("dplyr","psych","ggplot2","vtable","maps","fastDummies","broom"))
package.check <- lapply(
  packages,
  function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
lapply(c("dplyr","psych","ggplot2","vtable","maps","fastDummies","broom"),require, character.only = TRUE)



#importing csv using read.csv
getwd()
setwd("C:/Users/ralph/Desktop/studies/Probability/Module 3/R practice")
main_dataset <- read.csv('comorbidities (any) expanded.csv')


all_count <- count(main_dataset, ethnicity,sex,comorbidity) 

#summary



mytable1 <- function(DF) {
  table1 <- psych::describe(DF)
  library(dplyr)
  table1 <- table1 %>% select(count=n,mean,sd)
  table1$statistic <- (t.test(main_dataset$death,mu=0.042))$statistic
  table1$p_value <- (t.test(main_dataset$death,mu=0.042))$p.value
  table1$mean <- round(table1$mean,1)
  write.csv(table1,"Table 1.csv")
}


mytable1(summary_sex)

#####################################


binom_test <- tidy(binom.test(sum(main_dataset$death), n=sum(main_dataset$case), p=0.042))
t_test <- tidy(t.test(main_dataset$death,mu=0.042))
prop_test<-tidy(prop.test(sum(main_dataset$death),sum(main_dataset$case),p=0.042))



tests <- rbind(binom_test,t_test,prop_test)

table1 <- tests %>% select(method=method,count=parameter,mean=estimate,statistic=statistic,p_value=p.value,conf.low=conf.low,conf.high=conf.high)

write.csv(table1,"difftesting.csv")


###################################

male_subset<-subset(main_dataset,main_dataset$sex == "Male")
female_subset<-subset(main_dataset,main_dataset$sex == "Female")
unknown_subset <-subset(main_dataset,main_dataset$sex == "Unknown")

group_test <- rbind(tidy(t.test(main_dataset$death,mu=0.042)),
                    tidy(t.test(male_subset$death,mu=0.042)),
                    tidy(t.test(female_subset$death,mu=0.042)),
                    tidy(t.test(unknown_subset$death,mu=0.042))
                    )
table2 <- group_test %>% select(count=parameter,mean=estimate,statistic=statistic,p_value=p.value,conf.low=conf.low,conf.high=conf.high)
write.csv(table2,"group_t_testing.csv")



###############################################################


summary_sex <- main_dataset %>%
  group_by(sex) %>%
  summarize(count_cases = sum(case),
            count_death= sum(death)
  )  %>%
  mutate(proportion_cases = round(count_cases / sum(count_cases), 3),
         proportion_death = round(count_death / sum(count_death), 3)) %>% 
  arrange(desc(proportion_death))
write.csv(summary_sex,"summary_sex.csv")


ggplot(summary_sex,aes(x=sex,y=proportion_cases,fill=summary_sex$proportion_death))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=summary_sex$sex,vjust=-1))+
  theme(axis.text.x = element_text(angle = 0,size = 15))+
  labs(title = "Proportion of positive covid19 cases based on Gender")+
  labs(x="Gender",y="Proportion of cases")+
  labs(fill="death")

##################################

summary_ethnicity <- main_dataset %>%
  group_by(ethnicity) %>%
  summarize(count_cases = sum(case),
            count_death= sum(death)
  )  %>%
  mutate(proportion_cases = round(count_cases / sum(count_cases), 3),
         proportion_death = round(count_death / sum(count_death), 3)) %>% 
  arrange(desc(proportion_death))
write.csv(summary_ethnicity,"summary_ethnicity.csv")

ggplot(summary_ethnicity,aes(x=ethnicity,y=proportion_cases,fill=summary_ethnicity$proportion_death))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=summary_ethnicity$ethnicity,vjust=-1))+
  theme(axis.text.x = element_text(angle = 0,size = 15))+
  labs(title = "Proportion of positive covid19 cases based on Ethnicity")+
  labs(x="Ethnicity",y="Proportion of cases")+
  labs(fill="Death")

##################################

summary_race <- main_dataset %>%
  group_by(race) %>%
  summarize(count_cases = sum(case),
            count_death= sum(death),
            sex
  )  %>%
  mutate(proportion_cases = round(count_cases / sum(count_cases), 3),
         proportion_death = round(count_death / sum(count_death), 3)) %>% 
  arrange(desc(proportion_death))
write.csv(summary_race,"summary_race.csv")

ggplot(summary_race,aes(x=as.factor(sex),fill=as.factor(summary_race$race)))+
  geom_bar(position="dodge")+
  labs(x="sex",fill="race")+labs(title = "Covid-19 Possitive cases based on Sex and Race")+
  theme(axis.text.x = element_text(angle = 0,size = 19))+
  labs(fill="race")



##########################################
