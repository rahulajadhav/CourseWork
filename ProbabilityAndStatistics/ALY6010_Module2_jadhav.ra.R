#importing libraries
packages<-(c("dplyr","psych","ggplot2","vtable","maps"))
package.check <- lapply(
  packages,
  function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
lapply(c("dplyr","psych","ggplot2","vtable","maps"),require, character.only = TRUE)



#importing csv using read.csv
getwd()
setwd("C:/Users/ralph/Desktop/studies/Probability/Module 2/practice")
main_dataset <- read.csv('deaths.csv')
headtail(main_dataset)
head(main_dataset)
class(main_dataset)
unique(main_dataset$age)
unique(main_dataset$ethnicity)
unique(main_dataset$race)
unique(main_dataset$sex)
unique(factor(main_dataset$county))
unique(main_dataset$chronic_condition)

factor(main_dataset$age)

min(unique(main_dataset$age))
max(unique(main_dataset$age)).

summary(main_dataset)
summary(as.integer(main_dataset$age))
str(main_dataset)

mean(main_dataset)
dplyr::count(main_dataset, race,age,ethnicity,sex,county,chronic_condition) 




.#Prepare data.frame for analysis. Arrange variables (rename, drop, or keep variables), clean data (gsub or conditional assignments with ifelse() ), and apply appropriate data structure (i.e. integer, numerical, character, factors, etc.)

cleaned_dataset <- main_dataset%>%
  mutate(
    age = as.numeric(ifelse(
      age == '90+',90,age
      )),
    sex = factor(sex,
                 levels = c('Female','Male','Unknown'),
                 labels = c('Female', 'Male','Unknown')),
    ethnicity = factor(ethnicity,
                       levels = c('Hispanic/ Latino','Non-Hispanic/ Latino','Total','Unknown'), 
                       labels = c('Hispanic/ Latino', 'Non-Hispanic/ Latino','Total','Unknown')),
    race = factor(race, 
                  levels = c('African-American/ Black','American Indian/ Alaska Native','Asian','Native Hawaiian/ Pacific Islander','Other','Total','Unknown','White'), 
                  labels = c('African-American/ Black', 'American Indian/ Alaska Native','Asian','Native Hawaiian/ Pacific Islander','Other','Total','Unknown','White')),
    Age_Group = factor(case_when(
      main_dataset$age >= '0' & main_dataset$age <='14' ~ "Children",
      main_dataset$age >= '15' & main_dataset$age <='24' ~ "Youth",
      main_dataset$age >= '25' & main_dataset$age <='64' ~ "Adults",
      main_dataset$age >= '64'~ "Seniors"
    )),
    chronic_condition = factor(chronic_condition,
                               levels = c("Yes","No","Unknown"),
                               labels = c("Yes","No","Unknown")
      ),
    county = factor(county)
  ) %>%
  mutate(
    race = factor(ifelse(
      race == "White", "White",
      ifelse(
        race== "African-American/ Black", "African-American/ Black",
        ifelse(
          race == "Asian", "Asian",
          "Other"
        )
      )
    )
  )
  )



summary(cleaned_dataset)




#summary

mytable1 <- function(DF) {
  table1 <- psych::describe(DF)
  library(dplyr)
  table1 <- table1 %>% select(count=n,mean,sd)
  table1$mean <- round(table1$mean,1)
  write.csv(table1,"Table 1.csv")
}


mytable1(cleaned_dataset)



###################################
summary_ethnicity <- cleaned_dataset %>%
  group_by(ethnicity) %>%
  summarize(count=n(),
            mean(age),
            sd(age))  %>%
  mutate(proportion = round(count / sum(count), 3)) %>% 
  arrange(desc(proportion))
write.csv(summary_ethnicity,"summary_ethnicity.csv")

summary_sex <- cleaned_dataset %>%
  group_by(sex) %>%
  summarize(count=n(),
            mean(age),
            sd(age))  %>%
  mutate(proportion = round(count / sum(count), 3)) %>% 
  arrange(desc(proportion))
write.csv(summary_sex,"summary_sex.csv")


summary_race <- cleaned_dataset %>%
  group_by(race) %>%
  summarize(count=n(),
            mean(age),
            sd(age))  %>%
  mutate(proportion = round(count / sum(count), 3),
         percentage = count/sum(count)*100) %>% 
  arrange(desc(proportion))
write.csv(summary_race,"summary_race.csv")


summary_sex <- cleaned_dataset %>%
  group_by(sex) %>%
  summarize(count=n(),
            mean(age),
            sd(age))  %>%
  mutate(proportion = round(count / sum(count), 3)) %>% 
  arrange(desc(proportion))
write.csv(summary_sex,"summary_sex.csv")

summary_county <- cleaned_dataset %>%
  group_by(county) %>%
  summarize(count=n(),
            mean(age),
            sd(age))  %>%
  mutate(proportion = round(count / sum(count), 3),
         percentage = count/sum(count)*100) %>% 
  arrange(desc(proportion))
write.csv(summary_county,"summary_county.csv")



#####################################3











t1 <- table(cleaned_dataset,cleaned_dataset$sex)





#data analysis and plotting

attach(cleaned_dataset)

ggplot(cleaned_dataset, aes(x=sex,fill=chronic_condition)) + 
  geom_bar(position = "dodge")

ggplot(cleaned_dataset, aes(x=sex,fill=Age_Group)) + 
  geom_bar(position = "dodge")


ggplot(summary_race,aes(x=race,y=proportion))+
  geom_bar(position="dodge", stat="identity")


pie(summary_sex$count,labels=summary_sex$sex)




