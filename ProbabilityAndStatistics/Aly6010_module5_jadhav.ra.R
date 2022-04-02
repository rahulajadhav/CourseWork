#importing libraries
packages<-(c("dplyr","psych","ggplot2","vtable","maps","fastDummies","broom","ggcorrplot","stargazer"))
package.check <- lapply(
  packages,
  function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
lapply(c("dplyr","psych","ggplot2","vtable","maps","fastDummies","broom","ggcorrplot","stargazer"),require, character.only = TRUE)



#importing csv using read.csv
getwd()
setwd("C:/Users/ralph/Desktop/studies/Probability/Module 5/R practice/")
main_dataset <- read.csv('Guns_1999.csv')
cor(main_dataset)
unique(main_dataset$state)

sub_dataset = main_dataset %>%
  select(
    robbery,prisoners,afam,
    cauc,male,population,income
  ) %>%
  mutate (
    prisoners = as.numeric(prisoners)
  )
cor_table <- cor(sub_dataset,method = c("pearson"))

cor_test <- cor.test(sub_dataset$robbery, sub_dataset$income, method=c( "pearson"))
write.csv(cor_table, "cor_table.csv")

ggcorrplot(cor_table, type = "lower", lab = TRUE)


##########################regression
lm_robberyvsincome<-lm(sub_dataset$robbery~sub_dataset$income)

plot(sub_dataset$income,sub_dataset$robbery,main="Scatterplot",pch=19,
     xlab = "real per capita personal income in the state (US dollars)"
     ,ylab = "robbery rate (incidents per 100,000)")
abline(lm_robberyvsincome)


lm_robberyvsmale<-lm(sub_dataset$robbery~sub_dataset$male)
plot(sub_dataset$male,sub_dataset$robbery,main="Scatterplot",pch=16,
     xlab = "state wise percentage population of Male, ages 10 to 29"
     ,ylab = "robbery rate (incidents per 100,000)")
abline(lm_robberyvsmale)


stargazer(lm_robberyvsincome,lm_robberyvsmale,type="html", title="Regression table of Guns_1999",
          convariate.lables= c("income","male"),out="model.htm")
