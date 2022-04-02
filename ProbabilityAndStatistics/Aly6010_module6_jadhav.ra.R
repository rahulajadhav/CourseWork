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




### week 6 assignment
main_dataset <- read.csv("PSID (14 variables).csv")
names(main_dataset)
main_dataset$ln_wage <- log(wage2)
attach(main_dataset)
hist(ln_wage,
     main = "Histogram of Wages",
     xlab = "Natural log of wage")
unique(gender)
unique(married)
# step 1
lm_wage_edu <- lm(ln_wage~ education)
stargazer(lm_wage_edu,type="html", title="Regression table on Education",
          dep.var.labels = "Wage",
          convariate.lables= c("education"),out="Regression table on education.htm")

# step 2
main_dataset$dummy_Male <- ifelse(main_dataset$gender=="male",1,0)
main_dataset$dummy_married <- ifelse(main_dataset$married=="yes",1,0)
attach(main_dataset)
lm_dummy <- (lm(ln_wage~ education + dummy_Male+ dummy_married))
stargazer(lm_dummy,type="html", title="",
          dep.var.labels = "Wage",
          convariate.lables= c("education","Gender","Married"),out="Regression table on dummy.htm")



# step 3
male_dataset <- subset(main_dataset, main_dataset$gender=="male")
female_dataset <- subset(main_dataset, main_dataset$gender=="female")
summary(lm(data=male_dataset, ln_wage ~ education))
summary(lm(data=female_dataset, ln_wage ~ education))

lm_male <- (lm(data=male_dataset,ln_wage~ education))
lm_female <- (lm(data=female_dataset,ln_wage~ education))

stargazer(lm_male,lm_female ,type="html", title="",
          dep.var.labels = "wages",
          convariate.lables= c("education"),out="multi Regression.htm")

