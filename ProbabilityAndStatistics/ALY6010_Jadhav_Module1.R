#importing libraries
packages<-(c("dplyr","psych","ggplot2"))
package.check <- lapply(
  packages,
  function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

lapply(c("dplyr","psych","ggplot2"),require, character.only = TRUE)



#importing csv using read.csv
getwd()
setwd('C:/Users/ralph/Desktop/studies/Probability/Module 1')
main_dataset <- read.csv('comorbidities (any).csv')
headTail(main_dataset)
class(main_dataset)


#Prepare data.frame for analysis. Arrange variables (rename, drop, or keep variables), clean data (gsub or conditional assignments with ifelse() ), and apply appropriate data structure (i.e. integer, numerical, character, factors, etc.)
main_dataset <- main_dataset%>%
  mutate(
    sex = factor(sex, levels = c('Female','Male','Unknown'), labels = c('Female', 'Male','Unknown')),
    ethnicity = factor(ethnicity, levels = c('Hispanic/ Latino','Non-Hispanic/ Latino','Total','Unknown'), labels = c('Hispanic/ Latino', 'Non-Hispanic/ Latino','Total','Unknown')),
    race = factor(race, levels = c('African-American/ Black','American Indian/ Alaska Native','Asian','Native Hawaiian/ Pacific Islander','Other','Total','Unknown','White'), labels = c('African-American/ Black', 'American Indian/ Alaska Native','Asian','Native Hawaiian/ Pacific Islander','Other','Total','Unknown','White')),
    )

main_dataset <- rename(main_dataset, SEX=sex,Ethnicity= ethnicity, Race= race, Total_cases = cases, Total_deaths = deaths)
clean_dataset <- subset(main_dataset,Ethnicity != 'Total' & Race != 'Total')
headTail(clean_dataset)
attach(clean_dataset)



#Create new variables or new observations (such as proportion or total count) to aid in your analysis. Do you need to create new columns or new rows? You may want to use dplyr or tidyr commands to aggregate by groups.
clean_dataset <- clean_dataset %>%
  mutate(
    cases_in_percentage = round(Total_cases/sum(Total_cases)*100,4), 
    death_in_percentage = round(Total_deaths/sum(Total_deaths)*100,4), 
    cases_proportion = (proportions(Total_cases)),
    deaths_proportion = (proportions(Total_deaths))
  )
head(clean_dataset)




mytable <- function(df,outname) {
  t0 <- psych::describe(df)
  t0 <- t0 %>% select(mean, sd) %>% 
    mutate(mean=round(mean,2)) %>% 
    mutate(sd=round(sd,2)) %>% 
    rename(Mean=mean, SD=sd)
  write.csv(t0, outname)
}
mytable(clean_dataset,"module1.csv")


table1 <- table(clean_dataset$ethnicity,clean_dataset$race)
table1
write.table(table1, file = "olstab.txt", sep = ",", quote = FALSE, row.names = F)

#data analysis and plotting

hist(Total_cases,Total_deaths)

ggplot(clean_dataset, aes(fill=death_in_percentage, y=cases_in_percentage, x=race)) + 
  geom_bar(position="dodge", stat="identity")


sum(clean_dataset$Total_cases)
sum(clean_dataset$Total_deaths)

t1 <- subset(clean_dataset,race=="African-American/ Black")
sum(t1$Total_deaths)

ggplot(clean_dataset, aes(fill=Ethnicity, y=cases_in_percentage, x=sex)) + 
  geom_bar(position="dodge", stat="identity",)

ggplot(clean_dataset, aes(fill=Ethnicity, y=cases_in_percentage, x=race)) + 
  geom_bar(position="dodge", stat="identity",)
