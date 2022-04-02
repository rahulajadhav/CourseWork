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
main_dataset <- read.csv('small sample PSID (4 variables).csv')
male_dataset <- subset(main_dataset,main_dataset$gender=="male")
female_dataset <- subset(main_dataset,main_dataset$gender=="female")
two_sample_Unpaired_part1 <- rbind(cbind(Significance = "0.1",tidy(t.test(male_dataset$wage2,female_dataset$wage2,conf.level = 0.99))),
                          cbind(Significance = "0.001",tidy(t.test(male_dataset$wage2,female_dataset$wage2,conf.level = 0.999))))
two_sample_Unpaired_part1 <- two_sample_Unpaired_part1 %>% 
  select(Significance,
         estimate=estimate,
         "mean Wage of Female"=estimate1,
         "mean wage of Male"=estimate2,
         method=method,
         alternative=alternative,
         count=parameter,
         statistic,p.value,
         conf.low,conf.high) %>%
  mutate(
    count = round(count,2),
    statistic = round(statistic,2),
    p.value = round(p.value,2),
    conf.low = round(conf.low,2),
    conf.high = round(conf.high,2)
  )

write.csv(two_sample_Unpaired_part1,"two_sample_Unpaired_part1.csv")


##############################part2

year_1980 <- subset(main_dataset,main_dataset$year==1980)
year_1981 <- subset(main_dataset,main_dataset$year==1981)
dataset_merged <- merge(year_1980,year_1981,by=c("id"),all=TRUE)
dataset_merged
two_sample_paired_part2 <- rbind(cbind(Significance=0.1,tidy(t.test(dataset_merged$wage2.x,dataset_merged$wage2.y,paired = TRUE,conf.level = 0.99))),
                                 cbind(Significance=0.001,tidy(t.test(dataset_merged$wage2.x,dataset_merged$wage2.y,paired = TRUE,conf.level = 0.999))))
two_sample_paired_part2 <- two_sample_paired_part2 %>% 
  select(Significance,
         "mean_of_differences" = estimate,
         method=method,
         alternative=alternative,
         count=parameter,
         statistic,p.value,
         conf.low,conf.high) %>%
  mutate(
    count = round(count,2),
    mean_of_differences = round(mean_of_differences,2),
    statistic = round(statistic,2),
    p.value = round(p.value,2),
    conf.low = round(conf.low,2),
    conf.high = round(conf.high,2)
  )

write.csv(two_sample_paired_part2,"two_sample_paired_part2.csv")

