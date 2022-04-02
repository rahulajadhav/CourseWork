#importing libraries
packages<-(c("dplyr","psych","ggplot2","vtable","maps","fastDummies","broom","ggpubr","stargazer"))
package.check <- lapply(
  packages,
  function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
lapply(c("dplyr","psych","ggplot2","vtable","maps","fastDummies","broom","stargazer"),require, character.only = TRUE)



#importing csv using read.csv
main_dataset <- read.csv('NLSY1979_1994.csv')
getwd()
###cleaned dataset


cleaned_dataset <- main_dataset %>%
  mutate(
    YEAR_OF_BIRTH = case_when(
      YEAR_OF_BIRTH == "59" ~ 1959,
      YEAR_OF_BIRTH == "61" ~ 1961,
      YEAR_OF_BIRTH == "60" ~ 1960,
      YEAR_OF_BIRTH == "64" ~ 1964,
      YEAR_OF_BIRTH == "58" ~ 1958,
      YEAR_OF_BIRTH == "63" ~ 1963,
      YEAR_OF_BIRTH == "57" ~ 1957,
      YEAR_OF_BIRTH == "62" ~ 1962,
    ),
    GENDER = factor(GENDER,
                    levels = c("FEMALE","MALE"),
                    labels = c("FEMALE","MALE")),
    REGION_ = factor(ifelse(
      REGION_ == "1: NORTHEAST","NORTHEAST",
      ifelse(
        REGION_ == "4: WEST","WEST",
        ifelse(
          REGION_ == "2: NORTH CENTRAL", "NORTH CENTRAL",
          ifelse(
            REGION_ == "3: SOUTH", "SOUTH", 
            "Unspecified"
            )
          )
        )
      )
    ),
    MARSTAT_KEY_ = factor(ifelse(
      MARSTAT_KEY_ == "1: 1  MARRIED","MARRIED",
      ifelse(
        MARSTAT_KEY_ == "2: 2  SEPARATED","SEPARATED",
        ifelse(
          MARSTAT_KEY_ == "3: 3  DIVORCED", "DIVORCED",
          ifelse(
            MARSTAT_KEY_ == "0: 0  NEVER MARRIED", "NEVER MARRIED",
            ifelse(
              MARSTAT_KEY_ == "6: 6  WIDOWED", "WIDOWED",
              "Unspecified"
              )
            )
          )
        )
      )
      ),
    WKSUEMP_PCY_= ifelse(
      WKSUEMP_PCY_ == -3, NA,
      main_dataset$WKSUEMP_PCY
      ),
    URBAN_RURAL_= factor(ifelse(
      URBAN_RURAL_ == "1: URBAN", "URBAN",
      ifelse(
        URBAN_RURAL_ == "0: RURAL", "RURAL",
        "Unspecified"
        )
    )
    ),
    INCOME_= ifelse(
      INCOME_ == -1 | INCOME_ == -2 | INCOME_ == -3 | INCOME_ == -4, 0,
      main_dataset$INCOME_
          ),
    EDU_DEGREE= factor(ifelse(
      EDU_DEGREE == "-5", "UNKNOWN",
      ifelse(
        EDU_DEGREE == "Doctoral Degree (PhD)" | EDU_DEGREE == "Other (SPECIFY)" | EDU_DEGREE == "Professional Degree (MD, LLD, DDS)", "Professional, Doctoral and Other",
        main_dataset$EDU_DEGREE
          )
        )),
    MAJOR_1_ = factor(ifelse(
      MAJOR_1_ >= 100 & MAJOR_1_ <=199, "Agriculture and Natural Resources",
      ifelse(
        MAJOR_1_ >= 200 & MAJOR_1_ <=299, "Architecture and Environmental Design",
        ifelse(
          MAJOR_1_ >= 300 & MAJOR_1_ <= 399, "Area Studies",
          ifelse(
            MAJOR_1_ >= 400 & MAJOR_1_ <= 499,"Biological Sciences",
            ifelse(
              MAJOR_1_ >= 500 & MAJOR_1_ <= 599, "Business and Management",
              ifelse(
                MAJOR_1_ >= 600 & MAJOR_1_ <=  699 ,"Communications",
                ifelse(
                  MAJOR_1_ >= 700 & MAJOR_1_ <= 799, "Computer and Information Sciences",
                  ifelse(
                    MAJOR_1_ >= 800 & MAJOR_1_ <= 899, "Education",
                    ifelse(
                      MAJOR_1_ >= 900 & MAJOR_1_ <= 999, "Engineering",
                      ifelse(
                        MAJOR_1_ >= 1000 & MAJOR_1_ <= 1099, "Fine and Applied Arts",
                        ifelse(
                          MAJOR_1_ >= 1100 & MAJOR_1_ <= 1108, "Foreign Languages",
                          ifelse(
                            MAJOR_1_ >= 1200 & MAJOR_1_ <= 1299, "Health Professions",
                            ifelse(
                              MAJOR_1_ >= 1300 & MAJOR_1_ <= 1399, "Home Economics",
                              ifelse(
                                MAJOR_1_ >= 1400 & MAJOR_1_ <= 1499, "Law",
                                ifelse(
                                  MAJOR_1_ >= 1500 & MAJOR_1_ <= 1599, "Letters",
                                  ifelse(
                                    MAJOR_1_ >= 1600 & MAJOR_1_ <= 1699, "Library Science",
                                    ifelse(
                                      MAJOR_1_ >= 1700 & MAJOR_1_ <= 1799, "Mathematics",
                                      ifelse(
                                        MAJOR_1_ >= 1800 & MAJOR_1_ <= 1899, "Military Sciences",
                                        ifelse(
                                          MAJOR_1_ >= 1900 & MAJOR_1_ <= 1999 , "Physical Sciences",
                                          ifelse(
                                            MAJOR_1_ >= 2000 & MAJOR_1_ <= 2099, "Psychology",
                                            ifelse(
                                              MAJOR_1_ >= 2100 & MAJOR_1_ <= 2199 , "Public Affairs and Services",
                                              ifelse(
                                                MAJOR_1_ >= 2200 & MAJOR_1_ <= 2299, "Social Sciences",
                                                ifelse(
                                                  MAJOR_1_ >= 2300 & MAJOR_1_ <= 2399, "Theology",
                                                  ifelse(
                                                    MAJOR_1_ >= 4900 & MAJOR_1_ <= 9996, "Interdisciplinary Studies",
                                                    ifelse(
                                                      MAJOR_1_ == 0 , "NO DEGREE",
                                                      "unspecified"
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
            )
          )
        )
      )
    
    )
    ), 
    Age = as.numeric(1994-YEAR_OF_BIRTH),
    EMP_STATUS_ = ifelse(
      EMP_STATUS_ == -3 , "Unspecified", main_dataset$EMP_STATUS_
    ),
    NET_WORTH_ = ifelse(
      NET_WORTH_ <= 0 , 0 ,
      main_dataset$NET_WORTH_
    ),
    HAVING_HEALTHPLAN = ifelse(
      HAVING_HEALTHPLAN <=0 , "Unknown", 
      main_dataset$HAVING_HEALTHPLAN
    ),
    RACE = factor(ifelse(
      RACE == "NON-BLACK, NON-HISPANIC", "WHITE", 
      main_dataset$RACE
    ))
    ) %>%
  select("GENDER","YEAR_OF_BIRTH","Age","RACE","HAVING_HEALTHPLAN","REGION_", "URBAN_RURAL_", "MARSTAT_KEY_", "WKSUEMP_PCY_", "EDU_DEGREE" , "MAJOR_1_", "INCOME_", "NET_WORTH_")





#summary statistic
describe(cleaned_dataset)

#test
mytable1 <- function(DF) {
  table1 <- psych::describe(DF)
  library(dplyr)
  table1 <- table1 %>% select(count=n,mean,standard_deviation=sd,min,max,standard_error=se)
  table1$mean <- round(table1$mean,1)
  write.csv(table1,"describe.csv")
}


mytable1(cleaned_dataset)


#################################


groupby_major<- cleaned_dataset %>%
  group_by(MAJOR_1_,GENDER)%>% 
  summarize(income = mean(INCOME_),
            networth = mean(NET_WORTH_),
            count=n()) %>%
  mutate(proportion = round(count / sum(count), 3),
         income=round(income),
         networth=round(networth)) %>%
  arrange(desc(MAJOR_1_))
write.csv(groupby_major,"groupby_major.csv")




ggplot(data=groupby_major,aes(groupby_major$MAJOR_1_,y=groupby_major$income, fill=groupby_major$GENDER))+
  geom_bar(stat = "identity",position = "dodge")+
  geom_text(aes(label=groupby_major$income,vjust=-1))+
  coord_cartesian(ylim = c(10000,40000))+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Income based on Major and Genders")+
  labs(x="Major",y="INCOME(mean)")+
  labs(fill="Gender")



############################################t test
GENDER_FEMALE <- subset(groupby_major,groupby_major$GENDER=="FEMALE")
GENDER_MALE <- subset(groupby_major,groupby_major$GENDER=="MALE")

grouptest <-rbind(cbind(Gender="ALL",tidy(t.test(groupby_major$income))),
                  cbind(Gender="FEMALE",tidy(t.test(GENDER_FEMALE$income))),
                  cbind(Gender="MALE",tidy(t.test(GENDER_MALE$income)))) %>% 
  select(Gender,count=parameter,"Mean income"=estimate,method=method,alternative=alternative,statistic,p.value,conf.low,conf.high) %>%
  mutate(
    p.value = round(p.value,2),
    statistic = round(statistic,2)
  )

write.csv(grouptest,"grouptest.csv")


############################################2 sample t test
grouped_2sample <- tidy(t.test(GENDER_FEMALE$income, GENDER_MALE$income)) %>%
  select(estimate=estimate,"Mean income(Female)"=estimate1,"Mean income(Male)"=estimate2,method=method,alternative=alternative,count=parameter,statistic,p.value,conf.low,conf.high)
  
write.csv(grouped_2sample,"grouped_2sample.csv")




################################################

groupby_major$dummy_Male <- ifelse(groupby_major$GENDER=="MALE",1,0)
lm_dummy <- (lm(groupby_major$income ~ groupby_major$dummy_Male))
stargazer(lm_dummy,type="html", title="",
          dep.var.labels = "Mean income",
          convariate.lables= c("Gender"),out="Regression table on dummy.htm")





male_dataset <- subset(groupby_major, groupby_major$GENDER=="MALE")
female_dataset <- subset(groupby_major, groupby_major$GENDER=="FEMALE")

mm12<-lm(groupby_major$income ~ groupby_major$GENDER + groupby_major$MAJOR_1_)

stargazer(mm12,type="html", title="",
          dep.var.labels = c("Mean income"),
          convariate.lables= c("MAJOR","GENDER"),out="t1.htm")

unique(groupby_major$MAJOR_1_)

###############income based on marital status
martal_status<- cleaned_dataset %>%
  group_by(MARSTAT_KEY_)%>% 
  summarize(mean(INCOME_),
            mean(NET_WORTH_),
            count=n()) %>%
  mutate(proportion = round(count / sum(count), 3)) %>%
  arrange(desc(proportion))
write.csv(martal_status,"martal_status.csv")


ggplot(data=martal_status,aes(MARSTAT_KEY_,y=martal_status$`mean(INCOME_)`))+
  geom_bar(stat = "identity",fill=53)+
  geom_text(aes(label=martal_status$`mean(INCOME_)`,vjust=-1))+
  coord_cartesian(ylim = c(0,28000))+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Monthly Income on the basis of Marrage status")+
  labs(x="Marrage status",y="INCOME(mean)")+
  labs(fill="net")

colr<-c(5,386,3898,37863,87,365)
pie(martal_status$`mean(NET_WORTH_)`,
    labels=martal_status$MARSTAT_KEY_,
    col = rainbow(length(colr)))

###################################################### income based on race
income_race<- cleaned_dataset %>% 
  group_by(RACE)%>% 
  summarize(mean(INCOME_),
            mean(NET_WORTH_),
            count=n()) %>%
  mutate(proportion = round(count / sum(count), 3)) %>%
  arrange(desc(proportion))
write.csv(income_race,"income_race.csv")

ggplot(data=income_race,aes(x=income_race$RACE,y=income_race$`mean(NET_WORTH_)`))+
  geom_bar(stat = "identity",fill=51)+
  geom_text(aes(label=income_race$`mean(NET_WORTH_)`,vjust=-1))+
  coord_cartesian(ylim = c(0,153000))+
  theme(axis.text.x = element_text(angle =0))+
  labs(title = "Net worth on the basis of Race")+
  labs(x="Race",y="Net Worth(mean)")+
  labs(fill="net")

ggplot(data=income_race,aes(x=RACE,y=income_race$`mean(INCOME_)`))+
  geom_bar(stat = "identity",fill=43)+
  geom_text(aes(label=income_race$`mean(INCOME_)`,vjust=-1))+
  coord_cartesian(ylim = c(0,30000))+
  theme(axis.text.x = element_text(angle = 0))+
  labs(title = "Monthly Income on the basis of Race")+
  labs(x="Race",y="INCOME(mean)")+
  labs(fill="net")

## income based on jobs

income_edu<- cleaned_dataset %>% 
  group_by(EDU_DEGREE)%>% 
  summarize(mean(INCOME_),
            mean(NET_WORTH_),
            count=n()) %>%
  mutate(proportion = round(count / sum(count), 3)) %>%
  arrange(desc(proportion))
write.csv(income_edu,"income_edu.csv")

ggplot(data=income_edu,aes(x=income_edu$EDU_DEGREE,y=income_edu$`mean(NET_WORTH_)`))+
  geom_bar(stat = "identity",fill=51)+
  geom_text(aes(label=income_edu$`mean(NET_WORTH_)`,vjust=-1))+
  coord_cartesian(ylim = c(0,153000))+
  theme(axis.text.x = element_text(angle =0))+
  labs(title = "Net worth on the basis of Education Degree")+
  labs(x="EDU_DEGREE",y="Net Worth(mean)")+
  labs(fill="net")

ggplot(data=income_edu,aes(x=EDU_DEGREE,y=income_edu$`mean(INCOME_)`))+
  geom_bar(stat = "identity",fill=43)+
  geom_text(aes(label=income_edu$`mean(INCOME_)`,vjust=-1))+
  coord_cartesian(ylim = c(0,30000))+
  theme(axis.text.x = element_text(angle = 0))+
  labs(title = "Monthly Income on the basis of Education Degree")+
  labs(x="EDU_DEGREE",y="INCOME(mean)")+
  labs(fill="net")

