#importing libraries
packages<-(c("dplyr","psych","ggplot2","vtable","maps","fastDummies","broom","ggpubr"))
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
      MAJOR_1_ == 100|MAJOR_1_ == 101|MAJOR_1_ ==102|MAJOR_1_ ==104|MAJOR_1_ ==105|MAJOR_1_ ==106|MAJOR_1_ ==107|MAJOR_1_ ==108|MAJOR_1_ ==109|MAJOR_1_ ==110|MAJOR_1_ ==111|MAJOR_1_ ==112|MAJOR_1_ ==113|MAJOR_1_ ==114|MAJOR_1_ ==115|MAJOR_1_ ==116|MAJOR_1_ ==199, "Agriculture and Natural Resources",
      ifelse(
        MAJOR_1_ == 200|MAJOR_1_ ==201|MAJOR_1_ ==202|MAJOR_1_ ==203|MAJOR_1_ ==204|MAJOR_1_ ==299, "Architecture and Environmental Design",
        ifelse(
          MAJOR_1_ == 301|  MAJOR_1_ == 302|  MAJOR_1_ == 311|  MAJOR_1_ == 399, "Area Studies",
          ifelse(
            MAJOR_1_ == 400|  MAJOR_1_ == 401|  MAJOR_1_ == 402|  MAJOR_1_ == 405|  MAJOR_1_ == 407|  MAJOR_1_ == 409|  MAJOR_1_ == 410|  MAJOR_1_ == 411|  MAJOR_1_ == 414|  MAJOR_1_ == 415|  MAJOR_1_ == 417|  MAJOR_1_ == 418|  MAJOR_1_ == 420|  MAJOR_1_ == 422|  MAJOR_1_ == 423|  MAJOR_1_ == 424|  MAJOR_1_ == 425|  MAJOR_1_ == 427|  MAJOR_1_ == 428|  MAJOR_1_ == 429|  MAJOR_1_ == 430|  MAJOR_1_ == 499,"Biological Sciences",
            ifelse(
              MAJOR_1_ == 500|  MAJOR_1_ == 501|  MAJOR_1_ == 502|  MAJOR_1_ == 503|  MAJOR_1_ == 504|  MAJOR_1_ == 505|  MAJOR_1_ == 506|  MAJOR_1_ == 507|  MAJOR_1_ == 508|  MAJOR_1_ == 509|  MAJOR_1_ == 510|  MAJOR_1_ == 511|  MAJOR_1_ == 513|  MAJOR_1_ == 514|  MAJOR_1_ == 515|  MAJOR_1_ == 517|  MAJOR_1_ == 599, "Business and Management",
              ifelse(
                MAJOR_1_ == 600|  MAJOR_1_ == 601|  MAJOR_1_ == 602|  MAJOR_1_ == 603|  MAJOR_1_ == 604|  MAJOR_1_ == 605|  MAJOR_1_ == 606|  MAJOR_1_ == 607|  MAJOR_1_ ==  699 ,"Communications",
                ifelse(
                  MAJOR_1_ == 700|  MAJOR_1_ == 701|  MAJOR_1_ == 702|  MAJOR_1_ == 703|  MAJOR_1_ == 704|  MAJOR_1_ == 705|  MAJOR_1_ == 799, "Computer and Information Sciences",
                  ifelse(
                    MAJOR_1_ == 800|  MAJOR_1_ == 801|  MAJOR_1_ == 802|  MAJOR_1_ == 803|  MAJOR_1_ == 806|  MAJOR_1_ == 808|  MAJOR_1_ == 809|  MAJOR_1_ == 810|  MAJOR_1_ == 811|  MAJOR_1_ == 812|  MAJOR_1_ == 815|  MAJOR_1_ == 816|  MAJOR_1_ == 818|  MAJOR_1_ == 819|  MAJOR_1_ == 820|  MAJOR_1_ == 823|  MAJOR_1_ == 827|  MAJOR_1_ == 828|  MAJOR_1_ == 829|  MAJOR_1_ == 830|  MAJOR_1_ == 831|  MAJOR_1_ == 832|  MAJOR_1_ == 833|  MAJOR_1_ == 834|  MAJOR_1_ == 835|  MAJOR_1_ == 836|  MAJOR_1_ == 837|  MAJOR_1_ == 838|  MAJOR_1_ == 839|  MAJOR_1_ == 840|  MAJOR_1_ == 841|  MAJOR_1_ == 842|  MAJOR_1_ == 845|  MAJOR_1_ == 848|  MAJOR_1_ == 893|  MAJOR_1_ == 894|  MAJOR_1_ == 899, "Education",
                    ifelse(
                      MAJOR_1_ == 900|  MAJOR_1_ == 901|  MAJOR_1_ == 902|  MAJOR_1_ == 903|  MAJOR_1_ == 904|  MAJOR_1_ == 905|  MAJOR_1_ == 906|  MAJOR_1_ == 907|  MAJOR_1_ == 908|  MAJOR_1_ == 909|  MAJOR_1_ == 910|  MAJOR_1_ == 911|  MAJOR_1_ == 913|  MAJOR_1_ == 914|  MAJOR_1_ == 918|  MAJOR_1_ == 920|  MAJOR_1_ == 921|  MAJOR_1_ == 922|  MAJOR_1_ == 925|  MAJOR_1_ == 999, "Engineering",
                      ifelse(
                        MAJOR_1_ == 1000|  MAJOR_1_ == 1001|  MAJOR_1_ == 1002|  MAJOR_1_ == 1003|  MAJOR_1_ == 1004|  MAJOR_1_ == 1005|  MAJOR_1_ == 1006|  MAJOR_1_ == 1007|  MAJOR_1_ == 1008|  MAJOR_1_ == 1009|  MAJOR_1_ == 1011|  MAJOR_1_ == 1013|  MAJOR_1_ == 1014|  MAJOR_1_ == 1099, "Fine and Applied Arts",
                        ifelse(
                          MAJOR_1_ == 1100|  MAJOR_1_ == 1101|  MAJOR_1_ == 1102|  MAJOR_1_ == 1103|  MAJOR_1_ == 1105|  MAJOR_1_ == 1108, "Foreign Languages",
                          ifelse(
                            MAJOR_1_ == 1201|  MAJOR_1_ == 1202|  MAJOR_1_ == 1203|  MAJOR_1_ == 1205| MAJOR_1_ == 1207|  MAJOR_1_ == 1208|  MAJOR_1_ == 1209|  MAJOR_1_ == 1211|  MAJOR_1_ == 1212|  MAJOR_1_ == 1213|  MAJOR_1_ == 1214|  MAJOR_1_ == 1215|  MAJOR_1_ == 1219|  MAJOR_1_ == 1220|  MAJOR_1_ == 1222|  MAJOR_1_ == 1223|  MAJOR_1_ == 1224|  MAJOR_1_ == 1225|  MAJOR_1_ == 1226|  MAJOR_1_ == 1227|  MAJOR_1_ == 1228|  MAJOR_1_ == 1299, "Health Professions",
                            ifelse(
                              MAJOR_1_ == 1300|  MAJOR_1_ == 1301|  MAJOR_1_ == 1302|  MAJOR_1_ == 1303|  MAJOR_1_ == 1304|  MAJOR_1_ == 1305|  MAJOR_1_ == 1306|  MAJOR_1_ == 1307|  MAJOR_1_ == 1399, "Home Economics",
                              ifelse(
                                MAJOR_1_ == 1401|  MAJOR_1_ == 1402|  MAJOR_1_ == 1499, "Law",
                                ifelse(
                                  MAJOR_1_ == 1500|  MAJOR_1_ == 1501|  MAJOR_1_ == 1502|  MAJOR_1_ == 1505|  MAJOR_1_ == 1506|  MAJOR_1_ == 1507|  MAJOR_1_ == 1508|  MAJOR_1_ == 1509|  MAJOR_1_ == 1510|  MAJOR_1_ == 1511|  MAJOR_1_ == 1599, "Letters",
                                  ifelse(
                                    MAJOR_1_ == 1699, "Library Science",
                                    ifelse(
                                      MAJOR_1_ == 1701|  MAJOR_1_ == 1703|  MAJOR_1_ == 1799, "Mathematics",
                                      ifelse(
                                        MAJOR_1_ == 1801|  MAJOR_1_ == 1803|  MAJOR_1_ == 1899, "Military Sciences",
                                        ifelse(
                                          MAJOR_1_ == 1901|  MAJOR_1_ == 1902|  MAJOR_1_ == 1905|  MAJOR_1_ == 1911|  MAJOR_1_ == 1912|  MAJOR_1_ == 1913|  MAJOR_1_ == 1914|  MAJOR_1_ == 1916|  MAJOR_1_ == 1917|  MAJOR_1_ == 1919|  MAJOR_1_ == 1991|  MAJOR_1_ == 1992 , "Physical Sciences",
                                          ifelse(
                                            MAJOR_1_ == 2000|  MAJOR_1_ == 2001|  MAJOR_1_ == 2002|  MAJOR_1_ == 2004|  MAJOR_1_ == 2005|  MAJOR_1_ == 2008|  MAJOR_1_ == 2009|  MAJOR_1_ == 2011|  MAJOR_1_ == 2099, "Psychology",
                                            ifelse(
                                              MAJOR_1_ == 2100|  MAJOR_1_ == 2102|  MAJOR_1_ == 2103|  MAJOR_1_ == 2104|  MAJOR_1_ == 2105|  MAJOR_1_ == 2107|  MAJOR_1_ == 2199 , "Public Affairs and Services",
                                              ifelse(
                                                MAJOR_1_ == 2200|  MAJOR_1_ == 2201|  MAJOR_1_ == 2202|  MAJOR_1_ == 2204|  MAJOR_1_ == 2205|  MAJOR_1_ == 2206|  MAJOR_1_ == 2207|  MAJOR_1_ == 2208|  MAJOR_1_ == 2209|  MAJOR_1_ == 2211|  MAJOR_1_ == 2214|  MAJOR_1_ == 2299, "Social Sciences",
                                                ifelse(
                                                  MAJOR_1_ == 2300|  MAJOR_1_ == 2301|  MAJOR_1_ == 2304|  MAJOR_1_ == 2399, "Theology",
                                                  ifelse(
                                                    MAJOR_1_ == 4900|  MAJOR_1_ == 4901|  MAJOR_1_ == 4902|  MAJOR_1_ == 4903|  MAJOR_1_ == 4904|  MAJOR_1_ == 4999|  MAJOR_1_ == 9994|  MAJOR_1_ == 9995|  MAJOR_1_ == 9996, "Interdisciplinary Studies",
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



###############income based on marital status
martal_status<- cleaned_dataset %>%
  group_by(MARSTAT_KEY_,GENDER)%>% 
  summarize(mean(INCOME_),
            mean(NET_WORTH_),
            count=n()) %>%
  mutate(proportion = round(count / sum(count), 3)) %>%
           arrange(desc(MARSTAT_KEY_))
write.csv(martal_status,"martal_status.csv")

########################################################## t test

martal_status_separated <- subset(martal_status,martal_status$MARSTAT_KEY_=="SEPARATED")
martal_status_female_separated <- subset(martal_status,martal_status$GENDER=="FEMALE" & martal_status$MARSTAT_KEY_=="SEPARATED")
martal_status_male_separated <- subset(martal_status,martal_status$GENDER=="MALE"  & martal_status$MARSTAT_KEY_=="SEPARATED")

group_test_separated<- rbind(cbind(Group="All",Maritial_status="Separated",tidy(t.test(martal_status_separated$`mean(NET_WORTH_)`))),
                             cbind(Group="Female",Maritial_status="Separated",tidy(t.test(martal_status_female_separated$`mean(NET_WORTH_)`))),
                             cbind(Group="Male",Maritial_status="Separated",tidy(t.test(martal_status_male_separated$`mean(NET_WORTH_)`)))
                             )


martal_status_MARRIED <- subset(martal_status,martal_status$MARSTAT_KEY_=="MARRIED")
martal_status_female_MARRIED <- subset(martal_status,martal_status$GENDER=="FEMALE" & martal_status$MARSTAT_KEY_=="MARRIED")
martal_status_male_MARRIED <- subset(martal_status,martal_status$GENDER=="MALE"  & martal_status$MARSTAT_KEY_=="MARRIED")

group_test_married<- rbind(cbind(Group="All",Maritial_status="MARRIED",tidy(t.test(martal_status_MARRIED$`mean(NET_WORTH_)`))),
                   cbind(Group="Female",Maritial_status="MARRIED",tidy(t.test(martal_status_female_MARRIED$`mean(NET_WORTH_)`))),
                   cbind(Group="Male",Maritial_status="MARRIED",tidy(t.test(martal_status_male_MARRIED$`mean(NET_WORTH_)`)))
                   )


martal_status_WIDOWED <- subset(martal_status,martal_status$MARSTAT_KEY_=="WIDOWED")
martal_status_female_WIDOWED <- subset(martal_status,martal_status$GENDER=="FEMALE" & martal_status$MARSTAT_KEY_=="WIDOWED")
martal_status_male_WIDOWED <- subset(martal_status,martal_status$GENDER=="MALE"  & martal_status$MARSTAT_KEY_=="WIDOWED")

group_test_WIDOWED<- rbind(cbind(Group="All",Maritial_status="WIDOWED",tidy(t.test(martal_status_WIDOWED$`mean(NET_WORTH_)`))),
                           cbind(Group="Female",Maritial_status="WIDOWED",tidy(t.test(martal_status_female_WIDOWED$`mean(NET_WORTH_)`))),
                           cbind(Group="Male",Maritial_status="WIDOWED",tidy(t.test(martal_status_male_WIDOWED$`mean(NET_WORTH_)`)))
                           )



martal_status_DIVORCED <- subset(martal_status,martal_status$MARSTAT_KEY_=="DIVORCED")
martal_status_female_DIVORCED <- subset(martal_status,martal_status$GENDER=="FEMALE" & martal_status$MARSTAT_KEY_=="DIVORCED")
martal_status_male_DIVORCED <- subset(martal_status,martal_status$GENDER=="MALE"  & martal_status$MARSTAT_KEY_=="DIVORCED")

group_test_DIVORCED<- rbind(cbind(Group="All",Maritial_status="DIVORCED",tidy(t.test(martal_status_DIVORCED$`mean(NET_WORTH_)`))),
                           cbind(Group="Female",Maritial_status="DIVORCED",tidy(t.test(martal_status_female_DIVORCED$`mean(NET_WORTH_)`))),
                           cbind(Group="Male",Maritial_status="DIVORCED",tidy(t.test(martal_status_male_DIVORCED$`mean(NET_WORTH_)`)))
                           )


martal_status_NEVER_MARRIED <- subset(martal_status,martal_status$MARSTAT_KEY_=="NEVER MARRIED")
martal_status_female_NEVER_MARRIED <- subset(martal_status,martal_status$GENDER=="FEMALE" & martal_status$MARSTAT_KEY_=="NEVER MARRIED")
martal_status_male_NEVER_MARRIED <- subset(martal_status,martal_status$GENDER=="MALE"  & martal_status$MARSTAT_KEY_=="NEVER MARRIED")

group_test_NEVER_MARRIED<- rbind(cbind(Group="All",Maritial_status="NEVER MARRIED",tidy(t.test(martal_status_NEVER_MARRIED$`mean(NET_WORTH_)`))),
                           cbind(Group="Female",Maritial_status="NEVER MARRIED",tidy(t.test(martal_status_female_NEVER_MARRIED$`mean(NET_WORTH_)`))),
                           cbind(Group="Male",Maritial_status="NEVER MARRIED",tidy(t.test(martal_status_male_NEVER_MARRIED$`mean(NET_WORTH_)`)))
)


group_test <- rbind(group_test_separated,
                    group_test_married,
                    group_test_WIDOWED,
                    group_test_DIVORCED,
                    group_test_NEVER_MARRIED
)

group_t_test_table <- group_test %>% select(Maritial_status, Group, method=method,alternative=alternative,count=parameter,mean(networth)=round(estimate),statistic=mean(statistic),p_value=mean(p.value),conf.low=mean(conf.low),conf.high=mean(conf.high))
write.csv(group_t_test_table,"t_test_based_on_marital_status.csv")

############################################2 sample t test
grouped_2sample <- rbind(cbind(Maritial_status="Separated",tidy(t.test(martal_status_female_separated$`mean(NET_WORTH_)`, martal_status_male_separated$`mean(NET_WORTH_)`))),
                           cbind(Maritial_status="married",tidy(t.test(martal_status_female_MARRIED$`mean(NET_WORTH_)`, martal_status_male_MARRIED$`mean(NET_WORTH_)`))),
                           cbind(Maritial_status="widowed",tidy(t.test(martal_status_female_WIDOWED$`mean(NET_WORTH_)`, martal_status_male_WIDOWED$`mean(NET_WORTH_)`))),
                           cbind(Maritial_status="divorced",tidy(t.test(martal_status_female_DIVORCED$`mean(NET_WORTH_)`, martal_status_male_DIVORCED$`mean(NET_WORTH_)`))),
                           cbind(Maritial_status="never married",tidy(t.test(martal_status_female_NEVER_MARRIED$`mean(NET_WORTH_)`, martal_status_male_NEVER_MARRIED$`mean(NET_WORTH_)`)))
                           )

grouped_2sample <- grouped_2sample %>% select(Maritial_status,estimate=estimate,"Mean net worth(Female)"=estimate1,"Mean net worth(Male)"=estimate2,method=method,alternative=alternative,count=parameter,statistic,p.value,conf.low,conf.high)
write.csv(grouped_2sample,"grouped_2sample.csv")






###########################################



ggplot(data=martal_status,aes(x=martal_status$GENDER,y=round(martal_status$`mean(NET_WORTH_)`),fill=martal_status$MARSTAT_KEY_))+
  geom_bar(position="dodge", stat="identity")+
  coord_cartesian(ylim = c(0,153000))+
  theme(axis.text.x = element_text(angle =0))+
  labs(x="Gender",y="Net Worth(mean)")+
  labs(fill="Marital Status")+
  coord_flip()

ggplot(data=martal_status,aes(x=martal_status$GENDER,y=round(martal_status$`mean(INCOME_)`),fill=martal_status$MARSTAT_KEY_))+
  geom_bar(position="dodge", stat="identity")+
  coord_cartesian(ylim = c(0,153000))+
  theme(axis.text.x = element_text(angle =0))+
  labs(x="Gender",y="Net Worth(mean)")+
  labs(fill="Marital Status")+
  coord_flip()


