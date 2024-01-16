if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools, dplyr, tidyverse, tidyr, stringr,  curl)



#rm(list=setdiff(ls(), c("")))


options(scipen=999)


#setwd("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/Children's Care Homes Project")

####ASC-FR - activity####

####Step 1: identify outsourced activity for home care, nursing care; residential care
#### most recent files only have it super disaggregated
#### older files have: home care hours by LA/indy; residential care by LA/ indy; 
####new files have no funding type break down; older files do ffs

####Ugh - no activity for community home care in by provision type in recent data


#As I see it first variables to construct are: activity in house % for: home care, nursing care; residential care 2005-2022

#LFG!!!


####ACH I@VE DONE THESE ALL WRONG####
#I didn't have 18-64 resies - which will be broken down by need
#####BUT I THINK I WANT OLD PEOPLE ONLY PLZ####

asc23 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/ASC-FR%20Data%20File%20(descriptions)%20v2_2023.csv"))
asc22 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/ASC-FR%20Data%20File%20(descriptions)%20v2_2022.csv"))
asc21 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/ASC-FR%20Data%20File%20(descriptions)%20v2_2021.csv"))
asc20 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/ASC-FR%20Data%20File%20(descriptions)%20v2_2020.csv"))
asc19 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/ASC-FR%20Data%20File%20(descriptions)%20v2_2019.csv"))
asc18 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/Copy%20of%20ASCFR%20Data%20File%20(with%20descriptions)_2018.csv"))


asc17 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2017/CSV%20ASCFR%20Activity.csv"))
asc16 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2016/pss-exp-eng-15-16-fin-act.csv"))
asc15 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2015/pss-exp-eng-14-15-fin-coun-lev-act-data.csv"), 
                  skip=1,colClasses = "character")



asc14 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2014/Annex_Final_CASSR_Level_Activity_Data_2013_14.csv"),
                  skip = 3, colClasses = "character")
asc13 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2013/Annex_Fin_CASSR_Level_Activity_Data_2012_13.csv"),
                  skip = 3, colClasses = "character")
asc12 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2012/Final_Council_Level_Activity_Data_2011_12.csv"),
                  skip = 3, colClasses = "character")
asc11 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2011/Final_Council_Level_Activity_Data_2010-11.csv"),
                  skip = 4, colClasses = "character")
asc10 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2010/Final_Council%20Level_ActivityData_2009-10.csv"),
                  skip = 3, colClasses = "character")
asc09 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2009/pers-soc-serv-exp-unit-cost-eng-2008-09-data.csv"),
                  skip = 4, colClasses = "character")



asc08 <- read.csv(curl())
asc07 <- read.csv(curl())
asc06 <- read.csv(curl())
asc05 <- read.csv(curl())


asc23 <- asc23 %>% dplyr::filter(GEOGRAPHY_LEVEL=="Local Authority",
                                AgeBand=="65 and Over",
                                 DimensionGroup=="Activity")%>%
  dplyr::mutate(ITEMVALUE = as.numeric(ITEMVALUE))%>%
  dplyr::select(GEOGRAPHY_CODE, DH_GEOGRAPHY_NAME,ActivityProvision, SupportSetting ,ITEMVALUE)%>%
  dplyr::group_by(GEOGRAPHY_CODE, DH_GEOGRAPHY_NAME,ActivityProvision, SupportSetting )%>%
  dplyr::summarise(ITEMVALUE = sum(ITEMVALUE, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, GEOGRAPHY_CODE, SupportSetting) %>%
  dplyr::mutate(percent_sector = ITEMVALUE /ITEMVALUE[ActivityProvision == "99"]*100) %>%
  dplyr::ungroup()
  
asc22 <- asc22 %>% dplyr::filter(GEOGRAPHY_LEVEL=="Local Authority",
                                 AgeBand=="65 and Over",
                                 DimensionGroup=="Activity")%>%
  dplyr::mutate(ITEMVALUE = as.numeric(ITEMVALUE))%>%
  dplyr::select(GEOGRAPHY_CODE, DH_GEOGRAPHY_NAME,ActivityProvision, SupportSetting ,ITEMVALUE)%>%
  dplyr::group_by(GEOGRAPHY_CODE, DH_GEOGRAPHY_NAME,ActivityProvision, SupportSetting )%>%
  dplyr::summarise(ITEMVALUE = sum(ITEMVALUE, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, GEOGRAPHY_CODE, SupportSetting) %>%
  dplyr::mutate(percent_sector = ITEMVALUE /ITEMVALUE[ActivityProvision == "99"]*100) %>%
  dplyr::ungroup()

asc21 <- asc21 %>% dplyr::filter(GEOGRAPHY_LEVEL=="Local Authority",
                                 AgeBand=="65 and Over",
                                 DimensionGroup=="Activity")%>%
  dplyr::mutate(ITEMVALUE = as.numeric(ITEMVALUE))%>%
  dplyr::select(GEOGRAPHY_CODE, DH_GEOGRAPHY_NAME,ActivityProvision, SupportSetting ,ITEMVALUE)%>%
  dplyr::group_by(GEOGRAPHY_CODE, DH_GEOGRAPHY_NAME,ActivityProvision, SupportSetting )%>%
  dplyr::summarise(ITEMVALUE = sum(ITEMVALUE, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, GEOGRAPHY_CODE, SupportSetting) %>%
  dplyr::mutate(percent_sector = ITEMVALUE /ITEMVALUE[ActivityProvision == "99"]*100) %>%
  dplyr::ungroup()

asc20 <- asc20 %>% dplyr::filter(GEOGRAPHY_LEVEL=="Local Authority",
                                 AgeBand=="65 and Over",
                                 DimensionGroup=="Activity")%>%
  dplyr::mutate(ITEMVALUE = as.numeric(ITEMVALUE))%>%
  dplyr::select(GEOGRAPHY_CODE, DH_GEOGRAPHY_NAME,ActivityProvision, SupportSetting ,ITEMVALUE)%>%
  dplyr::group_by(GEOGRAPHY_CODE, DH_GEOGRAPHY_NAME,ActivityProvision, SupportSetting )%>%
  dplyr::summarise(ITEMVALUE = sum(ITEMVALUE, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, GEOGRAPHY_CODE, SupportSetting) %>%
  dplyr::mutate(percent_sector = ITEMVALUE /ITEMVALUE[ActivityProvision == "99"]*100) %>%
  dplyr::ungroup()

asc19 <- asc19 %>% dplyr::filter(GEOGRAPHY_LEVEL=="Local Authority",
                                 AgeBand_Key=="65 and Over",
                                 DimensionGroup=="Activity")%>%
  dplyr::mutate(ITEMVALUE = as.numeric(ITEMVALUE))%>%
  dplyr::select(GEOGRAPHY_CODE, DH_GEOGRAPHY_NAME,ActivityProvision_Key, SupportSetting_Key ,ITEMVALUE)%>%
  dplyr::group_by(GEOGRAPHY_CODE, DH_GEOGRAPHY_NAME,ActivityProvision_Key, SupportSetting_Key )%>%
  dplyr::summarise(ITEMVALUE = sum(ITEMVALUE, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, GEOGRAPHY_CODE, SupportSetting_Key) %>%
  dplyr::mutate(percent_sector = ITEMVALUE /ITEMVALUE[ActivityProvision_Key == "99"]*100) %>%
  dplyr::ungroup()

asc18 <- asc18 %>% dplyr::filter(GEOGRAPHY_LEVEL=="Local Authority",
                                 AgeBand_Key=="65 and Over",
                                 DimensionGroup=="Activity")%>%
  dplyr::mutate(ITEMVALUE = as.numeric(ITEMVALUE))%>%
  dplyr::select(GEOGRAPHY_CODE, DH_GEOGRAPHY_NAME,ActivityProvision_Key, SupportSetting_Key ,ITEMVALUE)%>%
  dplyr::group_by(GEOGRAPHY_CODE, DH_GEOGRAPHY_NAME,ActivityProvision_Key, SupportSetting_Key )%>%
  dplyr::summarise(ITEMVALUE = sum(ITEMVALUE, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, GEOGRAPHY_CODE, SupportSetting_Key) %>%
  dplyr::mutate(percent_sector = ITEMVALUE /ITEMVALUE[ActivityProvision_Key == "99"]*100) %>%
  dplyr::ungroup()

total17 <- asc17 %>% dplyr::filter(GEOGRAPHY_CODE!="", 
                                   AgeBand_KEY==2)%>%
  dplyr::mutate(ItemValue=as.numeric(ItemValue))%>%
  dplyr::select(GEOGRAPHY_CODE, SupportSetting_KEY, ItemValue)%>%
  dplyr::group_by(GEOGRAPHY_CODE, SupportSetting_KEY) %>%
  dplyr::summarise(ItemValue = sum(ItemValue, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(ActivityProvision_KEY=1)


 asc17 <- asc17 %>% dplyr::filter(AgeBand_KEY==2, 
                                  GEOGRAPHY_CODE!="")%>%
   dplyr::mutate( ItemValue=as.numeric(ItemValue))%>%
   dplyr::select(GEOGRAPHY_CODE, SupportSetting_KEY, ActivityProvision_KEY, ItemValue)%>%
   dplyr::bind_rows(., total17)%>%
   dplyr::mutate(ActivityProvision_KEY = ifelse(ActivityProvision_KEY==1,"99",
                                                         ifelse(ActivityProvision_KEY==2,"External",
                                                                ifelse(ActivityProvision_KEY==3,"In House",NA))),
                          SupportSetting_KEY = ifelse(SupportSetting_KEY==6, "Nursing",
                                                      ifelse(SupportSetting_KEY == 7,"Residential",
                                                             ifelse(is.na(SupportSetting_KEY), "", NA))))%>%
  dplyr::rename(SupportSetting=SupportSetting_KEY,
                ActivityProvision=ActivityProvision_KEY,
                ITEMVALUE = ItemValue)%>%
  dplyr::group_by(GEOGRAPHY_CODE, ActivityProvision, SupportSetting) %>%
  dplyr::summarise(ITEMVALUE = sum(ITEMVALUE, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::group_by(GEOGRAPHY_CODE, SupportSetting) %>%
   dplyr::mutate(percent_sector = ITEMVALUE /ITEMVALUE[ActivityProvision == "99"]*100) %>%
   dplyr::ungroup()%>%
  dplyr::mutate(year=2017,
                DH_GEOGRAPHY_NAME=NA)%>%
   dplyr::mutate(SupportSetting = ifelse(is.na(SupportSetting), "", SupportSetting))
  
 

 total16 <- asc16 %>% dplyr::filter(Council.Code!="",
                                    Age.Bands=="Age 65 and over")%>%
   dplyr::mutate(Value=as.numeric(Value))%>%
   dplyr::select(Council.Code, Council.Name, Activity.Provision, Long.Term.Support.Setting, Value)%>%
   dplyr::group_by(Council.Code, Council.Name, Long.Term.Support.Setting) %>%
   dplyr::summarise(Value = sum(Value, na.rm  =T))%>%
   dplyr::ungroup()%>%
   dplyr::mutate(Activity.Provision="99")
 
 
 
 asc16 <- asc16 %>% dplyr::filter(Council.Code!="",
                                  Age.Bands=="Age 65 and over")%>%
   dplyr::mutate( Value=as.numeric(Value))%>%
   dplyr::select(Council.Code, Council.Name, Long.Term.Support.Setting, Activity.Provision, Value)%>%
   dplyr::bind_rows(., total16)%>%
   dplyr::rename(SupportSetting=Long.Term.Support.Setting,
                 ActivityProvision=Activity.Provision,
                 ITEMVALUE = Value,
                 GEOGRAPHY_CODE = Council.Code,
                 DH_GEOGRAPHY_NAME = Council.Name)%>%
   dplyr::group_by(GEOGRAPHY_CODE,DH_GEOGRAPHY_NAME, ActivityProvision, SupportSetting) %>%
   dplyr::summarise(ITEMVALUE = sum(ITEMVALUE, na.rm=T))%>%
   dplyr::ungroup()%>%
   dplyr::group_by(GEOGRAPHY_CODE, DH_GEOGRAPHY_NAME,SupportSetting) %>%
   dplyr::mutate(percent_sector = ITEMVALUE /ITEMVALUE[ActivityProvision == "99"]*100,
                 SupportSetting = ifelse(SupportSetting=="-", "", SupportSetting)) %>%
   dplyr::ungroup()%>%
   dplyr::mutate(year=2016)%>%
   dplyr::mutate(SupportSetting = ifelse(is.na(SupportSetting), "", SupportSetting))
 
asc15 <- asc15 %>%
  dplyr::filter(Age.category=="Age 65 and over")%>%
  pivot_longer( cols=!c(Support.type, Age.category, Primary.support.reason, Service.delivery.mechanism, Provision), 
               names_to = "DH_GEOGRAPHY_NAME", values_to = "ITEMVALUE", names_prefix = ".*\\.{3}")%>%
  dplyr::mutate( ITEMVALUE=as.numeric(ITEMVALUE))%>%
  dplyr::select(DH_GEOGRAPHY_NAME, Service.delivery.mechanism, Provision, ITEMVALUE)%>%
  dplyr::rename(SupportSetting=Service.delivery.mechanism,
                ActivityProvision=Provision)%>%
  dplyr::filter(SupportSetting=="Nursing"|SupportSetting=="Residential")%>%
  dplyr::mutate(ActivityProvision = ifelse(ActivityProvision=="Total provision", "99",
                                           ifelse(ActivityProvision=="In house provision", "In House",
                                                  ifelse(ActivityProvision=="External provision", "External", ActivityProvision))))%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, ActivityProvision, SupportSetting) %>%
  dplyr::summarise(ITEMVALUE = sum(ITEMVALUE, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
  dplyr::mutate(percent_sector = ITEMVALUE /ITEMVALUE[ActivityProvision == "99"]*100,
                SupportSetting = ifelse(SupportSetting=="-", "", SupportSetting)) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(year=2015)%>%
  dplyr::mutate(SupportSetting = ifelse(is.na(SupportSetting), "", SupportSetting))
  

asc14 <- asc14 %>%
  dplyr::select(X,Residents.aged.65.and.over.in.own.provision.residential.placements,
                Residents.aged.65.and.over.in.residential.placements.provided.by.others,
                Provided.by.the.council,
                Provided.by.the.independent.sector,
                Clients.aged.18.64.with.a.Learning.disability..own.provision,
                Clients.aged.18.64.with.a.Learning.disability..provision.by.others,
                Clients.aged.18.64.with.a.Physical.disability..own.provision,
                Clients.aged.18.64.with.a.Physical.disability..provision.by.others,
                Clients.aged.18.64.with.Mental.health.needs..own.provision,
                Clients.aged.18.64.with.Mental.health.needs..provision.by.others,
                Clients.aged.65.and.over..own.provision,
                Clients.aged.65.and.over..provision.by.others)%>%
  dplyr::filter(X!="",
                X!="TOTAL England",
                Residents.aged.65.and.over.in.own.provision.residential.placements!="")%>%
tidyr::pivot_longer(cols=!c(X), names_to = "ActivityProvision", values_to = "ITEMVALUE")%>%
  dplyr::mutate(SupportSetting = ifelse(stringr::str_starts(ActivityProvision, "Client"), "Day care",
                                        ifelse(ActivityProvision=="Provided.by.the.council"|ActivityProvision=="Provided.by.the.independent.sector", "Home care", "Residential")),
                ActivityProvision = ifelse(stringr::str_ends(ActivityProvision, "others"), "External",
                                           ifelse(stringr::str_ends(ActivityProvision, "provision"), "In House",
                                                  ifelse(ActivityProvision=="Provided.by.the.independent.sector", "External", "In House"))),
                ITEMVALUE= as.numeric(gsub( ",", "", ITEMVALUE)),
                X = gsub("^\\d+\\s+-\\s+", "", X))%>%
  dplyr::rename(DH_GEOGRAPHY_NAME = X)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, ActivityProvision, SupportSetting) %>%
  dplyr::summarise(ITEMVALUE = sum(ITEMVALUE, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
  dplyr::mutate(percent_sector = ITEMVALUE /(ITEMVALUE[ActivityProvision == "External"]+ITEMVALUE[ActivityProvision == "In House"])*100,
                SupportSetting = ifelse(SupportSetting=="-", "", SupportSetting)) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(year=2014)%>%
  dplyr::mutate(SupportSetting = ifelse(is.na(SupportSetting), "", SupportSetting))  




asc13 <- asc13 %>%
  dplyr::select(X,Residents.aged.65.and.over.in.own.provision.residential.placements,
                Residents.aged.65.and.over.in.residential.placements.provided.by.others,
                Provided.by.the.council,
                Provided.by.the.independent.sector,
                Clients.aged.18.64.with.a.Learning.disability..own.provision,
                Clients.aged.18.64.with.a.Learning.disability..provision.by.others,
                Clients.aged.18.64.with.a.Physical.disability..own.provision,
                Clients.aged.18.64.with.a.Physical.disability..provision.by.others,
                Clients.aged.18.64.with.Mental.health.needs..own.provision,
                Clients.aged.18.64.with.Mental.health.needs..provision.by.others,
                Clients.aged.65.and.over..own.provision,
                Clients.aged.65.and.over..provision.by.others)%>%
  dplyr::filter(X!="",
                X!="TOTAL England",
                Residents.aged.65.and.over.in.own.provision.residential.placements!="")%>%
  tidyr::pivot_longer(cols=!c(X), names_to = "ActivityProvision", values_to = "ITEMVALUE")%>%
  dplyr::mutate(SupportSetting = ifelse(stringr::str_starts(ActivityProvision, "Client"), "Day care",
                                        ifelse(ActivityProvision=="Provided.by.the.council"|ActivityProvision=="Provided.by.the.independent.sector", "Home care", "Residential")),
                ActivityProvision = ifelse(stringr::str_ends(ActivityProvision, "others"), "External",
                                           ifelse(stringr::str_ends(ActivityProvision, "provision"), "In House",
                                                  ifelse(ActivityProvision=="Provided.by.the.independent.sector", "External", "In House"))),
                ITEMVALUE= as.numeric(gsub( ",", "", ITEMVALUE)),
                X = gsub("^\\d+\\s+-\\s+", "", X))%>%
  dplyr::rename(DH_GEOGRAPHY_NAME = X)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, ActivityProvision, SupportSetting) %>%
  dplyr::summarise(ITEMVALUE = sum(ITEMVALUE, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
  dplyr::mutate(percent_sector = ITEMVALUE /(ITEMVALUE[ActivityProvision == "External"]+ITEMVALUE[ActivityProvision == "In House"])*100,
                SupportSetting = ifelse(SupportSetting=="-", "", SupportSetting)) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(year=2013)%>%
  dplyr::mutate(SupportSetting = ifelse(is.na(SupportSetting), "", SupportSetting))  



asc12 <- asc12 %>%
  dplyr::select(X,Residents.aged.65.and.over.in.own.provision.residential.placements,
                Residents.aged.65.and.over.in.residential.placements.provided.by.others,
                Provided.by.the.council,
                Provided.by.the.independent.sector,
                Clients.aged.18.64.with.a.Learning.disability..own.provision,
                Clients.aged.18.64.with.a.Learning.disability..provision.by.others,
                Clients.aged.18.64.with.a.Physical.disability..own.provision,
                Clients.aged.18.64.with.a.Physical.disability..provision.by.others,
                Clients.aged.18.64.with.Mental.health.needs..own.provision,
                Clients.aged.18.64.with.Mental.health.needs..provision.by.others,
                Clients.aged.65.and.over..own.provision,
                Clients.aged.65.and.over..provision.by.others)%>%
  dplyr::filter(X!="",
                X!="TOTAL England",
                Residents.aged.65.and.over.in.own.provision.residential.placements!="")%>%
  tidyr::pivot_longer(cols=!c(X), names_to = "ActivityProvision", values_to = "ITEMVALUE")%>%
  dplyr::mutate(SupportSetting = ifelse(stringr::str_starts(ActivityProvision, "Client"), "Day care",
                                        ifelse(ActivityProvision=="Provided.by.the.council"|ActivityProvision=="Provided.by.the.independent.sector", "Home care", "Residential")),
                ActivityProvision = ifelse(stringr::str_ends(ActivityProvision, "others"), "External",
                                           ifelse(stringr::str_ends(ActivityProvision, "provision"), "In House",
                                                  ifelse(ActivityProvision=="Provided.by.the.independent.sector", "External", "In House"))),
                ITEMVALUE= as.numeric(gsub( ",", "", ITEMVALUE)),
                X = gsub("^\\d+\\s+-\\s+", "", X))%>%
  dplyr::rename(DH_GEOGRAPHY_NAME = X)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, ActivityProvision, SupportSetting) %>%
  dplyr::summarise(ITEMVALUE = sum(ITEMVALUE, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
  dplyr::mutate(percent_sector = ITEMVALUE /(ITEMVALUE[ActivityProvision == "External"]+ITEMVALUE[ActivityProvision == "In House"])*100,
                SupportSetting = ifelse(SupportSetting=="-", "", SupportSetting)) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(year=2012)%>%
  dplyr::mutate(SupportSetting = ifelse(is.na(SupportSetting), "", SupportSetting))  


asc11 <- asc11 %>%
  dplyr::select(X,Residents.aged.65.and.over.in.own.provision.residential.placements,
                Residents.aged.65.and.over.in.residential.placements.provided.by.others,
                Provided.by.the.council,
                Provided.by.the.independent.sector,
                Clients.aged.18.64.with.a.Learning.disability..own.provision,
                Clients.aged.18.64.with.a.Learning.disability..provision.by.others,
                Clients.aged.18.64.with.a.Physical.disability..own.provision,
                Clients.aged.18.64.with.a.Physical.disability..provision.by.others,
                Clients.aged.18.64.with.Mental.health.needs..own.provision,
                Clients.aged.18.64.with.Mental.health.needs..provision.by.others,
                Clients.aged.65.and.over..own.provision,
                Clients.aged.65.and.over..provision.by.others)%>%
  dplyr::filter(X!="",
                X!="TOTAL England",
                Residents.aged.65.and.over.in.own.provision.residential.placements!="")%>%
  tidyr::pivot_longer(cols=!c(X), names_to = "ActivityProvision", values_to = "ITEMVALUE")%>%
  dplyr::mutate(SupportSetting = ifelse(stringr::str_starts(ActivityProvision, "Client"), "Day care",
                                        ifelse(ActivityProvision=="Provided.by.the.council"|ActivityProvision=="Provided.by.the.independent.sector", "Home care", "Residential")),
                ActivityProvision = ifelse(stringr::str_ends(ActivityProvision, "others"), "External",
                                           ifelse(stringr::str_ends(ActivityProvision, "provision"), "In House",
                                                  ifelse(ActivityProvision=="Provided.by.the.independent.sector", "External", "In House"))),
                ITEMVALUE= as.numeric(gsub( ",", "", ITEMVALUE)),
                X = gsub("^\\d+\\s+-\\s+", "", X))%>%
  dplyr::rename(DH_GEOGRAPHY_NAME = X)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, ActivityProvision, SupportSetting) %>%
  dplyr::summarise(ITEMVALUE = sum(ITEMVALUE, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
  dplyr::mutate(percent_sector = ITEMVALUE /(ITEMVALUE[ActivityProvision == "External"]+ITEMVALUE[ActivityProvision == "In House"])*100,
                SupportSetting = ifelse(SupportSetting=="-", "", SupportSetting)) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(year=2011)%>%
  dplyr::mutate(SupportSetting = ifelse(is.na(SupportSetting), "", SupportSetting))  


asc10 <- asc10 %>%
  dplyr::select(X,Residents.aged.65.and.over.in.own.provision.residential.placements,
                Residents.aged.65.and.over.in.residential.placements.provided.by.others,
                Provided.by.the.council,
                Provided.by.the.independent.sector,
                Clients.aged.18.64.with.a.Learning.disability..own.provision,
                Clients.aged.18.64.with.a.Learning.disability..provision.by.others,
                Clients.aged.18.64.with.a.Physical.disability..own.provision,
                Clients.aged.18.64.with.a.Physical.disability..provision.by.others,
                Clients.aged.18.64.with.Mental.health.needs..own.provision,
                Clients.aged.18.64.with.Mental.health.needs..provision.by.others,
                Clients.aged.65.and.over..own.provision,
                Clients.aged.65.and.over..provision.by.others)%>%
  dplyr::filter(X!="",
                X!="TOTAL England",
                Residents.aged.65.and.over.in.own.provision.residential.placements!="")%>%
  tidyr::pivot_longer(cols=!c(X), names_to = "ActivityProvision", values_to = "ITEMVALUE")%>%
  dplyr::mutate(SupportSetting = ifelse(stringr::str_starts(ActivityProvision, "Client"), "Day care",
                                        ifelse(ActivityProvision=="Provided.by.the.council"|ActivityProvision=="Provided.by.the.independent.sector", "Home care", "Residential")),
                ActivityProvision = ifelse(stringr::str_ends(ActivityProvision, "others"), "External",
                                           ifelse(stringr::str_ends(ActivityProvision, "provision"), "In House",
                                                  ifelse(ActivityProvision=="Provided.by.the.independent.sector", "External", "In House"))),
                ITEMVALUE= as.numeric(gsub( ",", "", ITEMVALUE)),
                X = gsub("^\\d+\\s+-\\s+", "", X))%>%
  dplyr::rename(DH_GEOGRAPHY_NAME = X)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, ActivityProvision, SupportSetting) %>%
  dplyr::summarise(ITEMVALUE = sum(ITEMVALUE, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
  dplyr::mutate(percent_sector = ITEMVALUE /(ITEMVALUE[ActivityProvision == "External"]+ITEMVALUE[ActivityProvision == "In House"])*100,
                SupportSetting = ifelse(SupportSetting=="-", "", SupportSetting)) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(year=2010)%>%
  dplyr::mutate(SupportSetting = ifelse(is.na(SupportSetting), "", SupportSetting))  

asc09 <- asc09 %>%
  dplyr::select(X.1,residents.aged.65.and.over.in.own.provision.residential.placements,
                residents.aged.65.and.over.in.residential.placements.provided.by.others,
                X.54, X.55,
                clients.aged.65.and.over..own.provision,
                clients.aged.65.and.over..provision.by.others)%>%
  dplyr::rename(X=X.1,
                Residents.aged.65.and.over.in.own.provision.residential.placements=residents.aged.65.and.over.in.own.provision.residential.placements,
                Residents.aged.65.and.over.in.residential.placements.provided.by.others=residents.aged.65.and.over.in.residential.placements.provided.by.others,
                Clients.aged.65.and.over..own.provision=clients.aged.65.and.over..own.provision,
                Clients.aged.65.and.over..provision.by.others=clients.aged.65.and.over..provision.by.others,
                Provided.by.the.council=X.54,
                Provided.by.the.independent.sector=X.55)%>%
  dplyr::filter(X!="",
                X!="TOTAL England",
                Residents.aged.65.and.over.in.own.provision.residential.placements!="")%>%
  tidyr::pivot_longer(cols=!c(X), names_to = "ActivityProvision", values_to = "ITEMVALUE")%>%
  dplyr::mutate(SupportSetting = ifelse(stringr::str_starts(ActivityProvision, "Client"), "Day care",
                                        ifelse(ActivityProvision=="Provided.by.the.council"|ActivityProvision=="Provided.by.the.independent.sector", "Home care", "Residential")),
                ActivityProvision = ifelse(stringr::str_ends(ActivityProvision, "others"), "External",
                                           ifelse(stringr::str_ends(ActivityProvision, "provision"), "In House",
                                                  ifelse(ActivityProvision=="Provided.by.the.independent.sector", "External", "In House"))),
                ITEMVALUE= as.numeric(gsub( ",", "", ITEMVALUE)),
                X = gsub("^\\d+\\s+-\\s+", "", X))%>%
  dplyr::rename(DH_GEOGRAPHY_NAME = X)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, ActivityProvision, SupportSetting) %>%
  dplyr::summarise(ITEMVALUE = sum(ITEMVALUE, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
  dplyr::mutate(percent_sector = ITEMVALUE /(ITEMVALUE[ActivityProvision == "External"]+ITEMVALUE[ActivityProvision == "In House"])*100,
                SupportSetting = ifelse(SupportSetting=="-", "", SupportSetting)) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(year=2009)%>%
  dplyr::mutate(SupportSetting = ifelse(is.na(SupportSetting), "", SupportSetting))  



library(readxl)
library(dplyr)

# excel_path <- "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2008\\DetailedActivityDataByCouncil2007-08 supressed (values only).xls"
# 
# excel_sheets <- excel_sheets(excel_path)
# 
# for (sheet_name in excel_sheets) {
#   df <- read_excel(excel_path, sheet = sheet_name, col_types = NULL)
#   
#   csv_path <- paste0("C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2008\\council_csvs\\output_", sheet_name, ".csv")
#   
#   write.csv(df, file = csv_path, row.names = FALSE)
# }
# 
# write.csv(excel_sheets, file = "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2008\\council_csvs\\sheet_names.csv", row.names = FALSE)
# 

sheet_names <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2008/council_csvs/sheet_names.csv"))

sheet_names$x <- gsub(" ", "%20", sheet_names$x)

all_df <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2008/council_csvs/output_106-Gateshead.csv"), skip=1)%>%
  dplyr::select(dplyr::starts_with("Data"))%>%
  dplyr::mutate(DH_GEOGRAPHY_NAME=sheet_names$x[1])


for (i in sheet_names$x) {
  df <- read.csv(curl(paste0("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2008/council_csvs/output_", i, ".csv")), skip=1)%>%
    dplyr::select(dplyr::starts_with("Data"))%>%
    dplyr::mutate(DH_GEOGRAPHY_NAME=i)
  
  all_df <- rbind(all_df, df)
  print(i)
}


asc08 <- all_df %>% 
  dplyr::filter(Data.item.description=="residents aged 65 and over in own provision residential placements"|
                                    Data.item.description=="residents aged 65 and over in residential placements provided by others" )%>%
  dplyr::distinct()%>%
  dplyr::mutate(SupportSetting="Residential",
                ActivityProvision = ifelse(Data.item.description=="residents aged 65 and over in own provision residential placements", "In House",
                                           "External"),
                year=2008,
                ITEMVALUE=as.numeric(Data.item.data))%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
  dplyr::mutate(percent_sector = ITEMVALUE /(ITEMVALUE[ActivityProvision == "External"]+ITEMVALUE[ActivityProvision == "In House"])*100) %>%
  dplyr::ungroup()


# 
#  excel_path <- "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2007\\DetailedActivityDataByCouncil2006-07 ENG.xls"
#  
#  excel_sheets <- excel_sheets(excel_path)
#  
#  for (sheet_name in excel_sheets) {
#    df <- read_excel(excel_path, sheet = sheet_name, col_types = NULL)
#    
#    csv_path <- paste0("C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2007\\council_csvs\\output_", sheet_name, ".csv")
#    
#    write.csv(df, file = csv_path, row.names = FALSE)
#  }
#  
#  write.csv(excel_sheets, file = "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2007\\council_csvs\\sheet_names.csv", row.names = FALSE)
#  




sheet_names <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2007/council_csvs/sheet_names.csv"))

sheet_names$x <- gsub(" ", "%20", sheet_names$x)

all_df <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2007/council_csvs/output_Barnsley.csv"), skip=1)%>%
  dplyr::select(dplyr::starts_with("Data"))%>%
  dplyr::mutate(DH_GEOGRAPHY_NAME=sheet_names$x[1])


for (i in sheet_names$x) {
  df <- read.csv(curl(paste0("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2007/council_csvs/output_", i, ".csv")), skip=1)%>%
    dplyr::select(dplyr::starts_with("Data"))%>%
    dplyr::mutate(DH_GEOGRAPHY_NAME=i)
  
  all_df <- rbind(all_df, df)
  print(i)
}


asc07 <- all_df %>% 
  dplyr::filter(Data.item.description=="residents aged 65 and over in own provision residential placements"|
                  Data.item.description=="residents aged 65 and over in residential placements provided by others" )%>%
  dplyr::distinct()%>%
  dplyr::mutate(SupportSetting="Residential",
                ActivityProvision = ifelse(Data.item.description=="residents aged 65 and over in own provision residential placements", "In House",
                                           "External"),
                year=2007,
                ITEMVALUE=as.numeric(Data.item.data))%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
  dplyr::mutate(percent_sector = ITEMVALUE /(ITEMVALUE[ActivityProvision == "External"]+ITEMVALUE[ActivityProvision == "In House"])*100) %>%
  dplyr::ungroup()




# 
  # excel_path <- "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2006\\per-soc-serv-exp-unit-cost-eng-2005-06-tab-v5 (1).xls"
  # 
  # excel_sheets <- excel_sheets(excel_path)
  # 
  # for (sheet_name in excel_sheets) {
  #   df <- read_excel(excel_path, sheet = sheet_name, col_types = NULL)
  #   
  #   csv_path <- paste0("C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2006\\council_csvs\\output_", sheet_name, ".csv")
  #   
  #   write.csv(df, file = csv_path, row.names = FALSE)
  # }
  # 
  # write.csv(excel_sheets, file = "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2006\\council_csvs\\sheet_names.csv", row.names = FALSE)
  # 

  sheet_names <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2006/council_csvs/sheet_names.csv"))
  
  sheet_names$x <- gsub(" ", "%20", sheet_names$x)
  
  all_df <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2006/council_csvs/output_Barnsley.csv"), skip=1)%>%
    dplyr::select(dplyr::starts_with("Data"))%>%
    dplyr::mutate(DH_GEOGRAPHY_NAME=sheet_names$x[1])
  
  
  for (i in sheet_names$x) {
    df <- read.csv(curl(paste0("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2006/council_csvs/output_", i, ".csv")), skip=1)%>%
      dplyr::select(dplyr::starts_with("Data"))%>%
      dplyr::mutate(DH_GEOGRAPHY_NAME=i)
    
    all_df <- rbind(all_df, df)
    print(i)
  }

  
  asc06 <- all_df %>% 
    dplyr::filter(Data.item.description=="residents aged 65 and over in own provision residential placements"|
                    Data.item.description=="residents aged 65 and over in residential placements provided by others" )%>%
    dplyr::distinct()%>%
    dplyr::mutate(SupportSetting="Residential",
                  ActivityProvision = ifelse(Data.item.description=="residents aged 65 and over in own provision residential placements", "In House",
                                             "External"),
                  year=2006,
                  ITEMVALUE=as.numeric(Data.item.data))%>%
    dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
    dplyr::mutate(percent_sector = ITEMVALUE /(ITEMVALUE[ActivityProvision == "External"]+ITEMVALUE[ActivityProvision == "In House"])*100) %>%
    dplyr::ungroup()

  
  
  
  
  
 
 # excel_path <- "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2005\\per-soc-serv-exp-unit-cost-eng-2004-05-dat-v1 (2).xls"
 # 
 # excel_sheets <- excel_sheets(excel_path)
 # 
 # for (sheet_name in excel_sheets) {
 #   df <- read_excel(excel_path, sheet = sheet_name, col_types = NULL)
 #   
 #   csv_path <- paste0("C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2005\\council_csvs\\output_", sheet_name, ".csv")
 #   
 #   write.csv(df, file = csv_path, row.names = FALSE)
 # }
 # 
 # write.csv(excel_sheets, file = "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2005\\council_csvs\\sheet_names.csv", row.names = FALSE)
 # 
 #  
  sheet_names <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2005/council_csvs/sheet_names.csv"))
  
  sheet_names$x <- gsub(" ", "%20", sheet_names$x)
  
  all_df <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2005/council_csvs/output_Barnsley.csv"), skip=1)%>%
    dplyr::select(dplyr::starts_with("Data"))%>%
    dplyr::mutate(DH_GEOGRAPHY_NAME=sheet_names$x[1])
  
  
  for (i in sheet_names$x) {
    df <- read.csv(curl(paste0("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2005/council_csvs/output_", i, ".csv")), skip=1)%>%
      dplyr::select(dplyr::starts_with("Data"))%>%
      dplyr::mutate(DH_GEOGRAPHY_NAME=i)
    
    all_df <- rbind(all_df, df)
    print(i)
  }
  
  
  asc05 <- all_df %>% 
    dplyr::filter(Data.item.description=="residents aged 65 and over in own provision residential placements"|
                    Data.item.description=="residents aged 65 and over in residential placements provided by others" )%>%
    dplyr::distinct()%>%
    dplyr::mutate(SupportSetting="Residential",
                  ActivityProvision = ifelse(Data.item.description=="residents aged 65 and over in own provision residential placements", "In House",
                                             "External"),
                  year=2005,
                  ITEMVALUE=as.numeric(Data.item.data))%>%
    dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
    dplyr::mutate(percent_sector = ITEMVALUE /(ITEMVALUE[ActivityProvision == "External"]+ITEMVALUE[ActivityProvision == "In House"])*100) %>%
    dplyr::ungroup()
  
  
  
  
  
  
  
 #  
 # excel_path <- "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2004\\dh_4104162.xls"
 # 
 # excel_sheets <- excel_sheets(excel_path)
 # 
 # for (sheet_name in excel_sheets) {
 #   df <- read_excel(excel_path, sheet = sheet_name, col_types = NULL)
 #   
 #   csv_path <- paste0("C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2004\\council_csvs\\output_", sheet_name, ".csv")
 #   
 #   write.csv(df, file = csv_path, row.names = FALSE)
 # }
 # 
 # write.csv(excel_sheets, file = "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2004\\council_csvs\\sheet_names.csv", row.names = FALSE)
 # 
  
  sheet_names <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2004/council_csvs/sheet_names.csv"))
  
  sheet_names$x <- gsub(" ", "%20", sheet_names$x)
  
  all_df <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2004/council_csvs/output_Barnsley.csv"), skip=1)%>%
    dplyr::select(dplyr::starts_with("Data"))%>%
    dplyr::mutate(DH_GEOGRAPHY_NAME=sheet_names$x[1])
  
  
  for (i in sheet_names$x) {
    df <- read.csv(curl(paste0("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2004/council_csvs/output_", i, ".csv")), skip=1)%>%
      dplyr::select(dplyr::starts_with("Data"))%>%
      dplyr::mutate(DH_GEOGRAPHY_NAME=i)
    
    all_df <- rbind(all_df, df)
    print(i)
  }
  
  
  asc04 <- all_df %>% 
    dplyr::filter(Data.item.description=="residents aged 65 and over in own provision residential placements"|
                    Data.item.description=="residents aged 65 and over in residential placements provided by others" )%>%
    dplyr::distinct()%>%
    dplyr::mutate(SupportSetting="Residential",
                  ActivityProvision = ifelse(Data.item.description=="residents aged 65 and over in own provision residential placements", "In House",
                                             "External"),
                  year=2004,
                  ITEMVALUE=as.numeric(Data.item.data))%>%
    dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
    dplyr::mutate(percent_sector = ITEMVALUE /(ITEMVALUE[ActivityProvision == "External"]+ITEMVALUE[ActivityProvision == "In House"])*100) %>%
    dplyr::ungroup()
  
  
  
  
  
 #  
 #  
 # excel_path <- "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2003\\dh_4080354.xls"
 # 
 # excel_sheets <- excel_sheets(excel_path)
 # 
 # for (sheet_name in excel_sheets) {
 #   df <- read_excel(excel_path, sheet = sheet_name, col_types = NULL)
 #   
 #   csv_path <- paste0("C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2003\\council_csvs\\output_", sheet_name, ".csv")
 #   
 #   write.csv(df, file = csv_path, row.names = FALSE)
 # }
 # 
 # write.csv(excel_sheets, file = "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2003\\council_csvs\\sheet_names.csv", row.names = FALSE)
 # 
  
  sheet_names <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2003/council_csvs/sheet_names.csv"))
  
  sheet_names$x <- gsub(" ", "%20", sheet_names$x)
  
  all_df <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2003/council_csvs/output_Barnsley.csv"), skip=1)%>%
    dplyr::select(dplyr::starts_with("Data"))%>%
    dplyr::mutate(DH_GEOGRAPHY_NAME=sheet_names$x[1])
  
  
  for (i in sheet_names$x) {
    df <- read.csv(curl(paste0("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2003/council_csvs/output_", i, ".csv")), skip=1)%>%
      dplyr::select(dplyr::starts_with("Data"))%>%
      dplyr::mutate(DH_GEOGRAPHY_NAME=i)
    
    all_df <- rbind(all_df, df)
    print(i)
  }
  
  
  asc03 <- all_df %>% 
    dplyr::filter(Data.item.description=="residents aged 65 and over in own provision residential placements"|
                    Data.item.description=="residents aged 65 and over in residential placements provided by others" )%>%
    dplyr::distinct()%>%
    dplyr::mutate(SupportSetting="Residential",
                  ActivityProvision = ifelse(Data.item.description=="residents aged 65 and over in own provision residential placements", "In House",
                                             "External"),
                  year=2003,
                  ITEMVALUE=as.numeric(Data.item.data))%>%
    dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
    dplyr::mutate(percent_sector = ITEMVALUE /(ITEMVALUE[ActivityProvision == "External"]+ITEMVALUE[ActivityProvision == "In House"])*100) %>%
    dplyr::ungroup()
  
  
  
  
  

plotfun <- rbind( asc03[c("percent_sector", "SupportSetting", "DH_GEOGRAPHY_NAME", "ActivityProvision", "year")],
                  asc04[c("percent_sector", "SupportSetting", "DH_GEOGRAPHY_NAME", "ActivityProvision", "year")],
                  asc05[c("percent_sector", "SupportSetting", "DH_GEOGRAPHY_NAME", "ActivityProvision", "year")],
                  asc06[c("percent_sector", "SupportSetting", "DH_GEOGRAPHY_NAME", "ActivityProvision", "year")],
                  asc07[c("percent_sector", "SupportSetting", "DH_GEOGRAPHY_NAME", "ActivityProvision", "year")],
                  asc08[c("percent_sector", "SupportSetting", "DH_GEOGRAPHY_NAME", "ActivityProvision", "year")],
                  asc09[c("percent_sector", "SupportSetting", "DH_GEOGRAPHY_NAME", "ActivityProvision", "year")],
                  asc10[c("percent_sector", "SupportSetting", "DH_GEOGRAPHY_NAME", "ActivityProvision", "year")],
                  asc11[c("percent_sector", "SupportSetting", "DH_GEOGRAPHY_NAME", "ActivityProvision", "year")],
                  asc12[c("percent_sector", "SupportSetting", "DH_GEOGRAPHY_NAME", "ActivityProvision", "year")],
                  asc13[c("percent_sector", "SupportSetting", "DH_GEOGRAPHY_NAME", "ActivityProvision", "year")],
                  asc14[c("percent_sector", "SupportSetting", "DH_GEOGRAPHY_NAME", "ActivityProvision", "year")],
                  asc15[c("percent_sector", "SupportSetting", "DH_GEOGRAPHY_NAME", "ActivityProvision", "year")],
                  asc16[c("percent_sector", "SupportSetting", "DH_GEOGRAPHY_NAME", "ActivityProvision", "year")],
                  asc17[c("percent_sector", "SupportSetting", "DH_GEOGRAPHY_NAME", "ActivityProvision", "year")],
                 asc18[c("percent_sector", "SupportSetting_Key", "DH_GEOGRAPHY_NAME", "ActivityProvision_Key")]%>%
                   dplyr::rename(SupportSetting=SupportSetting_Key,
                                 ActivityProvision=ActivityProvision_Key)%>%
                   dplyr::mutate(year=2018),
                 asc19[c("percent_sector", "SupportSetting_Key", "DH_GEOGRAPHY_NAME", "ActivityProvision_Key")]%>%
                   dplyr::rename(SupportSetting=SupportSetting_Key,
                                 ActivityProvision=ActivityProvision_Key)%>%
                   dplyr::mutate(year=2019),
                 asc20[c("percent_sector", "SupportSetting", "DH_GEOGRAPHY_NAME", "ActivityProvision")]%>%
                   dplyr::mutate(year=2020),
                 asc21[c("percent_sector", "SupportSetting", "DH_GEOGRAPHY_NAME", "ActivityProvision")]%>%
                   dplyr::mutate(year=2021),
                 asc22[c("percent_sector", "SupportSetting", "DH_GEOGRAPHY_NAME", "ActivityProvision")]%>%
                   dplyr::mutate(year=2022),
                 asc23[c("percent_sector", "SupportSetting", "DH_GEOGRAPHY_NAME", "ActivityProvision")]%>%
                   dplyr::mutate(year=2023)
                 )%>%
  dplyr::filter(ActivityProvision=="External")

library(ggplot2)

# Assuming 'filtered_df_spend' is your data frame in R

one <- ggplot(plotfun[plotfun$SupportSetting=="Nursing",], aes(x = year, y = percent_sector, color = percent_sector)) +
  geom_point(size = 3) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    x = "Year",
    y = "Outsourced activity (%)",
    title = "Percent of activity outsourced by LAs\nNursing Care",
    color = "Outsourced %"
  )+
  theme_bw()


two <- ggplot(plotfun[plotfun$SupportSetting=="Residential",], aes(x = year, y = percent_sector, color = percent_sector)) +
  geom_point(size = 3) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    x = "Year",
    y = "Outsourced activity (%)",
    title = "Residential Care",
    color = "Outsourced %"
  )+
  theme_bw()

three <- ggplot(plotfun[plotfun$SupportSetting=="",], aes(x = year, y = percent_sector, color = percent_sector)) +
  geom_point(size = 3) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    x = "Year",
    y = "Outsourced activity (%)",
    title = "Other Care",
    color = "Outsourced %"
  )+
  theme_bw()


plot <- cowplot::plot_grid(one,two,three, ncol=1)


#sp and a calcs#

  sum(asc22[asc22$SupportSetting=="Residential"&asc22$ActivityProvision=="In House",]$ITEMVALUE, na.rm=T)/sum(asc22[asc22$SupportSetting=="Residential"&asc22$ActivityProvision=="99",]$ITEMVALUE, na.rm=T)
sum(asc11[asc11$SupportSetting=="Residential"&asc11$ActivityProvision=="External",]$ITEMVALUE, na.rm=T)/(sum(asc11[asc11$SupportSetting=="Residential"&asc11$ActivityProvision=="External",]$ITEMVALUE, na.rm=T)+sum(asc11[asc11$SupportSetting=="Residential"&asc11$ActivityProvision=="In House",]$ITEMVALUE, na.rm=T))

