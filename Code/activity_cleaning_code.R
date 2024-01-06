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

asc23 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/ASC-FR%20Data%20File%20(descriptions)%20v2_2023.csv"))
asc22 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/ASC-FR%20Data%20File%20(descriptions)%20v2_2022.csv"))
asc21 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/ASC-FR%20Data%20File%20(descriptions)%20v2_2021.csv"))
asc20 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/ASC-FR%20Data%20File%20(descriptions)%20v2_2020.csv"))
asc19 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/ASC-FR%20Data%20File%20(descriptions)%20v2_2019.csv"))
asc18 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/Copy%20of%20ASCFR%20Data%20File%20(with%20descriptions)_2018.csv"))


asc17 <- read.csv(curl())
asc16 <- read.csv(curl())
asc15 <- read.csv(curl())
asc14 <- read.csv(curl())
asc13 <- read.csv(curl())
asc12 <- read.csv(curl())
asc11 <- read.csv(curl())
asc10 <- read.csv(curl())
asc09 <- read.csv(curl())
asc08 <- read.csv(curl())
asc07 <- read.csv(curl())
asc06 <- read.csv(curl())
asc05 <- read.csv(curl())


asc23 <- asc23 %>% dplyr::filter(GEOGRAPHY_LEVEL=="Local Authority",
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
                                 DimensionGroup=="Activity")%>%
  dplyr::mutate(ITEMVALUE = as.numeric(ITEMVALUE))%>%
  dplyr::select(GEOGRAPHY_CODE, DH_GEOGRAPHY_NAME,ActivityProvision_Key, SupportSetting_Key ,ITEMVALUE)%>%
  dplyr::group_by(GEOGRAPHY_CODE, DH_GEOGRAPHY_NAME,ActivityProvision_Key, SupportSetting_Key )%>%
  dplyr::summarise(ITEMVALUE = sum(ITEMVALUE, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, GEOGRAPHY_CODE, SupportSetting_Key) %>%
  dplyr::mutate(percent_sector = ITEMVALUE /ITEMVALUE[ActivityProvision_Key == "99"]*100) %>%
  dplyr::ungroup()

