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


asc17 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2017/CSV%20ASCFR%20Activity.csv"))
asc16 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2016/pss-exp-eng-15-16-fin-act.csv"))
asc15 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2015/pss-exp-eng-14-15-fin-coun-lev-act-data.csv"), skip=1)



asc14 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2014/Annex_Final_CASSR_Level_Activity_Data_2013_14.csv"))
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

total17 <- asc17 %>% dplyr::filter(GEOGRAPHY_CODE!="")%>%
  dplyr::mutate(ItemValue=as.numeric(ItemValue))%>%
  dplyr::select(GEOGRAPHY_CODE, SupportSetting_KEY, ItemValue)%>%
  dplyr::group_by(GEOGRAPHY_CODE, SupportSetting_KEY) %>%
  dplyr::summarise(ItemValue = sum(ItemValue, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(ActivityProvision_KEY=1)


 asc17 <- asc17 %>% dplyr::filter(GEOGRAPHY_CODE!="")%>%
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
  
 

 total16 <- asc16 %>% dplyr::filter(Council.Code!="")%>%
   dplyr::mutate(Value=as.numeric(Value))%>%
   dplyr::select(Council.Code, Council.Name, Activity.Provision, Long.Term.Support.Setting, Value)%>%
   dplyr::group_by(Council.Code, Council.Name, Long.Term.Support.Setting) %>%
   dplyr::summarise(Value = sum(Value, na.rm  =T))%>%
   dplyr::ungroup()%>%
   dplyr::mutate(Activity.Provision="99")
 
 
 
 asc16 <- asc16 %>% dplyr::filter(Council.Code!="")%>%
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
  pivot_longer( cols=!c(Support.type, Age.category, Primary.support.reason, Service.delivery.mechanism, Provision), 
               names_to = "DH_GEOGRAPHY_NAME", values_to = "ITEMVALUE", names_prefix = "...")
 

plotfun <- rbind(asc18[c("percent_sector", "SupportSetting_Key", "DH_GEOGRAPHY_NAME", "ActivityProvision_Key")]%>%
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

