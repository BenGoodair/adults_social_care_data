if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools, dplyr, tidyverse, tidyr, stringr,  curl, plm, readxl, zoo)


#the categories for in and out in 2000s is:
# Assessment and care management
# Nursing care placements
# Residential care placements
# Supported and other accommodation
# Direct payments
# Home care
# Day care
# Equipment and adaptations
# Meals
# Other services to older people
# TOTAL OLDER PEOPLE excluding Supporting People (LINES C1 to C10)
# Supporting People
# TOTAL OLDER PEOPLE including Supporting People (LINES C11+C12)


#2020 we have: 
# Direct Payments (mostly all outsourced by definition)
# Home Care
# Nursing care placements
# Residential care placements
# Supported accommodation 
# Other long term care

# 
# sooo, I think I clean:
#   
# Home care, residential care, nurse care, supported and other for 65+
# combined settings for old, young, total.



#lfg



#2001
 
 # excel_path <- "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2001\\spend.xls"
 # 
 # excel_sheets <- excel_sheets(excel_path)
 # 
 # for (sheet_name in excel_sheets) {
 #   df <- read_excel(excel_path, sheet = sheet_name, col_types = NULL)
 #   
 #   csv_path <- paste0("C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2001\\spend_csvs\\output_", sheet_name, ".csv")
 #   
 #   write.csv(df, file = csv_path, row.names = FALSE)
 # }
 # 
 # write.csv(excel_sheets, file = "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2001\\spend_csvs\\sheet_names.csv", row.names = FALSE)
 # 
  sheet_names <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2001/spend_csvs/sheet_names.csv"))
  
  sheet_names$x <- gsub(" ", "%20", sheet_names$x)
  

 all_df <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2001/spend_csvs/output_Barnsley.csv"), skip=3)%>%
   dplyr::select(Service, NA., Own.provision..including.joint.arrangements., Provision)%>%
   dplyr::mutate(DH_GEOGRAPHY_NAME=sheet_names$x[1])
 
 
 for (i in sheet_names$x) {
   df <- read.csv(curl(paste0("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2001/spend_csvs/output_", i, ".csv")), skip=3)%>%
     dplyr::select(Service, NA., Own.provision..including.joint.arrangements., Provision)%>%
     dplyr::mutate(DH_GEOGRAPHY_NAME=i)
   
   all_df <- rbind(all_df, df)
   print(i)
 }
 
 
 asc01 <- all_df %>% 
   dplyr::filter(Service=="C2"|
                 Service=="C3"|
                   Service=="C4"|
                   Service=="C5"|
                   Service=="C6"|
                 Service=="C11"|
                 Service=="D11"|
                 Service=="E11"|
                 Service=="F11")%>%
   dplyr::distinct()%>%
   dplyr::rename(SupportSetting=NA.)%>%
   dplyr::mutate(SupportSetting = ifelse(SupportSetting=="Home care", "Home care",
                                         ifelse(SupportSetting=="Direct payments", "Direct payments",
                                         ifelse(SupportSetting=="Nursing home placements", "Nursing home placements",
                                                ifelse(SupportSetting=="Residential care home placements", "Residential care home placements",
                                                        ifelse(SupportSetting=="Supported and other accommodation", "Supported and other accommodation",
                                                             ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH A PHYSICAL DISABILITY ETC (LINES D1 to D10)", "U65 PHYSICAL DISABILITY",
                                                                    ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH LEARNING DISABILITIES (LINES E1 to E10)", "U65 LEARNING DISABILITY",
                                                                            ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH MENTAL HEALTH NEEDS (LINES F1 to F10)", "U65 MENTAL HEALTH",
                                                                                  ifelse(SupportSetting=="TOTAL OLDER PEOPLE (LINES C1 to C10)", "Total over 65", 
                                                                                         NA))))))))))%>%
   dplyr::mutate(Total=as.character(as.numeric(Own.provision..including.joint.arrangements.)+as.numeric(Provision)))%>%
   tidyr::pivot_longer(cols = !c("SupportSetting", "DH_GEOGRAPHY_NAME", "Service"), names_to = "Sector", values_to = "Expenditure")%>%
   dplyr::mutate(Sector = ifelse(Sector=="Own.provision..including.joint.arrangements.", "In House",
                                 ifelse(Sector=="Total", "Total",
                                        "External")),
                 year=2001,
                 Expenditure=as.numeric(Expenditure))%>%
   dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting, year) %>%
   dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
   dplyr::ungroup()
 
 


#2002
 
 # excel_path <- "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2002\\spend.xls"
 # 
 # excel_sheets <- excel_sheets(excel_path)
 # 
 # for (sheet_name in excel_sheets) {
 #   df <- read_excel(excel_path, sheet = sheet_name, col_types = NULL)
 #   
 #   csv_path <- paste0("C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2002\\spend_csvs\\output_", sheet_name, ".csv")
 #   
 #   write.csv(df, file = csv_path, row.names = FALSE)
 # }
 # 
 # write.csv(excel_sheets, file = "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2002\\spend_csvs\\sheet_names.csv", row.names = FALSE)
 # 
 sheet_names <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2002/spend_csvs/sheet_names.csv"))
 
 sheet_names$x <- gsub(" ", "%20", sheet_names$x)
 
 
 all_df <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2002/spend_csvs/output_Barnsley.csv"), skip=3)%>%
   dplyr::select(Service, NA., Own.provision..including.joint.arrangements., Provision)%>%
   dplyr::mutate(DH_GEOGRAPHY_NAME=sheet_names$x[1])
 
 
 for (i in sheet_names$x) {
   df <- read.csv(curl(paste0("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2002/spend_csvs/output_", i, ".csv")), skip=3)%>%
     dplyr::select(Service, NA., Own.provision..including.joint.arrangements., Provision)%>%
     dplyr::mutate(DH_GEOGRAPHY_NAME=i)
   
   all_df <- rbind(all_df, df)
   print(i)
 }
 
 
 asc02 <- all_df %>% 
   dplyr::filter(Service=="C2"|
                   Service=="C3"|
                   Service=="C4"|
                   Service=="C5"|
                   Service=="C6"|
                   Service=="C11"|
                   Service=="D11"|
                   Service=="E11"|
                   Service=="F11")%>%
   dplyr::distinct()%>%
   dplyr::rename(SupportSetting=NA.)%>%
   dplyr::mutate(SupportSetting = ifelse(SupportSetting=="Home care", "Home care",
                                         ifelse(SupportSetting=="Direct payments", "Direct payments",
                                         ifelse(SupportSetting=="Nursing home placements", "Nursing home placements",
                                                ifelse(SupportSetting=="Residential care home placements", "Residential care home placements",
                                                       ifelse(SupportSetting=="Supported and other accommodation","Supported and other accommodation",
                                                              ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH A PHYSICAL DISABILITY ETC (LINES D1 to D10)", "U65 PHYSICAL DISABILITY",
                                                                     ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH LEARNING DISABILITIES (LINES E1 to E10)", "U65 LEARNING DISABILITY",
                                                                            ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH MENTAL HEALTH NEEDS (LINES F1 to F10)", "U65 MENTAL HEALTH",
                                                                                   ifelse(SupportSetting=="TOTAL OLDER PEOPLE (LINES C1 to C10)", "Total over 65", NA))))))))))%>%
   dplyr::mutate(Total=as.character(as.numeric(Own.provision..including.joint.arrangements.)+as.numeric(Provision)))%>%
   tidyr::pivot_longer(cols = !c("SupportSetting", "DH_GEOGRAPHY_NAME", "Service"), names_to = "Sector", values_to = "Expenditure")%>%
   dplyr::mutate(Sector = ifelse(Sector=="Own.provision..including.joint.arrangements.", "In House",
                                 ifelse(Sector=="Total", "Total",
                                        "External")),
                 year=2002,
                 Expenditure=as.numeric(Expenditure))%>%
   dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting, year) %>%
   dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
   dplyr::ungroup()
 
 
 #2003
 
 # excel_path <- "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2003\\spend.xls"
 # 
 # excel_sheets <- excel_sheets(excel_path)
 # 
 # for (sheet_name in excel_sheets) {
 #   df <- read_excel(excel_path, sheet = sheet_name, col_types = NULL)
 #   
 #   csv_path <- paste0("C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2003\\spend_csvs\\output_", sheet_name, ".csv")
 #   
 #   write.csv(df, file = csv_path, row.names = FALSE)
 # }
 # 
 # write.csv(excel_sheets, file = "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2003\\spend_csvs\\sheet_names.csv", row.names = FALSE)
 
 
 sheet_names <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2003/spend_csvs/sheet_names.csv"))
 
 sheet_names$x <- gsub(" ", "%20", sheet_names$x)
 
 
 all_df <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2003/spend_csvs/output_Barnsley.csv"), skip=3)%>%
   dplyr::select(Service, NA., Own.provision..including.joint.arrangements., Provision)%>%
   dplyr::mutate(DH_GEOGRAPHY_NAME=sheet_names$x[1])
 
 
 for (i in sheet_names$x) {
   df <- read.csv(curl(paste0("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2003/spend_csvs/output_", i, ".csv")), skip=3)%>%
     dplyr::select(Service, NA., Own.provision..including.joint.arrangements., Provision)%>%
     dplyr::mutate(DH_GEOGRAPHY_NAME=i)
   
   all_df <- rbind(all_df, df)
   print(i)
 }
 
 
 asc03 <- all_df %>% 
   dplyr::filter(Service=="C2"|
                   Service=="C3"|
                   Service=="C4"|
                   Service=="C5"|
                   Service=="C6"|
                   Service=="C11"|
                   Service=="D11"|
                   Service=="E11"|
                   Service=="F11")%>%
   dplyr::distinct()%>%
   dplyr::rename(SupportSetting=NA.)%>%
   dplyr::mutate(SupportSetting = ifelse(SupportSetting=="Home care", "Home care",
                                         ifelse(SupportSetting=="Direct payments", "Direct payments",
                                                ifelse(SupportSetting=="Nursing care placements", "Nursing home placements",
                                                ifelse(SupportSetting=="Residential care placements", "Residential care home placements",
                                                       ifelse(SupportSetting=="Supported and other accommodation", "Supported and other accommodation",
                                                              ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH A PHYSICAL DISABILITY ETC (LINES D1 to D10)", "U65 PHYSICAL DISABILITY",
                                                                     ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH LEARNING DISABILITIES (LINES E1 to E10)", "U65 LEARNING DISABILITY",
                                                                            ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH MENTAL HEALTH NEEDS (LINES F1 to F10)", "U65 MENTAL HEALTH",
                                                                                   ifelse(SupportSetting=="TOTAL OLDER PEOPLE (LINES C1 to C10)", "Total over 65", NA))))))))))%>%
   dplyr::mutate(Total=as.character(as.numeric(Own.provision..including.joint.arrangements.)+as.numeric(Provision)))%>%
   tidyr::pivot_longer(cols = !c("SupportSetting", "DH_GEOGRAPHY_NAME", "Service"), names_to = "Sector", values_to = "Expenditure")%>%
   dplyr::mutate(Sector = ifelse(Sector=="Own.provision..including.joint.arrangements.", "In House",
                                 ifelse(Sector=="Total", "Total",
                                        "External")),
                 year=2003,
                 Expenditure=as.numeric(Expenditure))%>%
   dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting, year) %>%
   dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
   dplyr::ungroup()
 
 
 
 
 #2004
 
 # excel_path <- "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2004\\spend.xls"
 # 
 # excel_sheets <- excel_sheets(excel_path)
 # 
 # for (sheet_name in excel_sheets) {
 #   df <- read_excel(excel_path, sheet = sheet_name, col_types = NULL)
 #   
 #   csv_path <- paste0("C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2004\\spend_csvs\\output_", sheet_name, ".csv")
 #   
 #   write.csv(df, file = csv_path, row.names = FALSE)
 # }
 # 
 # write.csv(excel_sheets, file = "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2004\\spend_csvs\\sheet_names.csv", row.names = FALSE)
 # 
 
 
 sheet_names <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2004/spend_csvs/sheet_names.csv"))
 
 sheet_names$x <- gsub(" ", "%20", sheet_names$x)
 
 
 all_df <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2004/spend_csvs/output_Barnsley.csv"), skip=3)%>%
   dplyr::select(Service, NA., Own.provision..including.joint.arrangements., Provision)%>%
   dplyr::mutate(DH_GEOGRAPHY_NAME=sheet_names$x[1])
 
 
 for (i in sheet_names$x) {
   df <- read.csv(curl(paste0("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2004/spend_csvs/output_", i, ".csv")), skip=3)%>%
     dplyr::select(Service, NA., Own.provision..including.joint.arrangements., Provision)%>%
     dplyr::mutate(DH_GEOGRAPHY_NAME=i)
   
   all_df <- rbind(all_df, df)
   print(i)
 }
 
 
 asc04 <- all_df %>% 
   dplyr::filter(Service=="C2"|
                   Service=="C3"|
                   Service=="C4"|
                   Service=="C5"|
                   Service=="C6"|
                   Service=="C11"|
                   Service=="D11"|
                   Service=="E11"|
                   Service=="F11")%>%
   dplyr::distinct()%>%
   dplyr::rename(SupportSetting=NA.)%>%
   dplyr::mutate(SupportSetting = ifelse(SupportSetting=="Home care", "Home care",
                                         ifelse(SupportSetting=="Direct payments", "Direct payments",
                                                ifelse(SupportSetting=="Nursing care placements", "Nursing home placements",
                                                ifelse(SupportSetting=="Residential care placements", "Residential care home placements",
                                                       ifelse(SupportSetting=="Supported and other accommodation", "Supported and other accommodation",
                                                              ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH A PHYSICAL DISABILITY ETC excl SP (LINES D1 to D10)", "U65 PHYSICAL DISABILITY",
                                                                     ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH LEARNING DISABILITIES excl SP (LINES E1 to E10)", "U65 LEARNING DISABILITY",
                                                                            ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH MENTAL HEALTH NEEDS excl SP (LINES F1 to F10)", "U65 MENTAL HEALTH",
                                                                                   ifelse(SupportSetting=="TOTAL OLDER PEOPLE excluding Supporting People (LINES C1 to C10)", "Total over 65", NA))))))))))%>%
   dplyr::mutate(Total=as.character(as.numeric(Own.provision..including.joint.arrangements.)+as.numeric(Provision)))%>%
   tidyr::pivot_longer(cols = !c("SupportSetting", "DH_GEOGRAPHY_NAME", "Service"), names_to = "Sector", values_to = "Expenditure")%>%
   dplyr::mutate(Sector = ifelse(Sector=="Own.provision..including.joint.arrangements.", "In House",
                                 ifelse(Sector=="Total", "Total",
                                        "External")),
                 year=2004,
                 Expenditure=as.numeric(Expenditure))%>%
   dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting, year) %>%
   dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
   dplyr::ungroup()
 
 
 
 #2005
 
  # 
  # excel_path <- "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2005\\spend.xls"
  # 
  # excel_sheets <- excel_sheets(excel_path)
  # 
  # for (sheet_name in excel_sheets) {
  #   df <- read_excel(excel_path, sheet = sheet_name, col_types = NULL)
  #   
  #   csv_path <- paste0("C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2005\\spend_csvs\\output_", sheet_name, ".csv")
  #   
  #   write.csv(df, file = csv_path, row.names = FALSE)
  # }
  # 
  # write.csv(excel_sheets, file = "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2005\\spend_csvs\\sheet_names.csv", row.names = FALSE)
  # 
  # 
  
  sheet_names <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2005/spend_csvs/sheet_names.csv"))
  
  sheet_names$x <- gsub(" ", "%20", sheet_names$x)

  all_df <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2005/spend_csvs/output_Barnsley.csv"), skip=3)%>%
    dplyr::select(Service, NA., Own.provision..including.joint.arrangements., Provision)%>%
    dplyr::mutate(DH_GEOGRAPHY_NAME=sheet_names$x[1])
  
  
  for (i in sheet_names$x) {
    df <- read.csv(curl(paste0("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2005/spend_csvs/output_", i, ".csv")), skip=3)%>%
      dplyr::select(Service, NA., Own.provision..including.joint.arrangements., Provision)%>%
      dplyr::mutate(DH_GEOGRAPHY_NAME=i)
    
    all_df <- rbind(all_df, df)
    print(i)
  }

  
  asc05 <- all_df %>% 
    dplyr::filter(Service=="C2"|
                    Service=="C3"|
                    Service=="C4"|
                    Service=="C5"|
                    Service=="C6"|
                    Service=="C11"|
                    Service=="D11"|
                    Service=="E11"|
                    Service=="F11")%>%
    dplyr::distinct()%>%
    dplyr::rename(SupportSetting=NA.)%>%
    dplyr::mutate(SupportSetting = ifelse(SupportSetting=="Home care", "Home care",
                                          ifelse(SupportSetting=="Direct payments", "Direct payments",
                                                 ifelse(SupportSetting=="Nursing care placements", "Nursing home placements",
                                                 ifelse(SupportSetting=="Residential care placements", "Residential care home placements",
                                                        ifelse(SupportSetting=="Supported and other accommodation", "Supported and other accommodation",
                                                               ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH A PHYSICAL DISABILITY ETC excl SP (LINES D1 to D10)", "U65 PHYSICAL DISABILITY",
                                                                      ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH LEARNING DISABILITIES excl SP (LINES E1 to E10)", "U65 LEARNING DISABILITY",
                                                                             ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH MENTAL HEALTH NEEDS excl SP (LINES F1 to F10)", "U65 MENTAL HEALTH",
                                                                                    ifelse(SupportSetting=="TOTAL OLDER PEOPLE excluding Supporting People (LINES C1 to C10)", "Total over 65", NA))))))))))%>%
    dplyr::mutate(Total=as.character(as.numeric(Own.provision..including.joint.arrangements.)+as.numeric(Provision)))%>%
    tidyr::pivot_longer(cols = !c("SupportSetting", "DH_GEOGRAPHY_NAME", "Service"), names_to = "Sector", values_to = "Expenditure")%>%
    dplyr::mutate(Sector = ifelse(Sector=="Own.provision..including.joint.arrangements.", "In House",
                                  ifelse(Sector=="Total", "Total",
                                         "External")),
                  year=2005,
                  Expenditure=as.numeric(Expenditure),
                  Expenditure = ifelse(DH_GEOGRAPHY_NAME=="Cheshire"&SupportSetting=="Supported and other accommodation", NA,Expenditure))%>%
    dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting, year) %>%
    dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
    dplyr::ungroup()
  
 
#2006
  
   # excel_path <- "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2006\\spend.xls"
   # 
   # excel_sheets <- excel_sheets(excel_path)
   # 
   # for (sheet_name in excel_sheets) {
   #   df <- read_excel(excel_path, sheet = sheet_name, col_types = NULL)
   #   
   #   csv_path <- paste0("C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2006\\spend_csvs\\output_", sheet_name, ".csv")
   #   
   #   write.csv(df, file = csv_path, row.names = FALSE)
   # }
   # 
   # write.csv(excel_sheets, file = "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2006\\spend_csvs\\sheet_names.csv", row.names = FALSE)
   # 
  
  
  sheet_names <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2006/spend_csvs/sheet_names.csv"))
  
  sheet_names$x <- gsub(" ", "%20", sheet_names$x)
  
  
  all_df <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2006/spend_csvs/output_Barnsley.csv"), skip=3)%>%
    dplyr::select(Service, NA., Own.provision..including.joint.arrangements., Provision.by.others)%>%
    dplyr::mutate(DH_GEOGRAPHY_NAME=sheet_names$x[1])
  
  
  for (i in sheet_names$x) {
    df <- read.csv(curl(paste0("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2006/spend_csvs/output_", i, ".csv")), skip=3)%>%
      dplyr::select(Service, NA., Own.provision..including.joint.arrangements., Provision.by.others)%>%
      dplyr::mutate(DH_GEOGRAPHY_NAME=i)
    
    all_df <- rbind(all_df, df)
    print(i)
  }
  
  
  asc06 <- all_df %>% 
    dplyr::filter(Service=="C2"|
                    Service=="C3"|
                    Service=="C4"|
                    Service=="C5"|
                    Service=="C6"|
                    Service=="C11"|
                    Service=="D11"|
                    Service=="E11"|
                    Service=="F11")%>%
    dplyr::distinct()%>%
    dplyr::rename(SupportSetting=NA.)%>%
    dplyr::mutate(SupportSetting = ifelse(SupportSetting=="Home care", "Home care",
                                          ifelse(SupportSetting=="Nursing care placements", "Nursing home placements",
                                                 ifelse(SupportSetting=="Direct payments", "Direct payments",
                                                        ifelse(SupportSetting=="Residential care placements", "Residential care home placements",
                                                        ifelse(SupportSetting=="Supported and other accommodation", "Supported and other accommodation",
                                                               ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH A PHYSICAL DISABILITY ETC excl SP (LINES D1 to D10)", "U65 PHYSICAL DISABILITY",
                                                                      ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH LEARNING DISABILITIES excl SP (LINES E1 to E10)", "U65 LEARNING DISABILITY",
                                                                             ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH MENTAL HEALTH NEEDS excl SP (LINES F1 to F10)", "U65 MENTAL HEALTH",
                                                                                    ifelse(SupportSetting=="TOTAL OLDER PEOPLE excluding Supporting People (LINES C1 to C10)", "Total over 65", NA))))))))))%>%
    dplyr::mutate(Total=as.character(as.numeric(Own.provision..including.joint.arrangements.)+as.numeric(Provision.by.others)))%>%
    tidyr::pivot_longer(cols = !c("SupportSetting", "DH_GEOGRAPHY_NAME", "Service"), names_to = "Sector", values_to = "Expenditure")%>%
    dplyr::mutate(Sector = ifelse(Sector=="Own.provision..including.joint.arrangements.", "In House",
                                  ifelse(Sector=="Total", "Total",
                                         "External")),
                  year=2006,
                  Expenditure=as.numeric(Expenditure))%>%
    dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting, year) %>%
    dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
    dplyr::ungroup()
  
#2007
   
    # excel_path <- "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2007\\DetailedPSSByCouncil2006-07 ENG.xls"
    # 
    # excel_sheets <- excel_sheets(excel_path)
    # 
    # for (sheet_name in excel_sheets) {
    #   df <- read_excel(excel_path, sheet = sheet_name, col_types = NULL)
    #   
    #   csv_path <- paste0("C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2007\\spend_csvs\\output_", sheet_name, ".csv")
    #   
    #   write.csv(df, file = csv_path, row.names = FALSE)
    # }
    # 
    # write.csv(excel_sheets, file = "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2007\\spend_csvs\\sheet_names.csv", row.names = FALSE)
    # 
  
  sheet_names <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2007/spend_csvs/sheet_names.csv"))
  
  sheet_names$x <- gsub(" ", "%20", sheet_names$x)
  
  
  all_df <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2007/spend_csvs/output_Barnsley.csv"), skip=3)%>%
    dplyr::select(Service, NA., Own.provision..including.joint.arrangements., Provision.by.others)%>%
    dplyr::mutate(DH_GEOGRAPHY_NAME=sheet_names$x[1])
  
  
  for (i in sheet_names$x) {
    df <- read.csv(curl(paste0("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2007/spend_csvs/output_", i, ".csv")), skip=3)%>%
      dplyr::select(Service, NA., Own.provision..including.joint.arrangements., Provision.by.others)%>%
      dplyr::mutate(DH_GEOGRAPHY_NAME=i)
    
    all_df <- rbind(all_df, df)
    print(i)
  }
  
  
  asc07 <- all_df %>% 
    dplyr::filter(Service=="C2"|
                    Service=="C3"|
                    Service=="C4"|
                    Service=="C5"|
                    Service=="C6"|
                    Service=="C11"|
                    Service=="D11"|
                    Service=="E11"|
                    Service=="F11")%>%
    dplyr::distinct()%>%
    dplyr::rename(SupportSetting=NA.)%>%
    dplyr::mutate(SupportSetting = ifelse(SupportSetting=="Home care", "Home care",
                                          ifelse(SupportSetting=="Nursing care placements", "Nursing home placements",
                                                 ifelse(SupportSetting=="Direct payments", "Direct payments",
                                                        ifelse(SupportSetting=="Residential care placements", "Residential care home placements",
                                                        ifelse(SupportSetting=="Supported and other accommodation", "Supported and other accommodation",
                                                               ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH A PHYSICAL DISABILITY ETC excl SP (LINES D1 to D10)", "U65 PHYSICAL DISABILITY",
                                                                      ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH LEARNING DISABILITIES excl SP (LINES E1 to E10)", "U65 LEARNING DISABILITY",
                                                                             ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH MENTAL HEALTH NEEDS excl SP (LINES F1 to F10)", "U65 MENTAL HEALTH",
                                                                                    ifelse(SupportSetting=="TOTAL OLDER PEOPLE excluding Supporting People (LINES C1 to C10)", "Total over 65", NA))))))))))%>%
    dplyr::mutate(Total=as.character(as.numeric(Own.provision..including.joint.arrangements.)+as.numeric(Provision.by.others)))%>%
    tidyr::pivot_longer(cols = !c("SupportSetting", "DH_GEOGRAPHY_NAME", "Service"), names_to = "Sector", values_to = "Expenditure")%>%
    dplyr::mutate(Sector = ifelse(Sector=="Own.provision..including.joint.arrangements.", "In House",
                                  ifelse(Sector=="Total", "Total",
                                         "External")),
                  year=2007,
                  Expenditure=as.numeric(Expenditure))%>%
    dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting, year) %>%
    dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
    dplyr::ungroup()
  
 
#2008
  
   # excel_path <- "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2008\\DetailedPSSByCouncil2007-08 (values only).xls"
   # 
   # excel_sheets <- excel_sheets(excel_path)
   # 
   # for (sheet_name in excel_sheets) {
   #   df <- read_excel(excel_path, sheet = sheet_name, col_types = NULL)
   #   
   #   csv_path <- paste0("C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2008\\spend_csvs\\output_", sheet_name, ".csv")
   #   
   #   write.csv(df, file = csv_path, row.names = FALSE)
   # }
   # 
   # write.csv(excel_sheets, file = "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2008\\spend_csvs\\sheet_names.csv", row.names = FALSE)
   # 
   
   
   
   
   sheet_names <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2008/spend_csvs/sheet_names.csv"))
   
   sheet_names$x <- gsub(" ", "%20", sheet_names$x)
   
   
   all_df <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2008/spend_csvs/output_106-Gateshead.csv"), skip=3)%>%
     dplyr::select(Service, NA., Current.expenditure.including.capital.charges, NA..1)%>%
     dplyr::mutate(DH_GEOGRAPHY_NAME=sheet_names$x[1])
   
   
   for (i in sheet_names$x) {
     df <- read.csv(curl(paste0("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2008/spend_csvs/output_", i, ".csv")), skip=3)%>%
       dplyr::select(Service, NA., Current.expenditure.including.capital.charges, NA..1)%>%
       dplyr::mutate(DH_GEOGRAPHY_NAME=i)
     
     all_df <- rbind(all_df, df)
     print(i)
   }
   
   
   asc08 <- all_df %>% 
     dplyr::filter(Service=="B2"|
                     Service=="B3"|
                     Service=="B4"|
                     Service=="B5"|
                     Service=="B6"|
                     Service=="B11"|
                     Service=="C11"|
                     Service=="D11"|
                     Service=="E11")%>%
     dplyr::distinct()%>%
     dplyr::rename(SupportSetting=NA.)%>%
     dplyr::mutate(SupportSetting = ifelse(SupportSetting=="Home care", "Home care",
                                           ifelse(SupportSetting=="Nursing care placements", "Nursing home placements",
                                                  ifelse(SupportSetting=="Direct payments", "Direct payments",
                                                         ifelse(SupportSetting=="Residential care placements", "Residential care home placements",
                                                         ifelse(SupportSetting=="Supported and other accommodation", "Supported and other accommodation",
                                                                ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH A PHYSICAL DISABILITY ETC excl SP (LINES C1 to C10)", "U65 PHYSICAL DISABILITY",
                                                                       ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH LEARNING DISABILITIES excl SP (LINES D1 to D10)", "U65 LEARNING DISABILITY",
                                                                              ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH MENTAL HEALTH NEEDS excl SP (LINES E1 to E10)", "U65 MENTAL HEALTH",
                                                                                     ifelse(SupportSetting=="TOTAL OLDER PEOPLE excluding Supporting People (LINES B1 to B10)", "Total over 65", NA))))))))))%>%
     dplyr::mutate(Total=as.character(as.numeric(Current.expenditure.including.capital.charges)+as.numeric(NA..1)))%>%
     tidyr::pivot_longer(cols = !c("SupportSetting", "DH_GEOGRAPHY_NAME", "Service"), names_to = "Sector", values_to = "Expenditure")%>%
     dplyr::mutate(Sector = ifelse(Sector=="Current.expenditure.including.capital.charges", "In House",
                                   ifelse(Sector=="Total", "Total",
                                          "External")),
                   year=2008,
                   Expenditure=as.numeric(Expenditure))%>%
     dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting, year) %>%
     dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
     dplyr::ungroup()
 
   
   
   
#2009
   
   asc09 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2009/pers-soc-serv-exp-unit-cost-eng-2008-09-units%20(1).csv"), skip=2)%>%
     dplyr::rename(residential_total=Gross.total.cost.for.residential.care.for.older.people.during.year.ended.31.March...000.s.,
                   residential_public = Gross.total.cost.for.own.provision.residential.care.for.older.people.during.year.ended.31.March...000.s.,
                   residential_private = Gross.total.cost.for.residential.care.for.older.people.provided.by.others.during.year.ended.31.March...000.s.,
                   DH_GEOGRAPHY_NAME=X)%>%
     dplyr::select(DH_GEOGRAPHY_NAME, residential_total, residential_public, residential_private)%>%
     tidyr::pivot_longer(cols=!DH_GEOGRAPHY_NAME, names_to = "Sector", values_to = "Expenditure")%>%
     dplyr::mutate(SupportSetting = "Residential care home placements",
                   Sector = ifelse(Sector=="residential_total", "Total",
                                   ifelse(Sector=="residential_public", "In House",
                                          ifelse(Sector=="residential_private", "External",NA
                                          ))),
                   Service=NA,
                   year=2009,
                   Expenditure = as.numeric(gsub(",","",Expenditure)))%>%
     dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting, year) %>%
     dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
     dplyr::ungroup()
   
   
#2010
####NEED TO CREATE A TOTAL FOR ALL PROVISION TYPES PLZ####   
   
   asc10 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2010/PSSEX_0910_OUTPUT.csv"))%>%
     dplyr::filter(MainHeading=="1. GROSS TOTAL COST (Current expenditure including capital charges)",
                   Category.Order==2&Subset.Order==22|
                     Category.Order==2&Subset.Order==5|
                     Category.Order==2&Subset.Order==6|
                     Category.Order==2&Subset.Order==7|
                     Category.Order==2&Subset.Order==8|
                     Category.Order==2&Subset.Order==11|
                     Category.Order==5&Subset.Order==23|
                     Category.Order==6&Subset.Order==24|
                     Category.Order==7&Subset.Order==25)%>%
     dplyr::select(council, SubHeading, Subset, Value)%>%
     dplyr::mutate(year=2010,
                   SubHeading= ifelse(SubHeading=="Own Provision", "In House",
                                      ifelse(SubHeading=="Provision by Others", "External",SubHeading)),
                   Subset = ifelse(Subset=="Home care", "Home care",
                                   ifelse(Subset=="Nursing and residential care placements total", "Nursing home placements",
                                          ifelse(Subset=="Direct Payments", "Direct payments",
                                                 ifelse(Subset=="Residential care placements", "Residential care home placements",
                                                   ifelse(Subset=="Supported and other accommodation", "Supported and other accommodation",
                                                          ifelse(Subset=="TOTAL ADULTS with PSD, Aged 18 to 64, excluding Supporting People", "U65 PHYSICAL DISABILITY",
                                                                 ifelse(Subset=="TOTAL ADULTS WITH LD, Aged 18 to 64, excluding Supporting People", "U65 LEARNING DISABILITY",
                                                                        ifelse(Subset=="TOTAL ADULTS WITH MH NEEDS, Aged 18 to 64, excluding Supporting People", "U65 MENTAL HEALTH",
                                                                               ifelse(Subset=="TOTAL OLDER PEOPLE excluding Supporting People", "Total over 65", NA))))))))))%>%
     dplyr::group_by(council, SubHeading)%>%
     dplyr::mutate(Value= ifelse(Subset=="Nursing home placements", Value-Value[Subset=="Residential care home placements"], Value),
                   Service = NA)%>%
   dplyr::ungroup()%>%
     dplyr::rename(Expenditure=Value,
                   DH_GEOGRAPHY_NAME = council,
                   SupportSetting = Subset,
                   Sector = SubHeading)
     
tot_sector <- asc10 %>% dplyr::select(DH_GEOGRAPHY_NAME, SupportSetting, Expenditure)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, SupportSetting)%>%
  dplyr::summarise(Expenditure=sum(Expenditure, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(Sector="Total")

asc10 <- asc10%>%bind_rows(., tot_sector)%>%
  dplyr::mutate(year=2010)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting, year) %>%
  dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
  dplyr::ungroup()
  


asc11 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2011/Final_Council_Level_Expenditure_Data_2010-11.csv"))%>%
  dplyr::filter(MainHeading=="1. GROSS TOTAL COST (Current expenditure including capital charges)",
                Category.Order==2&Subset.Order==22|
                  Category.Order==2&Subset.Order==5|
                  Category.Order==2&Subset.Order==6|
                  Category.Order==2&Subset.Order==7|
                  Category.Order==2&Subset.Order==8|
                  Category.Order==2&Subset.Order==11|
                  Category.Order==5&Subset.Order==23|
                  Category.Order==6&Subset.Order==24|
                  Category.Order==7&Subset.Order==25)%>%
  dplyr::select(council, SubHeading, Subset, Value)%>%
  dplyr::mutate(year=2011,
                SubHeading= ifelse(SubHeading=="Own Provision", "In House",
                                   ifelse(SubHeading=="Provision by Others", "External",SubHeading)),
                Subset = ifelse(Subset=="Home care", "Home care",
                                ifelse(Subset=="Nursing and residential care placements total", "Nursing home placements",
                                       ifelse(Subset=="Direct Payments", "Direct payments",
                                              ifelse(Subset=="Residential care placements", "Residential care home placements",
                                              ifelse(Subset=="Supported and other accommodation", "Supported and other accommodation",
                                                     ifelse(Subset=="TOTAL ADULTS with PSD, Aged 18 to 64, excluding Supporting People", "U65 PHYSICAL DISABILITY",
                                                            ifelse(Subset=="TOTAL ADULTS WITH LD, Aged 18 to 64, excluding Supporting People", "U65 LEARNING DISABILITY",
                                                                   ifelse(Subset=="TOTAL ADULTS WITH MH NEEDS, Aged 18 to 64, excluding Supporting People", "U65 MENTAL HEALTH",
                                                                          ifelse(Subset=="TOTAL OLDER PEOPLE excluding Supporting People", "Total over 65", NA))))))))))%>%
  dplyr::group_by(council, SubHeading)%>%
  dplyr::mutate(Value= ifelse(Subset=="Nursing home placements", Value-Value[Subset=="Residential care home placements"], Value),
                Service = NA)%>%
  dplyr::ungroup()%>%
  dplyr::rename(Expenditure=Value,
                DH_GEOGRAPHY_NAME = council,
                SupportSetting = Subset,
                Sector = SubHeading)

tot_sector <- asc11 %>% dplyr::select(DH_GEOGRAPHY_NAME, SupportSetting, Expenditure)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, SupportSetting)%>%
  dplyr::summarise(Expenditure=sum(Expenditure, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(Sector="Total")

asc11 <- asc11%>%bind_rows(., tot_sector)%>%
  dplyr::mutate(year=2011)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting, year) %>%
  dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
  dplyr::ungroup()

#2012

asc12 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2012/Final_PSS_Expenditure_2011_12_Raw_Data.csv"))%>%
  dplyr::filter(MainHeading=="1. GROSS TOTAL COST (Current expenditure including capital charges)",
                Category.Order==2&Subset.Order==22|
                  Category.Order==2&Subset.Order==5|
                  Category.Order==2&Subset.Order==6|
                  Category.Order==2&Subset.Order==7|
                  Category.Order==2&Subset.Order==8|
                  Category.Order==2&Subset.Order==11|
                  Category.Order==5&Subset.Order==23|
                  Category.Order==6&Subset.Order==24|
                  Category.Order==7&Subset.Order==25)%>%
  dplyr::select(council, SubHeading, Subset, Value)%>%
  dplyr::mutate(year=2012,
                SubHeading= ifelse(SubHeading=="Own Provision", "In House",
                                   ifelse(SubHeading=="Provision by Others", "External",SubHeading)),
                Subset = ifelse(Subset=="Home care", "Home care",
                                ifelse(Subset=="Nursing and residential care placements total", "Nursing home placements",
                                       ifelse(Subset=="Residential care placements", "Residential care home placements",
                                              ifelse(Subset=="Direct Payments", "Direct payments",
                                                     ifelse(Subset=="Supported and other accommodation", "Supported and other accommodation",
                                                     ifelse(Subset=="TOTAL ADULTS with PSD, Aged 18 to 64, excluding Supporting People", "U65 PHYSICAL DISABILITY",
                                                            ifelse(Subset=="TOTAL ADULTS WITH LD, Aged 18 to 64, excluding Supporting People", "U65 LEARNING DISABILITY",
                                                                   ifelse(Subset=="TOTAL ADULTS WITH MH NEEDS, Aged 18 to 64, excluding Supporting People", "U65 MENTAL HEALTH",
                                                                          ifelse(Subset=="TOTAL OLDER PEOPLE excluding Supporting People", "Total over 65", NA))))))))))%>%
  dplyr::group_by(council, SubHeading)%>%
  dplyr::mutate(Value= ifelse(Subset=="Nursing home placements", Value-Value[Subset=="Residential care home placements"], Value),
                Service = NA)%>%
  dplyr::ungroup()%>%
  dplyr::rename(Expenditure=Value,
                DH_GEOGRAPHY_NAME = council,
                SupportSetting = Subset,
                Sector = SubHeading)

tot_sector <- asc12 %>% dplyr::select(DH_GEOGRAPHY_NAME, SupportSetting, Expenditure)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, SupportSetting)%>%
  dplyr::summarise(Expenditure=sum(Expenditure, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(Sector="Total")

asc12 <- asc12%>%bind_rows(., tot_sector)%>%
  dplyr::mutate(year=2012)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting, year) %>%
  dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
  dplyr::ungroup()

#2013

asc13 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2013/Final_PSS_Expenditure_2012_13_Raw_Data.csv"))%>%
  dplyr::filter(MainHeading=="1. GROSS TOTAL COST (Current expenditure including capital charges)",
                  Category=="B. OLDER PEOPLE (AGED 65 OR OVER) INCLUDING OLDER MENTALLY ILL"&Subset=="TOTAL OLDER PEOPLE excluding Supporting People"|
                  Category=="B. OLDER PEOPLE (AGED 65 OR OVER) INCLUDING OLDER MENTALLY ILL"&Subset=="Nursing care placements"|
                  Category=="B. OLDER PEOPLE (AGED 65 OR OVER) INCLUDING OLDER MENTALLY ILL"&Subset=="Residential care placements"|
                  Category=="B. OLDER PEOPLE (AGED 65 OR OVER) INCLUDING OLDER MENTALLY ILL"&Subset=="Supported and other accommodation"|
                  Category=="B. OLDER PEOPLE (AGED 65 OR OVER) INCLUDING OLDER MENTALLY ILL"&Subset=="Direct Payments"|
                  Category=="B. OLDER PEOPLE (AGED 65 OR OVER) INCLUDING OLDER MENTALLY ILL"&Subset=="Home care"|
                  Subset=="TOTAL ADULTS with PSD, Aged 18 to 64, excluding Supporting People"|
                  Subset=="TOTAL ADULTS WITH LD, Aged 18 to 64, excluding Supporting People"|
                  Subset=="TOTAL ADULTS WITH MH NEEDS, Aged 18 to 64, excluding Supporting People")%>%
  dplyr::select(Name, SubHeading, Subset, Value)%>%
  dplyr::mutate(year=2013,
                SubHeading= ifelse(SubHeading=="Own Provision", "In House",
                                   ifelse(SubHeading=="Provision by Others", "External",SubHeading)),
                Subset = ifelse(Subset=="Home care", "Home care",
                                ifelse(Subset=="Nursing care placements", "Nursing home placements",
                                       ifelse(Subset=="Residential care placements", "Residential care home placements",
                                              ifelse(Subset=="Direct Payments", "Direct payments",
                                                     ifelse(Subset=="Supported and other accommodation", "Supported and other accommodation",
                                                     ifelse(Subset=="TOTAL ADULTS with PSD, Aged 18 to 64, excluding Supporting People", "U65 PHYSICAL DISABILITY",
                                                            ifelse(Subset=="TOTAL ADULTS WITH LD, Aged 18 to 64, excluding Supporting People", "U65 LEARNING DISABILITY",
                                                                   ifelse(Subset=="TOTAL ADULTS WITH MH NEEDS, Aged 18 to 64, excluding Supporting People", "U65 MENTAL HEALTH",
                                                                          ifelse(Subset=="TOTAL OLDER PEOPLE excluding Supporting People", "Total over 65", NA))))))))))%>%
  dplyr::group_by(Name, SubHeading)%>%
  dplyr::mutate(Service = NA)%>%
  dplyr::ungroup()%>%
  dplyr::rename(Expenditure=Value,
                DH_GEOGRAPHY_NAME = Name,
                SupportSetting = Subset,
                Sector = SubHeading)

tot_sector <- asc13 %>% dplyr::select(DH_GEOGRAPHY_NAME, SupportSetting, Expenditure)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, SupportSetting)%>%
  dplyr::summarise(Expenditure=sum(Expenditure, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(Sector="Total")

asc13 <- asc13%>%bind_rows(., tot_sector)%>%
  dplyr::mutate(year=2013)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting, year) %>%
  dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
  dplyr::ungroup()


#2014

asc14 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2014/Final_PSS_Expenditure_2013_14_Raw_Data.csv"))%>%
  dplyr::filter(MainHeading=="1. GROSS TOTAL COST (Current expenditure including capital charges)",
                Category=="B. OLDER PEOPLE (AGED 65 OR OVER) INCLUDING OLDER MENTALLY ILL"&Subset=="TOTAL OLDER PEOPLE excluding Supporting People"|
                  Category=="B. OLDER PEOPLE (AGED 65 OR OVER) INCLUDING OLDER MENTALLY ILL"&Subset=="Nursing care placements"|
                  Category=="B. OLDER PEOPLE (AGED 65 OR OVER) INCLUDING OLDER MENTALLY ILL"&Subset=="Residential care placements"|
                  Category=="B. OLDER PEOPLE (AGED 65 OR OVER) INCLUDING OLDER MENTALLY ILL"&Subset=="Direct Payments"|
                  Category=="B. OLDER PEOPLE (AGED 65 OR OVER) INCLUDING OLDER MENTALLY ILL"&Subset=="Supported and other accommodation"|
                  Category=="B. OLDER PEOPLE (AGED 65 OR OVER) INCLUDING OLDER MENTALLY ILL"&Subset=="Home care"|
                  Subset=="TOTAL ADULTS with PSD, Aged 18 to 64, excluding Supporting People"|
                  Subset=="TOTAL ADULTS WITH LD, Aged 18 to 64, excluding Supporting People"|
                  Subset=="TOTAL ADULTS WITH MH NEEDS, Aged 18 to 64, excluding Supporting People")%>%
  dplyr::select(Name, SubHeading, Subset, Value)%>%
  dplyr::mutate(year=2014,
                SubHeading= ifelse(SubHeading=="Own Provision", "In House",
                                   ifelse(SubHeading=="Provision by Others", "External",SubHeading)),
                Subset = ifelse(Subset=="Home care", "Home care",
                                ifelse(Subset=="Nursing care placements", "Nursing home placements",
                                       ifelse(Subset=="Residential care placements", "Residential care home placements",
                                              ifelse(Subset=="Direct Payments", "Direct payments",
                                                     ifelse(Subset=="Supported and other accommodation", "Supported and other accommodation",
                                                     ifelse(Subset=="TOTAL ADULTS with PSD, Aged 18 to 64, excluding Supporting People", "U65 PHYSICAL DISABILITY",
                                                            ifelse(Subset=="TOTAL ADULTS WITH LD, Aged 18 to 64, excluding Supporting People", "U65 LEARNING DISABILITY",
                                                                   ifelse(Subset=="TOTAL ADULTS WITH MH NEEDS, Aged 18 to 64, excluding Supporting People", "U65 MENTAL HEALTH",
                                                                          ifelse(Subset=="TOTAL OLDER PEOPLE excluding Supporting People", "Total over 65", NA))))))))))%>%
  dplyr::group_by(Name, SubHeading)%>%
  dplyr::mutate(Service = NA)%>%
  dplyr::ungroup()%>%
  dplyr::rename(Expenditure=Value,
                DH_GEOGRAPHY_NAME = Name,
                SupportSetting = Subset,
                Sector = SubHeading)

tot_sector <- asc14 %>% dplyr::select(DH_GEOGRAPHY_NAME, SupportSetting, Expenditure)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, SupportSetting)%>%
  dplyr::summarise(Expenditure=sum(Expenditure, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(Sector="Total")

asc14 <- asc14%>%bind_rows(., tot_sector)%>%
  dplyr::mutate(year=2014)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting, year) %>%
  dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
  dplyr::ungroup()


#2015
#add both long and short plz boss xx

asc15 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2015/pss-exp-eng-14-15-fin-coun-lev-exp-data.csv"),skip=1,
                  colClasses = "character")%>%
  dplyr::filter(grepl("Gross total cost:",Accounting.category),
                      Age.category=="Age 65 and over"&Summary=="Total"|
                      Age.category=="Age 18 to 64"&Delivery.mechanism=="Total")%>%
  dplyr::mutate(SupportSetting = ifelse(Age.category=="Age 65 and over"&Delivery.mechanism=="Nursing", "Nursing home placements",
                                        ifelse(Age.category=="Age 65 and over"&Delivery.mechanism=="Residential", "Residential care home placements",
                                               ifelse(Age.category=="Age 65 and over"&Delivery.mechanism=="Supported accommodation", "Supported and other accommodation",
                                                      ifelse(Age.category=="Age 65 and over"&Delivery.mechanism=="Community: home care", "Home care",
                                                             ifelse(Age.category=="Age 65 and over"&Delivery.mechanism=="Community: Direct Payments", "Direct payments",
                                                                    ifelse(Age.category=="Age 65 and over"&Delivery.mechanism=="Total", "Total over 65",
                                                                    ifelse(Age.category=="Age 18 to 64"&Summary=="Mental health support","U65 MENTAL HEALTH",
                                                                           ifelse(Age.category=="Age 18 to 64"&Summary=="Learning disability support","U65 LEARNING DISABILITY",
                                                                                  ifelse(Age.category=="Age 18 to 64"&Summary=="Physical support","U65 PHYSICAL DISABILITY",
                                                                                         ifelse(Age.category=="Age 18 to 64"&Summary=="Sensory support","U65 PHYSICAL DISABILITY",
                                                                                                ifelse(Age.category=="Age 18 to 64"&Summary=="Total","U65 TOTAL",
                                                                                                NA))))))))))),
                Sector = ifelse(Accounting.category=="Gross total cost: own provision", "In House",
                                ifelse(Accounting.category=="Gross total cost: provision by others", "External",
                                       ifelse(Accounting.category=="Gross total cost: total expenditure", "Total", NA))),
                year=2015)%>%
  dplyr::select(-Accounting.category, -Age.category, -Support.type, -Summary, -Delivery.mechanism)%>%
  tidyr::pivot_longer( cols=!c(SupportSetting, Sector, year), 
                     names_to = "DH_GEOGRAPHY_NAME", values_to = "Expenditure", names_prefix = ".*\\.{3}")%>%
  dplyr::mutate(Expenditure = as.numeric(Expenditure),
                Service=NA)%>%
  dplyr::filter(!is.na(SupportSetting),
                !is.na(Sector))%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting, Sector, year, Service) %>%
  dplyr::summarise(Expenditure = sum(Expenditure,na.rm=T)) %>%
  dplyr::ungroup()%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
  dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
  dplyr::ungroup()
  
#2016



asc16 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2016/pss-exp-eng-15-16-fin-exp.csv"))%>%
  dplyr::filter(Finance.Type=="Expenditure")

eight <- asc16%>%dplyr::filter(Age.Band=="Age 18 to 64")%>%
  dplyr::select(Council.Name,Finance.Description,Primary.support.reason, Value)%>%
  dplyr::mutate(Value=as.numeric(Value))%>%
  dplyr::group_by(Council.Name,Finance.Description,Primary.support.reason)%>%
  dplyr::summarise(Value=sum(Value, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(Value=ifelse(Value<0,0,Value),
                SupportSetting= ifelse(Primary.support.reason=="Learning disability support", "U65 LEARNING DISABILITY",
                                       ifelse(Primary.support.reason=="Physical support", "U65 PHYSICAL DISABILITY",
                                              ifelse(Primary.support.reason=="Mental health support", "U65 MENTAL HEALTH",
                                                     NA))),
                Finance.Description = ifelse(Finance.Description=="Own Provision", "In House",
                                             ifelse(Finance.Description=="Provision by Others", "External", NA)))%>%
  dplyr::rename(Sector=Finance.Description,
                DH_GEOGRAPHY_NAME = Council.Name)%>%
  dplyr::select(-Primary.support.reason)
  

eightot <- eight %>%
  dplyr::select(-Sector)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, SupportSetting)%>%
  dplyr::summarise(Value=sum(Value, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(Sector="Total")%>%
  dplyr::bind_rows(., eight)%>%
  dplyr::filter(!is.na(Sector),
                !is.na(SupportSetting))%>%
  dplyr::mutate(year=2016,
                Service=NA)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
  dplyr::rename(Expenditure=Value)%>%
  dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
  dplyr::ungroup()

six <- asc16%>%dplyr::filter(Age.Band=="Age 65 and over")%>%
  dplyr::select(Council.Name,Finance.Description,Long.Term.Support.Setting, Value)%>%
  dplyr::mutate(Value=as.numeric(Value))%>%
  dplyr::group_by(Council.Name,Finance.Description,Long.Term.Support.Setting)%>%
  dplyr::summarise(Value=sum(Value, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(Value=ifelse(Value<0,0,Value),
                SupportSetting= ifelse(Long.Term.Support.Setting=="Community: home care", "home care",
                                       ifelse(Long.Term.Support.Setting=="Community: supported living", "Supported and other accommodation",
                                              ifelse(Long.Term.Support.Setting=="Residential", "Residential care home placements",
                                                     ifelse(Long.Term.Support.Setting=="Community: Direct Payments", "Direct payments",
                                                     ifelse(Long.Term.Support.Setting=="Nursing", "Nursing home placements",
                                                            NA))))),
                Finance.Description = ifelse(Finance.Description=="Own Provision", "In House",
                                             ifelse(Finance.Description=="Provision by Others", "External", NA)))%>%
  dplyr::rename(Sector=Finance.Description,
                DH_GEOGRAPHY_NAME = Council.Name)%>%
  dplyr::select(-Long.Term.Support.Setting)

#ugh lazy, replaced minus values with zero


six <- six %>%
  dplyr::select(-SupportSetting)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, Sector)%>%
  dplyr::summarise(Value=sum(Value, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(SupportSetting="Total over 65")%>%
  dplyr::bind_rows(., six)
  


  
sixtot <- six %>%
  dplyr::select(-Sector)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, SupportSetting)%>%
  dplyr::summarise(Value=sum(Value, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(Sector="Total")%>%
  dplyr::bind_rows(., six)%>%
  dplyr::filter(!is.na(Sector),
                !is.na(SupportSetting))%>%
  dplyr::mutate(year=2016,
                Service=NA)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
  dplyr::rename(Expenditure=Value)%>%
  dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
  dplyr::ungroup()

asc16 <- rbind(sixtot, eightot)


#2017

asc17 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2017/CSV%20ASCFR%20Expenditure.csv"))%>%
  dplyr::filter(FinanceType_KEY==3)

eight <- asc17%>%dplyr::filter(AgeBand_KEY==1)%>%
  dplyr::select(CASSR_CODE,FinanceDesc_KEY,PrimarySupportReason_KEY, ItemValue)%>%
  dplyr::mutate(ItemValue=as.numeric(ItemValue))%>%
  dplyr::group_by(CASSR_CODE,FinanceDesc_KEY,PrimarySupportReason_KEY)%>%
  dplyr::summarise(ItemValue=sum(ItemValue, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(ItemValue=ifelse(ItemValue<0,0,ItemValue),
                SupportSetting= ifelse(PrimarySupportReason_KEY==4, "U65 LEARNING DISABILITY",
                                       ifelse(PrimarySupportReason_KEY==6, "U65 PHYSICAL DISABILITY",
                                              ifelse(PrimarySupportReason_KEY==5, "U65 MENTAL HEALTH",
                                                     NA))),
                FinanceDesc_KEY = ifelse(FinanceDesc_KEY==7, "In House",
                                             ifelse(FinanceDesc_KEY==8, "External", NA)))%>%
  dplyr::rename(Sector=FinanceDesc_KEY,
                DH_GEOGRAPHY_NAME = CASSR_CODE)%>%
  dplyr::select(-PrimarySupportReason_KEY)

eightot <- eight %>%
  dplyr::select(-Sector)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, SupportSetting)%>%
  dplyr::summarise(ItemValue=sum(ItemValue, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(Sector="Total")%>%
  dplyr::bind_rows(., eight)%>%
  dplyr::filter(!is.na(Sector),
                !is.na(SupportSetting))%>%
  dplyr::mutate(year=2017,
                Service=NA)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
  dplyr::rename(Expenditure=ItemValue)%>%
  dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
  dplyr::ungroup()


six <- asc17%>%dplyr::filter(AgeBand_KEY==2)%>%
  dplyr::select(CASSR_CODE,FinanceDesc_KEY,SupportSetting_KEY, ItemValue)%>%
  dplyr::mutate(ItemValue=as.numeric(ItemValue))%>%
  dplyr::group_by(CASSR_CODE,FinanceDesc_KEY,SupportSetting_KEY)%>%
  dplyr::summarise(ItemValue=sum(ItemValue, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(ItemValue=ifelse(ItemValue<0,0,ItemValue),
                SupportSetting= ifelse(SupportSetting_KEY==2, "home care",
                                       ifelse(SupportSetting_KEY==4, "Supported and other accommodation",
                                              ifelse(SupportSetting_KEY==1, "Direct payments",
                                                     ifelse(SupportSetting_KEY==7, "Residential care home placements",
                                                     ifelse(SupportSetting_KEY==6, "Nursing home placements",
                                                            NA))))),
                FinanceDesc_KEY = ifelse(FinanceDesc_KEY==7, "In House",
                                             ifelse(FinanceDesc_KEY==8, "External", NA)))%>%
  dplyr::rename(Sector=FinanceDesc_KEY,
                DH_GEOGRAPHY_NAME = CASSR_CODE)%>%
  dplyr::select(-SupportSetting_KEY)

six <- six %>%
  dplyr::select(-SupportSetting)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, Sector)%>%
  dplyr::summarise(ItemValue=sum(ItemValue, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(SupportSetting="Total over 65")%>%
  dplyr::bind_rows(., six)
#ugh lazy, replaced minus values with zero

sixtot <- six %>%
  dplyr::select(-Sector)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, SupportSetting)%>%
  dplyr::summarise(ItemValue=sum(ItemValue, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(Sector="Total")%>%
  dplyr::bind_rows(., six)%>%
  dplyr::filter(!is.na(Sector),
                !is.na(SupportSetting))%>%
  dplyr::mutate(year=2017,
                Service=NA)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
  dplyr::rename(Expenditure=ItemValue)%>%
  dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
  dplyr::ungroup()

asc17 <- rbind(sixtot, eightot)


#2018

asc18 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/Copy%20of%20ASCFR%20Data%20File%20(with%20descriptions)_2018.csv"))%>%
  dplyr::filter(FinanceType_Key=="Expenditure")

eight <- asc18%>%dplyr::filter(AgeBand_Key=="18 to 64", SupportSetting_Key=="99")%>%
  dplyr::select(DH_GEOGRAPHY_NAME,FinanceDescription_Key,PrimarySupportReason_Key, ITEMVALUE)%>%
  dplyr::mutate(ITEMVALUE=as.numeric(ITEMVALUE))%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,FinanceDescription_Key,PrimarySupportReason_Key)%>%
  dplyr::summarise(ITEMVALUE=sum(ITEMVALUE, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(ITEMVALUE=ifelse(ITEMVALUE<0,0,ITEMVALUE),
                SupportSetting= ifelse(PrimarySupportReason_Key=="Learning Disability Support", "U65 LEARNING DISABILITY",
                                       ifelse(PrimarySupportReason_Key=="Physical Support", "U65 PHYSICAL DISABILITY",
                                              ifelse(PrimarySupportReason_Key=="Mental Health Support", "U65 MENTAL HEALTH",
                                                     ifelse(PrimarySupportReason_Key=="99", "U65 TOTAL",
                                                     NA)))),
                FinanceDescription_Key = ifelse(FinanceDescription_Key=="Own Provision", "In House",
                                         ifelse(FinanceDescription_Key=="Provision by Others", "External",
                                                ifelse(FinanceDescription_Key=="99", "Total", NA))))%>%
  dplyr::rename(Sector=FinanceDescription_Key)%>%
  dplyr::select(-PrimarySupportReason_Key)%>%
  dplyr::filter(!is.na(Sector),
                !is.na(SupportSetting))%>%
  dplyr::mutate(year=2018,
                Service=NA)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
  dplyr::rename(Expenditure=ITEMVALUE)%>%
  dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
  dplyr::ungroup()




six <- asc18%>%dplyr::filter(AgeBand_Key=="65 and Over", PrimarySupportReason_Key=="99")%>%
  dplyr::select(DH_GEOGRAPHY_NAME,FinanceDescription_Key,SupportSetting_Key, ITEMVALUE)%>%
  dplyr::mutate(ITEMVALUE=as.numeric(ITEMVALUE))%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,FinanceDescription_Key,SupportSetting_Key)%>%
  dplyr::summarise(ITEMVALUE=sum(ITEMVALUE, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(ITEMVALUE=ifelse(ITEMVALUE<0,0,ITEMVALUE),
                SupportSetting= ifelse(SupportSetting_Key=="Community: Home Care", "home care",
                                        ifelse(SupportSetting_Key=="Supported Accommodation", "Supported and other accommodation",
                                              ifelse(SupportSetting_Key=="Residential", "Residential care home placements",
                                                     ifelse(SupportSetting_Key=="Community: Direct Payments", "Direct payments",
                                                            ifelse(SupportSetting_Key=="Nursing", "Nursing home placements",
                                                                   ifelse(SupportSetting_Key=="99", "Total over 65",
                                                                          NA)))))),
                FinanceDescription_Key = ifelse(FinanceDescription_Key=="Own Provision", "In House",
                                                ifelse(FinanceDescription_Key=="Provision by Others", "External",
                                                       ifelse(FinanceDescription_Key=="99", "Total", NA))))%>%
  dplyr::rename(Sector=FinanceDescription_Key)%>%
  dplyr::select(-SupportSetting_Key)%>%
  dplyr::filter(!is.na(Sector),
                !is.na(SupportSetting))%>%
  dplyr::mutate(year=2018,
                Service=NA)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
  dplyr::rename(Expenditure=ITEMVALUE)%>%
  dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
  dplyr::ungroup()

#ugh lazy, replaced minus values with zero


asc18 <- rbind(six, eight)




#2019

asc19 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/ASC-FR%20Data%20File%20(descriptions)%20v2_2019.csv"))%>%
  dplyr::filter(FinanceType_Key=="Expenditure")

eight <- asc19%>%dplyr::filter(AgeBand_Key=="18 to 64", SupportSetting_Key=="99")%>%
  dplyr::select(DH_GEOGRAPHY_NAME,FinanceDescription_Key,PrimarySupportReason_Key, ITEMVALUE)%>%
  dplyr::mutate(ITEMVALUE=as.numeric(ITEMVALUE))%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,FinanceDescription_Key,PrimarySupportReason_Key)%>%
  dplyr::summarise(ITEMVALUE=sum(ITEMVALUE, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(ITEMVALUE=ifelse(ITEMVALUE<0,0,ITEMVALUE),
                SupportSetting= ifelse(PrimarySupportReason_Key=="Learning Disability Support", "U65 LEARNING DISABILITY",
                                       ifelse(PrimarySupportReason_Key=="Physical Support", "U65 PHYSICAL DISABILITY",
                                              ifelse(PrimarySupportReason_Key=="99", "U65 TOTAL",
                                                     ifelse(PrimarySupportReason_Key=="Mental Health Support", "U65 MENTAL HEALTH",
                                                     NA)))),
                FinanceDescription_Key = ifelse(FinanceDescription_Key=="Own Provision", "In House",
                                                ifelse(FinanceDescription_Key=="Provision by Others", "External",
                                                       ifelse(FinanceDescription_Key=="99", "Total", NA))))%>%
  dplyr::rename(Sector=FinanceDescription_Key)%>%
  dplyr::select(-PrimarySupportReason_Key)%>%
  dplyr::filter(!is.na(Sector),
                !is.na(SupportSetting))%>%
  dplyr::mutate(year=2019,
                Service=NA)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
  dplyr::rename(Expenditure=ITEMVALUE)%>%
  dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
  dplyr::ungroup()




six <- asc19%>%dplyr::filter(AgeBand_Key=="65 and Over", PrimarySupportReason_Key=="99")%>%
  dplyr::select(DH_GEOGRAPHY_NAME,FinanceDescription_Key,SupportSetting_Key, ITEMVALUE)%>%
  dplyr::mutate(ITEMVALUE=as.numeric(ITEMVALUE))%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,FinanceDescription_Key,SupportSetting_Key)%>%
  dplyr::summarise(ITEMVALUE=sum(ITEMVALUE, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(ITEMVALUE=ifelse(ITEMVALUE<0,0,ITEMVALUE),
                SupportSetting= ifelse(SupportSetting_Key=="Community: Home Care", "home care",
                                       ifelse(SupportSetting_Key=="Supported Accommodation", "Supported and other accommodation",
                                              ifelse(SupportSetting_Key=="Residential", "Residential care home placements",
                                                     ifelse(SupportSetting_Key=="Community: Direct Payments", "Direct payments",
                                                            ifelse(SupportSetting_Key=="Nursing", "Nursing home placements",
                                                                   ifelse(SupportSetting_Key=="99", "Total over 65",
                                                                          NA)))))),
                FinanceDescription_Key = ifelse(FinanceDescription_Key=="Own Provision", "In House",
                                                ifelse(FinanceDescription_Key=="Provision by Others", "External",
                                                       ifelse(FinanceDescription_Key=="99", "Total", NA))))%>%
  dplyr::rename(Sector=FinanceDescription_Key)%>%
  dplyr::select(-SupportSetting_Key)%>%
  dplyr::filter(!is.na(Sector),
                !is.na(SupportSetting))%>%
  dplyr::mutate(year=2019,
                Service=NA)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
  dplyr::rename(Expenditure=ITEMVALUE)%>%
  dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
  dplyr::ungroup()

#ugh lazy, replaced minus values with zero


asc19 <- rbind(six, eight)




#2020

asc20 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/ASC-FR%20Data%20File%20(descriptions)%20v2_2020.csv"))%>%
  dplyr::filter(FinanceType=="Expenditure")

eight <- asc20%>%dplyr::filter(AgeBand=="18 to 64", SupportSetting=="99")%>%
  dplyr::select(DH_GEOGRAPHY_NAME,FinanceDescription,PrimarySupportReason, ITEMVALUE)%>%
  dplyr::mutate(ITEMVALUE=as.numeric(ITEMVALUE))%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,FinanceDescription,PrimarySupportReason)%>%
  dplyr::summarise(ITEMVALUE=sum(ITEMVALUE, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(ITEMVALUE=ifelse(ITEMVALUE<0,0,ITEMVALUE),
                SupportSetting= ifelse(PrimarySupportReason=="Learning Disability Support", "U65 LEARNING DISABILITY",
                                       ifelse(PrimarySupportReason=="Physical Support", "U65 PHYSICAL DISABILITY",
                                              ifelse(PrimarySupportReason=="Mental Health Support", "U65 MENTAL HEALTH",
                                                     ifelse(PrimarySupportReason=="99", "U65 TOTAL",
                                                            NA)))),
                FinanceDescription = ifelse(FinanceDescription=="Own Provision", "In House",
                                                ifelse(FinanceDescription=="Provision by Others", "External",
                                                       ifelse(FinanceDescription=="99", "Total", NA))))%>%
  dplyr::rename(Sector=FinanceDescription)%>%
  dplyr::select(-PrimarySupportReason)%>%
  dplyr::filter(!is.na(Sector),
                !is.na(SupportSetting))%>%
  dplyr::mutate(year=2020,
                Service=NA)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
  dplyr::rename(Expenditure=ITEMVALUE)%>%
  dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
  dplyr::ungroup()




six <- asc20%>%dplyr::filter(AgeBand=="65 and Over", PrimarySupportReason=="99")%>%
  dplyr::select(DH_GEOGRAPHY_NAME,FinanceDescription,SupportSetting, ITEMVALUE)%>%
  dplyr::mutate(ITEMVALUE=as.numeric(ITEMVALUE))%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,FinanceDescription,SupportSetting)%>%
  dplyr::summarise(ITEMVALUE=sum(ITEMVALUE, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(ITEMVALUE=ifelse(ITEMVALUE<0,0,ITEMVALUE),
                SupportSetting= ifelse(SupportSetting=="Community: Home Care", "home care",
                                       ifelse(SupportSetting=="Supported Accommodation", "Supported and other accommodation",
                                              ifelse(SupportSetting=="Community: Direct Payments", "Direct payments",
                                                     ifelse(SupportSetting=="Residential", "Residential care home placements",
                                                     ifelse(SupportSetting=="Nursing", "Nursing home placements",
                                                            ifelse(SupportSetting=="99", "Total over 65",
                                                                   NA)))))),
                FinanceDescription = ifelse(FinanceDescription=="Own Provision", "In House",
                                                ifelse(FinanceDescription=="Provision by Others", "External",
                                                       ifelse(FinanceDescription=="99", "Total", NA))))%>%
  dplyr::rename(Sector=FinanceDescription)%>%
  dplyr::filter(!is.na(Sector),
                !is.na(SupportSetting))%>%
  dplyr::mutate(year=2020,
                Service=NA)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
  dplyr::rename(Expenditure=ITEMVALUE)%>%
  dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
  dplyr::ungroup()

#ugh lazy, replaced minus values with zero


asc20 <- rbind(six, eight)





#2021

asc21 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/ASC-FR%20Data%20File%20(descriptions)%20v2_2021.csv"))%>%
  dplyr::filter(FinanceType=="Expenditure")

eight <- asc21%>%dplyr::filter(AgeBand=="18 to 64", SupportSetting=="99")%>%
  dplyr::select(DH_GEOGRAPHY_NAME,FinanceDescription,PrimarySupportReason, ITEMVALUE)%>%
  dplyr::mutate(ITEMVALUE=as.numeric(ITEMVALUE))%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,FinanceDescription,PrimarySupportReason)%>%
  dplyr::summarise(ITEMVALUE=sum(ITEMVALUE, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(ITEMVALUE=ifelse(ITEMVALUE<0,0,ITEMVALUE),
                SupportSetting= ifelse(PrimarySupportReason=="Learning Disability Support", "U65 LEARNING DISABILITY",
                                       ifelse(PrimarySupportReason=="Physical Support", "U65 PHYSICAL DISABILITY",
                                              ifelse(PrimarySupportReason=="Mental Health Support", "U65 MENTAL HEALTH",
                                                     ifelse(PrimarySupportReason=="99", "U65 TOTAL",
                                                            NA)))),
                FinanceDescription = ifelse(FinanceDescription=="Own Provision", "In House",
                                            ifelse(FinanceDescription=="Provision by Others", "External",
                                                   ifelse(FinanceDescription=="99", "Total", NA))))%>%
  dplyr::rename(Sector=FinanceDescription)%>%
  dplyr::select(-PrimarySupportReason)%>%
  dplyr::filter(!is.na(Sector),
                !is.na(SupportSetting))%>%
  dplyr::mutate(year=2021,
                Service=NA)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
  dplyr::rename(Expenditure=ITEMVALUE)%>%
  dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
  dplyr::ungroup()




six <- asc21%>%dplyr::filter(AgeBand=="65 and Over", PrimarySupportReason=="99")%>%
  dplyr::select(DH_GEOGRAPHY_NAME,FinanceDescription,SupportSetting, ITEMVALUE)%>%
  dplyr::mutate(ITEMVALUE=as.numeric(ITEMVALUE))%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,FinanceDescription,SupportSetting)%>%
  dplyr::summarise(ITEMVALUE=sum(ITEMVALUE, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(ITEMVALUE=ifelse(ITEMVALUE<0,0,ITEMVALUE),
                SupportSetting= ifelse(SupportSetting=="Community: Home Care", "home care",
                                       ifelse(SupportSetting=="Community: Direct Payments", "Direct payments",
                                              ifelse(SupportSetting=="Supported Accommodation", "Supported and other accommodation",
                                              ifelse(SupportSetting=="Residential", "Residential care home placements",
                                                     ifelse(SupportSetting=="Nursing", "Nursing home placements",
                                                            ifelse(SupportSetting=="99", "Total over 65",
                                                                   NA)))))),
                FinanceDescription = ifelse(FinanceDescription=="Own Provision", "In House",
                                            ifelse(FinanceDescription=="Provision by Others", "External",
                                                   ifelse(FinanceDescription=="99", "Total", NA))))%>%
  dplyr::rename(Sector=FinanceDescription)%>%
  dplyr::filter(!is.na(Sector),
                !is.na(SupportSetting))%>%
  dplyr::mutate(year=2021,
                Service=NA)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
  dplyr::rename(Expenditure=ITEMVALUE)%>%
  dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
  dplyr::ungroup()

#ugh lazy, replaced minus values with zero


asc21 <- rbind(six, eight)



#2022


asc22 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/ASC-FR%20Data%20File%20(descriptions)%20v2_2022.csv"))%>%
  dplyr::filter(FinanceType=="Expenditure")

eight <- asc22%>%dplyr::filter(AgeBand=="18 to 64", SupportSetting=="99")%>%
  dplyr::select(DH_GEOGRAPHY_NAME,FinanceDescription,PrimarySupportReason, ITEMVALUE)%>%
  dplyr::mutate(ITEMVALUE=as.numeric(ITEMVALUE))%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,FinanceDescription,PrimarySupportReason)%>%
  dplyr::summarise(ITEMVALUE=sum(ITEMVALUE, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(ITEMVALUE=ifelse(ITEMVALUE<0,0,ITEMVALUE),
                SupportSetting= ifelse(PrimarySupportReason=="Learning Disability Support", "U65 LEARNING DISABILITY",
                                       ifelse(PrimarySupportReason=="Physical Support", "U65 PHYSICAL DISABILITY",
                                              ifelse(PrimarySupportReason=="Mental Health Support", "U65 MENTAL HEALTH",
                                                     ifelse(PrimarySupportReason=="99", "U65 TOTAL",
                                                            NA)))),
                FinanceDescription = ifelse(FinanceDescription=="Own Provision", "In House",
                                            ifelse(FinanceDescription=="Provision by Others", "External",
                                                   ifelse(FinanceDescription=="99", "Total", NA))))%>%
  dplyr::rename(Sector=FinanceDescription)%>%
  dplyr::select(-PrimarySupportReason)%>%
  dplyr::filter(!is.na(Sector),
                !is.na(SupportSetting))%>%
  dplyr::mutate(year=2022,
                Service=NA)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
  dplyr::rename(Expenditure=ITEMVALUE)%>%
  dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
  dplyr::ungroup()




six <- asc22%>%dplyr::filter(AgeBand=="65 and Over", PrimarySupportReason=="99")%>%
  dplyr::select(DH_GEOGRAPHY_NAME,FinanceDescription,SupportSetting, ITEMVALUE)%>%
  dplyr::mutate(ITEMVALUE=as.numeric(ITEMVALUE))%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,FinanceDescription,SupportSetting)%>%
  dplyr::summarise(ITEMVALUE=sum(ITEMVALUE, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(ITEMVALUE=ifelse(ITEMVALUE<0,0,ITEMVALUE),
                SupportSetting= ifelse(SupportSetting=="Community: Home Care", "home care",
                                       ifelse(SupportSetting=="Supported Accommodation", "Supported and other accommodation",
                                              ifelse(SupportSetting=="Community: Direct Payments", "Direct payments",
                                                     ifelse(SupportSetting=="Residential", "Residential care home placements",
                                                     ifelse(SupportSetting=="Nursing", "Nursing home placements",
                                                            ifelse(SupportSetting=="99", "Total over 65",
                                                                   NA)))))),
                FinanceDescription = ifelse(FinanceDescription=="Own Provision", "In House",
                                            ifelse(FinanceDescription=="Provision by Others", "External",
                                                   ifelse(FinanceDescription=="99", "Total", NA))))%>%
  dplyr::rename(Sector=FinanceDescription)%>%
  dplyr::filter(!is.na(Sector),
                !is.na(SupportSetting))%>%
  dplyr::mutate(year=2022,
                Service=NA)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
  dplyr::rename(Expenditure=ITEMVALUE)%>%
  dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
  dplyr::ungroup()

#ugh lazy, replaced minus values with zero


asc22 <- rbind(six, eight)

#2023


asc23 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/ASC-FR%20Data%20File%20(descriptions)%20v2_2023.csv"))%>%
  dplyr::filter(FinanceType=="Expenditure")

eight <- asc23%>%dplyr::filter(AgeBand=="18 to 64", SupportSetting=="99")%>%
  dplyr::select(DH_GEOGRAPHY_NAME,FinanceDescription,PrimarySupportReason, ITEMVALUE)%>%
  dplyr::mutate(ITEMVALUE=as.numeric(ITEMVALUE))%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,FinanceDescription,PrimarySupportReason)%>%
  dplyr::summarise(ITEMVALUE=sum(ITEMVALUE, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(ITEMVALUE=ifelse(ITEMVALUE<0,0,ITEMVALUE),
                SupportSetting= ifelse(PrimarySupportReason=="Learning Disability Support", "U65 LEARNING DISABILITY",
                                       ifelse(PrimarySupportReason=="Physical Support", "U65 PHYSICAL DISABILITY",
                                              ifelse(PrimarySupportReason=="Mental Health Support", "U65 MENTAL HEALTH",
                                                     ifelse(PrimarySupportReason=="99", "U65 TOTAL",
                                                            NA)))),
                FinanceDescription = ifelse(FinanceDescription=="Own Provision", "In House",
                                            ifelse(FinanceDescription=="Provision by Others", "External",
                                                   ifelse(FinanceDescription=="99", "Total", NA))))%>%
  dplyr::rename(Sector=FinanceDescription)%>%
  dplyr::select(-PrimarySupportReason)%>%
  dplyr::filter(!is.na(Sector),
                !is.na(SupportSetting))%>%
  dplyr::mutate(year=2023,
                Service=NA)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
  dplyr::rename(Expenditure=ITEMVALUE)%>%
  dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
  dplyr::ungroup()




six <- asc23%>%dplyr::filter(AgeBand=="65 and Over", PrimarySupportReason=="99")%>%
  dplyr::select(DH_GEOGRAPHY_NAME,FinanceDescription,SupportSetting, ITEMVALUE)%>%
  dplyr::mutate(ITEMVALUE=as.numeric(ITEMVALUE))%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,FinanceDescription,SupportSetting)%>%
  dplyr::summarise(ITEMVALUE=sum(ITEMVALUE, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(ITEMVALUE=ifelse(ITEMVALUE<0,0,ITEMVALUE),
                SupportSetting= ifelse(SupportSetting=="Community: Home Care", "home care",
                                       ifelse(SupportSetting=="Supported Accommodation", "Supported and other accommodation",
                                              ifelse(SupportSetting=="Community: Direct Payments", "Direct payments",
                                                     ifelse(SupportSetting=="Residential", "Residential care home placements",
                                                     ifelse(SupportSetting=="Nursing", "Nursing home placements",
                                                            ifelse(SupportSetting=="99", "Total over 65",
                                                                   NA)))))),
                FinanceDescription = ifelse(FinanceDescription=="Own Provision", "In House",
                                            ifelse(FinanceDescription=="Provision by Others", "External",
                                                   ifelse(FinanceDescription=="99", "Total", NA))))%>%
  dplyr::rename(Sector=FinanceDescription)%>%
  dplyr::filter(!is.na(Sector),
                !is.na(SupportSetting))%>%
  dplyr::mutate(year=2023,
                Service=NA)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME,SupportSetting) %>%
  dplyr::rename(Expenditure=ITEMVALUE)%>%
  dplyr::mutate(percent_sector = Expenditure /Expenditure[Sector == "Total"]*100) %>%
  dplyr::ungroup()

#ugh lazy, replaced minus values with zero


asc23 <- rbind(six, eight)



fulldata <- rbind(asc01,asc02,asc03, asc04, asc05, asc06, asc07, asc08, asc09, asc10,asc11, asc12,
                  asc13, asc14, asc15, asc16,asc17, asc18, asc19, asc20, asc21, asc22, asc23)%>%
  dplyr::mutate(DH_GEOGRAPHY_NAME = DH_GEOGRAPHY_NAME %>%
                  gsub('%20', " ",.)%>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  #gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  gsub(" THE", "",.)%>%
                  gsub("BEDFORD BOROUGH", "BEDFORD",.)%>%
                  str_trim())%>%
  dplyr::filter(!grepl("TOTAL", DH_GEOGRAPHY_NAME),
                DH_GEOGRAPHY_NAME!="ENGLAND",
                DH_GEOGRAPHY_NAME!="")

write.csv(fulldata, "C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/adults_social_care_data/expenditure.csv")

sum(fulldata[fulldata$Sector=="Total"&fulldata$SupportSetting=="Total over 65",]$Expenditure, na.rm=T)

fulldata %>% dplyr::filter(grepl("HERTFORDSHIRE", DH_GEOGRAPHY_NAME),
                           Sector=="External",
                           SupportSetting=="Total over 65")%>%
  dplyr::select(Expenditure)%>%
  sum(.,na.rm=T)
fulldata %>% dplyr::filter(grepl("HERTFORDSHIRE", DH_GEOGRAPHY_NAME),
                           Sector=="Total",
                           SupportSetting=="Total over 65")%>%
  dplyr::select(Expenditure)%>%
  sum(.,na.rm=T)

fulldata %>% dplyr::filter(grepl("CUMBRIA", DH_GEOGRAPHY_NAME),
                           Sector=="External",
                           SupportSetting=="Total over 65")%>%
  dplyr::select(Expenditure)%>%
  sum(.,na.rm=T)
fulldata %>% dplyr::filter(grepl("CUMBRIA", DH_GEOGRAPHY_NAME),
                           Sector=="Total",
                           SupportSetting=="Total over 65")%>%
  dplyr::select(Expenditure)%>%
  sum(.,na.rm=T)


fulldata <- read.csv("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/adults_social_care_data/expenditure.csv")
 ####plotfun####
 
 plotfun <-fulldata %>%
   dplyr::mutate(SupportSetting = tolower(SupportSetting))%>%
     dplyr::filter(SupportSetting!="supported and other accommodation",
                   SupportSetting!="nursing home placements", 
                   SupportSetting!="direct payments",
                   SupportSetting!="total under 65",
                   SupportSetting!="u65 total")%>%
  dplyr::mutate(SupportSetting= ifelse(SupportSetting=="home care", "Home Care",
                                       ifelse(SupportSetting=="residential care home placements", "Residential Care",
                                              ifelse(SupportSetting=="total over 65", "Total",
                                                     ifelse(SupportSetting=="u65 learning disability", "Learning Disability Support",
                                                            ifelse(SupportSetting=="u65 physical disability", "Physical Disability Support",
                                                                   ifelse(SupportSetting=="u65 mental health", "Mental Health Support",SupportSetting)))))))

plot1 <- plotfun %>%dplyr::filter(Sector=="In House",
                                  SupportSetting=="Total"|
                                    SupportSetting=="Home Care"|
                                    SupportSetting=="Residential Care")%>%
  ggplot(., aes(x = year, y = percent_sector)) +
  geom_point(size = 2, color = "#B4CFEE", alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE, colour = "#2A6EBB") +
  labs(
    x = "Year",
    y = "Expenditure on Public Provision (%)",
    title = "Aged 65 and over",
    color = ""
  )+
  theme_bw()+
  facet_wrap(~SupportSetting,nrow = 1)+
  theme(text = element_text(size=20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.length=unit(.28, "cm"),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=24),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=20),
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", size = 1),
        legend.text = element_text(size=20),
        legend.position = "top",
        strip.background = element_rect(fill="gray90", colour="black", size=1),
        strip.text = element_text(face="bold", size=16),
        title=element_text(face="bold")) +
  theme(panel.spacing.x = unit(4, "mm"))+
  geom_vline(xintercept=c(2014.5), linetype="dashed", colour="lightgrey", size=2)


plot2 <- plotfun %>%dplyr::filter(Sector=="In House",
                                  SupportSetting=="Learning Disability Support")%>%
  ggplot(., aes(x = year, y = percent_sector)) +
  geom_point(size = 2, color = "#B4CFEE", alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE, colour = "#2A6EBB") +
  labs(
    x = "Year",
    y = "Expenditure on Public Provision (%)",
    title = "Aged 18 to 65",
    color = ""
  )+
  theme_bw()+
  facet_wrap(~SupportSetting,nrow = 1)+
  theme(text = element_text(size=20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.length=unit(.28, "cm"),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=24),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=20),
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", size = 1),
        legend.text = element_text(size=20),
        legend.position = "top",
        strip.background = element_rect(fill="gray90", colour="black", size=1),
        strip.text = element_text(face="bold", size=16),
        title=element_text(face="bold")) +
  theme(panel.spacing.x = unit(4, "mm"))

 


plot <- cowplot::plot_grid(plot1, plot2, labels = c("A", "B"), ncol=1)

ggsave(plot=plot1, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/adults_social_care_data/fig1fin.png", width=20, height=8, dpi=600)
 
 plotfun <- fulldata %>%
   dplyr::mutate(SupportSetting = tolower(SupportSetting))%>%
   dplyr::filter(Sector=="Total",
                 DH_GEOGRAPHY_NAME!="",
                 year!=2009)%>%
   dplyr::select(SupportSetting,DH_GEOGRAPHY_NAME,year,Expenditure )%>%
   dplyr::group_by(DH_GEOGRAPHY_NAME, year) %>%
   dplyr::mutate(percent_total = Expenditure /Expenditure[SupportSetting=="total over 65"]*100) %>%
   dplyr::ungroup()%>%
   dplyr::filter(SupportSetting=="direct payments")
   
     ggplot(plotfun[plotfun$SupportSetting=="direct payments",], aes(x = year, y = percent_total, color = percent_total)) +
     geom_point(size = 3) +
     geom_smooth(method = "loess", se = FALSE) +
     labs(
       x = "Year",
       y = "Total spend (%)",
       title = "Percent of expenditure going to Direct Payments",
       color = "Outsourced %"
     )+
     theme_bw()+
     facet_wrap(~SupportSetting,nrow = 2)
 
 
 
 ###fig2 
     
     
     
fig2 <- read.csv("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/Children's Care Homes Project/Data/Ben Report Dates.csv")%>%
  dplyr::select(inspectiondate, overall, ownership)%>%
  dplyr::mutate(inspectiondate = as.Date(inspectiondate, format =  "%d%b%Y"))%>%
  dplyr::mutate(good = ifelse(overall=="Good", 1,
                              ifelse(overall=="Outstanding",1,
                                     ifelse(overall=="Requires improvement", 0,
                                            ifelse(overall=="Inadequate", 0,NA)))))%>%
  dplyr::filter(!is.na(good),
                !ownership=="")%>%
  dplyr::arrange(inspectiondate) %>%
  dplyr::mutate(all=1)%>%
  dplyr::arrange(inspectiondate) %>%
  dplyr::group_by(ownership, inspectiondate) %>%
  dplyr::summarise(
            good = sum(good),
            all = sum(all)) %>%
  dplyr::ungroup() %>%
  complete(ownership, inspectiondate = seq.Date(min(.$inspectiondate), max(.$inspectiondate), by = "day")) %>%
  dplyr::group_by(ownership) %>%
  dplyr::arrange(inspectiondate) %>%
  dplyr::mutate(rolling_good = zoo::rollapplyr(good, width = 365, FUN = sum, fill = NA, align = "right", na.rm=T)) %>%
  dplyr::mutate(rolling_all = zoo::rollapplyr(all, width = 365, FUN = sum, fill = NA, align = "right", na.rm=T)) %>%
  dplyr::mutate(rolling_good_ratio = rolling_good / rolling_all*100)%>%
  ggplot(., aes(x=inspectiondate, y=rolling_good_ratio, colour=ownership))+
  geom_line()+
  labs(x="Year", y="Inspected Good or Outstanding\n(%)", color="Ownership")+
  theme_bw()+
  theme(text = element_text(size=20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.length=unit(.28, "cm"),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=24),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=20),
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", size = 1),
        legend.text = element_text(size=20),
        legend.position = "top",
        strip.background = element_rect(fill="gray90", colour="black", size=1),
        strip.text = element_text(face="bold", size=16),
        title=element_text(face="bold")) +
  theme(panel.spacing.x = unit(4, "mm"))


ggsave(plot=fig2, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/adults_social_care_data/fig3.png", width=15, height=8, dpi=600)
