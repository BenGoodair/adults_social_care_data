if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools, dplyr, tidyverse, tidyr, stringr,  curl, plm, readxl)


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
                 Service=="C6"|
                 Service=="C11"|
                 Service=="D11"|
                 Service=="E11"|
                 Service=="F11")%>%
   dplyr::distinct()%>%
   dplyr::rename(SupportSetting=NA.)%>%
   dplyr::mutate(SupportSetting = ifelse(SupportSetting=="Home care", "Home care",
                                         ifelse(SupportSetting=="Nursing home placements", "Nursing home placements",
                                                ifelse(SupportSetting=="Residential care home placements", "Residential care home placements",
                                                        ifelse(SupportSetting=="Supported and other accommodation", "Supported and other accommodation",
                                                             ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH A PHYSICAL DISABILITY ETC (LINES D1 to D10)", "U65 PHYSICAL DISABILITY",
                                                                    ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH LEARNING DISABILITIES (LINES E1 to E10)", "U65 LEARNING DISABILITY",
                                                                            ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH MENTAL HEALTH NEEDS (LINES F1 to F10)", "U65 MENTAL HEALTH",
                                                                                  ifelse(SupportSetting=="TOTAL OLDER PEOPLE (LINES C1 to C10)", "Total over 65", 
                                                                                         NA)))))))))%>%
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
                   Service=="C6"|
                   Service=="C11"|
                   Service=="D11"|
                   Service=="E11"|
                   Service=="F11")%>%
   dplyr::distinct()%>%
   dplyr::rename(SupportSetting=NA.)%>%
   dplyr::mutate(SupportSetting = ifelse(SupportSetting=="Home care", "Home care",
                                         ifelse(SupportSetting=="Nursing home placements", "Nursing home placements",
                                                ifelse(SupportSetting=="Residential care home placements", "Residential care home placements",
                                                       ifelse(SupportSetting=="Supported and other accommodation","Supported and other accommodation",
                                                              ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH A PHYSICAL DISABILITY ETC (LINES D1 to D10)", "U65 PHYSICAL DISABILITY",
                                                                     ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH LEARNING DISABILITIES (LINES E1 to E10)", "U65 LEARNING DISABILITY",
                                                                            ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH MENTAL HEALTH NEEDS (LINES F1 to F10)", "U65 MENTAL HEALTH",
                                                                                   ifelse(SupportSetting=="TOTAL OLDER PEOPLE (LINES C1 to C10)", "Total over 65", NA)))))))))%>%
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
                   Service=="C6"|
                   Service=="C11"|
                   Service=="D11"|
                   Service=="E11"|
                   Service=="F11")%>%
   dplyr::distinct()%>%
   dplyr::rename(SupportSetting=NA.)%>%
   dplyr::mutate(SupportSetting = ifelse(SupportSetting=="Home care", "Home care",
                                         ifelse(SupportSetting=="Nursing care placements", "Nursing home placements",
                                                ifelse(SupportSetting=="Residential care placements", "Residential care home placements",
                                                       ifelse(SupportSetting=="Supported and other accommodation", "Supported and other accommodation",
                                                              ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH A PHYSICAL DISABILITY ETC (LINES D1 to D10)", "U65 PHYSICAL DISABILITY",
                                                                     ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH LEARNING DISABILITIES (LINES E1 to E10)", "U65 LEARNING DISABILITY",
                                                                            ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH MENTAL HEALTH NEEDS (LINES F1 to F10)", "U65 MENTAL HEALTH",
                                                                                   ifelse(SupportSetting=="TOTAL OLDER PEOPLE (LINES C1 to C10)", "Total over 65", NA)))))))))%>%
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
                   Service=="C6"|
                   Service=="C11"|
                   Service=="D11"|
                   Service=="E11"|
                   Service=="F11")%>%
   dplyr::distinct()%>%
   dplyr::rename(SupportSetting=NA.)%>%
   dplyr::mutate(SupportSetting = ifelse(SupportSetting=="Home care", "Home care",
                                         ifelse(SupportSetting=="Nursing care placements", "Nursing home placements",
                                                ifelse(SupportSetting=="Residential care placements", "Residential care home placements",
                                                       ifelse(SupportSetting=="Supported and other accommodation", "Supported and other accommodation",
                                                              ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH A PHYSICAL DISABILITY ETC excl SP (LINES D1 to D10)", "U65 PHYSICAL DISABILITY",
                                                                     ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH LEARNING DISABILITIES excl SP (LINES E1 to E10)", "U65 LEARNING DISABILITY",
                                                                            ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH MENTAL HEALTH NEEDS excl SP (LINES F1 to F10)", "U65 MENTAL HEALTH",
                                                                                   ifelse(SupportSetting=="TOTAL OLDER PEOPLE excluding Supporting People (LINES C1 to C10)", "Total over 65", NA)))))))))%>%
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
                    Service=="C6"|
                    Service=="C11"|
                    Service=="D11"|
                    Service=="E11"|
                    Service=="F11")%>%
    dplyr::distinct()%>%
    dplyr::rename(SupportSetting=NA.)%>%
    dplyr::mutate(SupportSetting = ifelse(SupportSetting=="Home care", "Home care",
                                          ifelse(SupportSetting=="Nursing care placements", "Nursing home placements",
                                                 ifelse(SupportSetting=="Residential care placements", "Residential care home placements",
                                                        ifelse(SupportSetting=="Supported and other accommodation", "Supported and other accommodation",
                                                               ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH A PHYSICAL DISABILITY ETC excl SP (LINES D1 to D10)", "U65 PHYSICAL DISABILITY",
                                                                      ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH LEARNING DISABILITIES excl SP (LINES E1 to E10)", "U65 LEARNING DISABILITY",
                                                                             ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH MENTAL HEALTH NEEDS excl SP (LINES F1 to F10)", "U65 MENTAL HEALTH",
                                                                                    ifelse(SupportSetting=="TOTAL OLDER PEOPLE excluding Supporting People (LINES C1 to C10)", "Total over 65", NA)))))))))%>%
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
                    Service=="C6"|
                    Service=="C11"|
                    Service=="D11"|
                    Service=="E11"|
                    Service=="F11")%>%
    dplyr::distinct()%>%
    dplyr::rename(SupportSetting=NA.)%>%
    dplyr::mutate(SupportSetting = ifelse(SupportSetting=="Home care", "Home care",
                                          ifelse(SupportSetting=="Nursing care placements", "Nursing home placements",
                                                 ifelse(SupportSetting=="Residential care placements", "Residential care home placements",
                                                        ifelse(SupportSetting=="Supported and other accommodation", "Supported and other accommodation",
                                                               ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH A PHYSICAL DISABILITY ETC excl SP (LINES D1 to D10)", "U65 PHYSICAL DISABILITY",
                                                                      ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH LEARNING DISABILITIES excl SP (LINES E1 to E10)", "U65 LEARNING DISABILITY",
                                                                             ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH MENTAL HEALTH NEEDS excl SP (LINES F1 to F10)", "U65 MENTAL HEALTH",
                                                                                    ifelse(SupportSetting=="TOTAL OLDER PEOPLE excluding Supporting People (LINES C1 to C10)", "Total over 65", NA)))))))))%>%
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
                    Service=="C6"|
                    Service=="C11"|
                    Service=="D11"|
                    Service=="E11"|
                    Service=="F11")%>%
    dplyr::distinct()%>%
    dplyr::rename(SupportSetting=NA.)%>%
    dplyr::mutate(SupportSetting = ifelse(SupportSetting=="Home care", "Home care",
                                          ifelse(SupportSetting=="Nursing care placements", "Nursing home placements",
                                                 ifelse(SupportSetting=="Residential care placements", "Residential care home placements",
                                                        ifelse(SupportSetting=="Supported and other accommodation", "Supported and other accommodation",
                                                               ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH A PHYSICAL DISABILITY ETC excl SP (LINES D1 to D10)", "U65 PHYSICAL DISABILITY",
                                                                      ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH LEARNING DISABILITIES excl SP (LINES E1 to E10)", "U65 LEARNING DISABILITY",
                                                                             ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH MENTAL HEALTH NEEDS excl SP (LINES F1 to F10)", "U65 MENTAL HEALTH",
                                                                                    ifelse(SupportSetting=="TOTAL OLDER PEOPLE excluding Supporting People (LINES C1 to C10)", "Total over 65", NA)))))))))%>%
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
                     Service=="B6"|
                     Service=="B11"|
                     Service=="C11"|
                     Service=="D11"|
                     Service=="E11")%>%
     dplyr::distinct()%>%
     dplyr::rename(SupportSetting=NA.)%>%
     dplyr::mutate(SupportSetting = ifelse(SupportSetting=="Home care", "Home care",
                                           ifelse(SupportSetting=="Nursing care placements", "Nursing home placements",
                                                  ifelse(SupportSetting=="Residential care placements", "Residential care home placements",
                                                         ifelse(SupportSetting=="Supported and other accommodation", "Supported and other accommodation",
                                                                ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH A PHYSICAL DISABILITY ETC excl SP (LINES C1 to C10)", "U65 PHYSICAL DISABILITY",
                                                                       ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH LEARNING DISABILITIES excl SP (LINES D1 to D10)", "U65 LEARNING DISABILITY",
                                                                              ifelse(SupportSetting=="TOTAL ADULTS AGED UNDER 65 WITH MENTAL HEALTH NEEDS excl SP (LINES E1 to E10)", "U65 MENTAL HEALTH",
                                                                                     ifelse(SupportSetting=="TOTAL OLDER PEOPLE excluding Supporting People (LINES B1 to B10)", "Total over 65", NA)))))))))%>%
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
   
   #ughhhhhhhh
   
#2010
####NEED TO CREATE A TOTAL FOR ALL PROVISION TYPES PLZ####   
   
   asc10 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2010/PSSEX_0910_OUTPUT.csv"))%>%
     dplyr::filter(MainHeading=="1. GROSS TOTAL COST (Current expenditure including capital charges)",
                   Category.Order==2&Subset.Order==22|
                     Category.Order==2&Subset.Order==5|
                     Category.Order==2&Subset.Order==6|
                     Category.Order==2&Subset.Order==7|
                     Category.Order==2&Subset.Order==8|
                     Category.Order==5&Subset.Order==23|
                     Category.Order==6&Subset.Order==24|
                     Category.Order==7&Subset.Order==25)%>%
     dplyr::select(council, SubHeading, Subset, Value)%>%
     dplyr::mutate(year=2010,
                   SubHeading= ifelse(SubHeading=="Own Provision", "In House",
                                      ifelse(SubHeading=="Provision by Others", "External",SubHeading)),
                   Subset = ifelse(Subset=="Home care", "Home care",
                                     ifelse(Subset=="Nursing and residential care placements total", "Nursing home placements",
                                            ifelse(Subset=="Residential care placements", "Residential care home placements",
                                                   ifelse(Subset=="Supported and other accommodation", "Supported and other accommodation",
                                                          ifelse(Subset=="TOTAL ADULTS with PSD, Aged 18 to 64, excluding Supporting People", "U65 PHYSICAL DISABILITY",
                                                                 ifelse(Subset=="TOTAL ADULTS WITH LD, Aged 18 to 64, excluding Supporting People", "U65 LEARNING DISABILITY",
                                                                        ifelse(Subset=="TOTAL ADULTS WITH MH NEEDS, Aged 18 to 64, excluding Supporting People", "U65 MENTAL HEALTH",
                                                                               ifelse(Subset=="TOTAL OLDER PEOPLE excluding Supporting People", "Total over 65", NA)))))))))%>%
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
                  Category.Order==5&Subset.Order==23|
                  Category.Order==6&Subset.Order==24|
                  Category.Order==7&Subset.Order==25)%>%
  dplyr::select(council, SubHeading, Subset, Value)%>%
  dplyr::mutate(year=2011,
                SubHeading= ifelse(SubHeading=="Own Provision", "In House",
                                   ifelse(SubHeading=="Provision by Others", "External",SubHeading)),
                Subset = ifelse(Subset=="Home care", "Home care",
                                ifelse(Subset=="Nursing and residential care placements total", "Nursing home placements",
                                       ifelse(Subset=="Residential care placements", "Residential care home placements",
                                              ifelse(Subset=="Supported and other accommodation", "Supported and other accommodation",
                                                     ifelse(Subset=="TOTAL ADULTS with PSD, Aged 18 to 64, excluding Supporting People", "U65 PHYSICAL DISABILITY",
                                                            ifelse(Subset=="TOTAL ADULTS WITH LD, Aged 18 to 64, excluding Supporting People", "U65 LEARNING DISABILITY",
                                                                   ifelse(Subset=="TOTAL ADULTS WITH MH NEEDS, Aged 18 to 64, excluding Supporting People", "U65 MENTAL HEALTH",
                                                                          ifelse(Subset=="TOTAL OLDER PEOPLE excluding Supporting People", "Total over 65", NA)))))))))%>%
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
                                              ifelse(Subset=="Supported and other accommodation", "Supported and other accommodation",
                                                     ifelse(Subset=="TOTAL ADULTS with PSD, Aged 18 to 64, excluding Supporting People", "U65 PHYSICAL DISABILITY",
                                                            ifelse(Subset=="TOTAL ADULTS WITH LD, Aged 18 to 64, excluding Supporting People", "U65 LEARNING DISABILITY",
                                                                   ifelse(Subset=="TOTAL ADULTS WITH MH NEEDS, Aged 18 to 64, excluding Supporting People", "U65 MENTAL HEALTH",
                                                                          ifelse(Subset=="TOTAL OLDER PEOPLE excluding Supporting People", "Total over 65", NA)))))))))%>%
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
                                              ifelse(Subset=="Supported and other accommodation", "Supported and other accommodation",
                                                     ifelse(Subset=="TOTAL ADULTS with PSD, Aged 18 to 64, excluding Supporting People", "U65 PHYSICAL DISABILITY",
                                                            ifelse(Subset=="TOTAL ADULTS WITH LD, Aged 18 to 64, excluding Supporting People", "U65 LEARNING DISABILITY",
                                                                   ifelse(Subset=="TOTAL ADULTS WITH MH NEEDS, Aged 18 to 64, excluding Supporting People", "U65 MENTAL HEALTH",
                                                                          ifelse(Subset=="TOTAL OLDER PEOPLE excluding Supporting People", "Total over 65", NA)))))))))%>%
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
                                              ifelse(Subset=="Supported and other accommodation", "Supported and other accommodation",
                                                     ifelse(Subset=="TOTAL ADULTS with PSD, Aged 18 to 64, excluding Supporting People", "U65 PHYSICAL DISABILITY",
                                                            ifelse(Subset=="TOTAL ADULTS WITH LD, Aged 18 to 64, excluding Supporting People", "U65 LEARNING DISABILITY",
                                                                   ifelse(Subset=="TOTAL ADULTS WITH MH NEEDS, Aged 18 to 64, excluding Supporting People", "U65 MENTAL HEALTH",
                                                                          ifelse(Subset=="TOTAL OLDER PEOPLE excluding Supporting People", "Total over 65", NA)))))))))%>%
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
                                                             ifelse(Age.category=="Age 65 and over"&Delivery.mechanism=="Total", "Total over 65",
                                                                    ifelse(Age.category=="Age 18 to 64"&Summary=="Mental health support","U65 MENTAL HEALTH",
                                                                           ifelse(Age.category=="Age 18 to 64"&Summary=="Learning disability support","U65 LEARNING DISABILITY",
                                                                                  ifelse(Age.category=="Age 18 to 64"&Summary=="Physical support","U65 PHYSICAL DISABILITY",NA)))))))),
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
                                                     ifelse(Long.Term.Support.Setting=="Nursing", "Nursing home placements",
                                                            NA)))),
                Finance.Description = ifelse(Finance.Description=="Own Provision", "In House",
                                             ifelse(Finance.Description=="Provision by Others", "External", NA)))%>%
  dplyr::rename(Sector=Finance.Description,
                DH_GEOGRAPHY_NAME = Council.Name)%>%
  dplyr::select(-Long.Term.Support.Setting)

#ugh lazy, replaced minus values with zero
  
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
                                              ifelse(SupportSetting_KEY==7, "Residential care home placements",
                                                     ifelse(SupportSetting_KEY==6, "Nursing home placements",
                                                            NA)))),
                FinanceDesc_KEY = ifelse(FinanceDesc_KEY==7, "In House",
                                             ifelse(FinanceDesc_KEY==8, "External", NA)))%>%
  dplyr::rename(Sector=FinanceDesc_KEY,
                DH_GEOGRAPHY_NAME = CASSR_CODE)%>%
  dplyr::select(-SupportSetting_KEY)

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




 ####plotfun####
 
 plotfun <- rbind(asc01,asc02,asc03, asc04, asc05, asc06, asc07, asc08, asc10,asc11, asc12,asc13, asc14, asc15, asc16,asc17)%>%
   dplyr::mutate(SupportSetting = tolower(SupportSetting))%>%
     dplyr::filter(SupportSetting!="supported and other accommodation")
 
 ggplot(plotfun[plotfun$Sector=="External",], aes(x = year, y = percent_sector, color = percent_sector)) +
   geom_point(size = 3) +
   geom_smooth(method = "loess", se = FALSE) +
   labs(
     x = "Year",
     y = "Outsourced spend (%)",
     title = "Percent of expenditure outsourced",
     color = "Outsourced %"
   )+
   theme_bw()+
   facet_wrap(~SupportSetting,nrow = 2)
 
 
 
 
