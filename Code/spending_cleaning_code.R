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


   
 ####plotfun####
 
 plotfun <- rbind(asc01,asc02,asc03, asc04, asc05, asc06, asc07, asc08, asc10,asc11, asc12)%>%
   dplyr::mutate(SupportSetting = tolower(SupportSetting))%>%
     dplyr::filter(SupportSetting!="supported and other accommodation")
 
 one <- ggplot(plotfun[plotfun$Sector=="External",], aes(x = year, y = percent_sector, color = percent_sector)) +
   geom_point(size = 3) +
   geom_smooth(method = "loess", se = FALSE) +
   labs(
     x = "Year",
     y = "Outsourced spend (%)",
     title = "Percent of expenditure outsourced",
     color = "Outsourced %"
   )+
   theme_bw()+
   facet_grid(~SupportSetting)
 
 
 
 
