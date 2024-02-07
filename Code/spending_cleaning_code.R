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
# Home care, residential care, nurse care, supported, and other for 65+
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
 # sheet_names <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Raw_data/2006/council_csvs/sheet_names.csv"))
 # 
 # sheet_names$x <- gsub(" ", "%20", sheet_names$x)
 # 

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
 
 


#2002
 
 excel_path <- "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2002\\spend.xls"
 
 excel_sheets <- excel_sheets(excel_path)
 
 for (sheet_name in excel_sheets) {
   df <- read_excel(excel_path, sheet = sheet_name, col_types = NULL)
   
   csv_path <- paste0("C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2002\\spend_csvs\\output_", sheet_name, ".csv")
   
   write.csv(df, file = csv_path, row.names = FALSE)
 }
 
 write.csv(excel_sheets, file = "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2002\\spend_csvs\\sheet_names.csv", row.names = FALSE)
 
 
 
 
 #2003
 
 excel_path <- "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2003\\spend.xls"
 
 excel_sheets <- excel_sheets(excel_path)
 
 for (sheet_name in excel_sheets) {
   df <- read_excel(excel_path, sheet = sheet_name, col_types = NULL)
   
   csv_path <- paste0("C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2003\\spend_csvs\\output_", sheet_name, ".csv")
   
   write.csv(df, file = csv_path, row.names = FALSE)
 }
 
 write.csv(excel_sheets, file = "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2003\\spend_csvs\\sheet_names.csv", row.names = FALSE)
 
 #2004
 
 excel_path <- "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2004\\spend.xls"
 
 excel_sheets <- excel_sheets(excel_path)
 
 for (sheet_name in excel_sheets) {
   df <- read_excel(excel_path, sheet = sheet_name, col_types = NULL)
   
   csv_path <- paste0("C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2004\\spend_csvs\\output_", sheet_name, ".csv")
   
   write.csv(df, file = csv_path, row.names = FALSE)
 }
 
 write.csv(excel_sheets, file = "C:\\Users\\benjamin.goodair\\OneDrive - Nexus365\\Documents\\GitHub\\adults_social_care_data\\Raw_data\\2004\\spend_csvs\\sheet_names.csv", row.names = FALSE)
 
 
 

