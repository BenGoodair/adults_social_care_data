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
   dplyr::mutate(Total=as.character(as.numeric(Own.provision..including.joint.arrangements.)+as.numeric(Provision)))%>%
   tidyr::pivot_longer(cols = !c("SupportSetting", "DH_GEOGRAPHY_NAME"), names_to = "Sector", values_to = "Expenditure")%>%
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
 
 
 
 ####plotfun####
 one <- ggplot(asc01[asc01$Sector=="External",], aes(x = factor(year), y = percent_sector, color = percent_sector)) +
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
