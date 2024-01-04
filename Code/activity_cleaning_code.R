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


#As I see it variables to construct are: activity in house % for: home care, nursing care; residential care 2005-2022

#LFG!!!

