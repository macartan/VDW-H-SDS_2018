## Can External Interventions Empower Women in Traditional Societies? 

rm(list=ls(all=TRUE)) 

# Packages
  library(macartan.fragments)
  packages <- c("foreign", "dplyr", "car", "sandwich")
  sapply(packages,library,character.only=TRUE)

# Options
  # random sample only
  random_sample <- TRUE
  
  # projects list 1
  projects <- c("sante2", "education2", "transport2", "watsan2", "agr2")
  
  # projects list 2
  RAPIDprojects <- c("RAPIDhealth", "RAPIDeducation", "RAPIDtransport", "RAPIDwatsan", "RAPIDagriculture", "RAPIDprivate")

  # preferred projects list
  pprojects <- c("health", "edu", "transport", "watsan", "agri", "private", "other")
  
  # attitudes list
  attitudes <- c("qg008_women2",  "qg009_mistreatment2", "qg010_socioadmin2", "qg011_women_pres2")

# Get data & Prep variables
####################################################################################

source("R/1 prep.R")

# Analysis
####################################################################################

# First Stage
lm_cluster_robust("femmes~cdv_parity", IRC_tracking[IRC_tracking$pilot_lottery==1,], "IDV_CDCCODE")

source("R/Table_2_Tuungane_Project.R")

source("R/Table_3_Rapid_Project.R")

source("R/Table_4_Other_Behavioral.R")

source("R/Table_5_attitudes.R")

source("R/Figure_B_Time_Allocation.R")

source("R/Table_extra_male_dominance.R")

# Additional
####################################################################################

# Chief selection figures
# source("R/Figure_A_Chief_Selection.R")

## END ##
