
# Clean data and create variable

###########
##  Correct duplicated factor labels in all data sets
###########

fix_labels <- function(dat){
  fix <- unlist(lapply(dat[,1:ncol(dat)], function(x) sum(grepl("_(", x, fixed = T)) > 0))
  dat[,fix] <- lapply(dat[,fix], function(x){
    levels <- levels(x)
    col <- as.factor(gsub("_\\(.*?\\)", "", as.character(x)))
    levels(col) <- c(levels(col), levels[!grepl("_(", levels, fixed = T)])
    return(col)})
  return(dat)}

vill_data <- rapid_projects <- fix_labels(vill_data)
ind <- ABD_INDIV <- DMC <- time_data <- fix_labels(ind)
roster <- fix_labels(roster)

###########
##  Lottery info (CDC level: has info on parity or not at CDC level; "cdc_parity")
###########
Lottery$IDV_CDCCODE <- Lottery$CDCCODE

###########
##  Chief-related variables
###########
chief_data <- dplyr::select(vill_data, IDV, IDV_CDCCODE, cq020_tin:cq066_a_visit_chef_other_localit)

##  Chief measures
chief_data <- within(chief_data, {
  
  #  Chief Tenure -- how long around (how long since last chief)
  departs <- cq049_end_year
  departs[departs<1800] <- NA
  chief_tenure1 <- 2012 - departs
  
  arrives <- cq053_when_chief_year
  arrives[arrives<1800] <- NA
  chief_tenure2 <- 2012 - arrives
  
  #  Chief Enter -- inherited or not?
  chief_enter <- (cq054_how_current_chief == "3 Inherited") 
  chief_enter[cq054_how_current_chief==""] <- NA
  
  # Chief change method
  cq054_how_current_chief[cq054_how_current_chief<0] <- NA
  cq048_how_former_chief[cq048_how_former_chief<0] <- NA
  chief_change <- cq054_how_current_chief != cq048_how_former_chief
}
)

chief <- dplyr::select(chief_data, IDV, IDV_CDCCODE, chief_tenure1, chief_tenure2, chief_change, chief_enter, cq049_end_year, cq054_how_current_chief, cq048_how_former_chief)
nrow(chief)


###########
##  IRC tracking data (VDC level) For projects
###########
##  Merge based on CDC code to bring in cdc_parity; CDV level dataset
IRC_tracking <- merge(Lottery, IRC_tracking,  all = TRUE, by = "CDCCODE")
table(IRC_tracking$cdv_parity[IRC_tracking$cdc_parity==0 & IRC_tracking$pilot_lottery == 1])
table(IRC_tracking$cdv_parity[IRC_tracking$cdc_parity==1 & IRC_tracking$pilot_lottery == 1])

table(IRC_tracking$pilot_lottery[IRC_tracking$cdc_parity==0])
table(IRC_tracking$pilot_lottery[IRC_tracking$cdc_parity==1])
with(IRC_tracking, table(pilot_lottery, cdc_parity))

IRC_tracking$IDV_CDCCODE <- IRC_tracking$CDCCODE
IRC_tracking     <- dplyr::select(IRC_tracking,  CDVCODE, IDV_CDCCODE, cdc_parity, cdv_parity, 
                           pilot_lottery, femmes, sante2, education2, transport2, watsan2, agr2, lott_bin)
IRC_tracking  <- IRC_tracking[IRC_tracking$pilot_lottery==1,]
nrow(IRC_tracking)

IRC_tracking.collapsed  <- IRC_tracking %>%
  dplyr::select(IDV_CDCCODE, cdv_parity)%>%
  group_by(IDV_CDCCODE) %>%
  summarise(cdv_parity = mean(cdv_parity))%>%
  as.data.frame
nrow(IRC_tracking.collapsed)

###########
## Indiv data
###########

if(random_sample) ind <- ind[ind$IDS_TYPES=="DML",]

ind <- ind %>% dplyr::select(IDS, IDV, IDS_RAPID, IDS_CDCCODE, IDS_CHEF, IDS_TYPES, qe013_a_water_hrs, qe013_a_water_mns, qe013_b_market_hrs, qe013_b_market_mns, qe013_c_public_transport_hrs, qe013_c_public_transport_mns, qe013_d_prim_school_hrs, qe013_d_prim_school_mns, qe013_e_sec_school_hrs, qe013_e_sec_school_mns, qe013_f_health_hrs, qe013_f_health_mns, qe013_g_pharmacy_hrs, qe013_g_pharmacy_mns, qe013_h_clinique_hrs, qe013_h_clinique_mns, qe013_i_mine_hrs, qe013_i_mine_mns, qe013_j_post_hrs, qe013_j_post_mns, qe013_k_chef_chefferie_hrs, qe013_k_chef_chefferie_mns, q011_sex, q079_a_meeting, q079_b_discours, av_14_need_1, av_14_bis_project_1, q70_know_pm, q72_know_party, q44_actions, q46_1h, qd002_must_be1,  q064_who_decides, q011_sex,  qg000_Att_version, qg008_women,qg011_women_pres, qg009_mistreatment, qg010_socioadmin, q5362_c_committee_since1, q5362_c_committee_since2, q5362_c_committee_since3, q5362_c_committee_since4, q5362_c_committee_since5, q5362_c_committee_since6, q5362_c_committee_since7, q5362_c_committee_since8, q5362_c_committee_since9, q5362_c_committee_since10, q5362_c_committee_since11, IDS_DISTRICT)
ind$IDV_CDCCODE <- ind$IDS_CDCCODE

###########
## DMC data
###########
DMC <- filter(DMC, IDS_TYPES=="DMC")
DMC <- dplyr::select(DMC, IDS, IDS_DISTRICT, IDS_TYPES, IDV, av_14_need_1, av_14_bis_project_1, av_10_gender, av_11_age, IDS_CDCCODE, IDS_LOTT_BIN, qr001_project_choice)
DMC$sector <- factor(DMC$av_14_bis_project_1)
DMC$sector <- dplyr::recode(DMC$sector,  
                            "distribution choux seeds" = "agri",
                            "riserie" = "agri",
                            "distribution rice seeds" = "agri",
                            "distribution bean seeds" = "agri",
                            "Pisciculture" = "agri",
                            "poultry raising" = "agri",
                            "distribution other seeds" = "agri",
                            "distribution agriculture tools" = "agri",
                            "distribution corn seeds" = "agri",
                            "cows raising" = "agri",
                            "distribution soil nutritions" = "agri",
                            "other raising" = "agri",
                            "distribution peanuts seeds" = "agri",
                            "community field" = "agri",
                            "mill/ renovation" = "agri",
                            "goats raising" = "agri",
                            "distribution school material" = "edu",
                            "school/ renovation" = "edu",
                            "health center / renovation" = "health",
                            "distribution medicines" = "health",
                            "religious center/ renovation" = "other",
                            "other distribution" = "private",
                            "distribution building construction" = "other",
                            "briques creation" = "other",
                            "private project" = "private",
                            "electricity" = "other",
                            "credit system" = "private",
                            "other construction/ renovation" = "other",
                            "distribution money" = "private",
                            "other" = "other",
                            "bridge" = "transport",
                            "market" = "transport",
                            "road" = "transport",
                            "irrigation" = "watsan",
                            "toilets/ renovation" = "watsan",
                            "wells (other water)" = "watsan"	
                            )
table(DMC$av_14_bis_project_1, DMC$sector)
DMC <- within(DMC, {
  agri <- sector == "agri"
  watsan <- sector == "watsan"
  health <- sector == "health"
  transport <- sector == "transport"
  edu <- sector == "edu"
  private <- sector == "private"
  other <- sector == "other"
})

# DMC$female <- DMC$av_10_gender == "female"
DMC$female <- DMC$av_10_gender == 0
DMC$IDV_CDCCODE <- DMC$IDS_CDCCODE 
DMC <- merge(DMC, Lottery, by = "IDV_CDCCODE")  
head(DMC)


###########
# Merge to Individual level
###########

ind <- merge(ind, Lottery, all = TRUE, by = "IDV_CDCCODE")
nrow(ind)

# MAKE OUTCOME: Attitudes
# Keep in mind: There was a variation. Att=0 or Att=1. 
# Keep in mind: Some statements go from pos->neg, neg->pos
# All answers structured: (1) 2 = (very) agree with left, 5 (6) = (very) agree with right, -9=dont know, 0 = indifferent
## Att=0 (GA): qg8: neg -> pos; qg9: pos -> neg; qg10: pos -> neg; qg11: neg -> pos
## Att=1 (GB): qg8: pos -> neg; qg9: neg -> pos; qg10: neg -> pos; qg11: pos -> neg
# Make everything 0-4 scale, from neg (0) to pos (4)	

ind$Att <- as.numeric(ind$qg000_Att_version)
ind$Att[is.na(ind$Att)] <- 0
ind <- within(ind, {  
  # Conservative statements; Recode and reverse if B
  qg008_women2      <-  Recode(qg008_women, "6 = 2; 5 =1; 2=-1;1 = -2; -7 = NA; -9 = NA; - 8 = NA")
  qg008_women2[Att==2] <- -qg008_women2[Att==2]
  qg011_women_pres2 <-  Recode(qg011_women_pres, "6 = 2; 5 =1; 2=-1;1 = -2;-7 = NA; -9 = NA; - 8 = NA")
  qg011_women_pres2[Att==2] <- -qg011_women_pres2[Att==2]
  # Progressive statements Recode and reverse if A  
  qg009_mistreatment2 <- Recode(qg009_mistreatment, "6 = 2; 5 =1; 2=-1;1 = -2; -7 = NA; -9 = NA; - 8 = NA")
  qg009_mistreatment2[Att==1] <- -qg009_mistreatment2[Att==1]
  qg010_socioadmin2   <- Recode(qg010_socioadmin, "6 = 2; 5 =1; 2=-1;1 = -2; -9 = NA;-7 = NA; - 8 = NA")
  qg010_socioadmin2[Att==1] <- -qg010_socioadmin2[Att==1]
})
nrow(ind)

####
#  Attitudes data for non-Tuungane subset
###
attitudes_control <- merge(ind[,c("IDV", "IDS_DISTRICT", "pilot_lottery", "IDS_CHEF", "IDS_CDCCODE", "lott_bin", "q011_sex", attitudes[1:4])], Tuungane_data, by = "IDV")

attitudes_control$parity_in_chefferie <- 
     with(attitudes_control, 
     sapply(1: nrow(attitudes_control), function(i) max(pilot_lottery[IDS_CHEF == IDS_CHEF[i]])))

with(attitudes_control, table(pilot_lottery, TUUNGANE))
with(attitudes_control, table(pilot_lottery, parity_in_chefferie))
with(attitudes_control, table(TUUNGANE, parity_in_chefferie))

attitudes_control2 <- attitudes_control

# Filter
attitudes_control <- filter(attitudes_control, TUUNGANE ==0 & parity_in_chefferie == 0)

ind <- filter(ind, pilot_lottery ==1 )
nrow(ind)

###############################
# Mean effects
###############################

ind$attitudes_mfi <- calculate_mean_effects_index(ind$cdc_parity, ind[,attitudes[1:4]])
#attitudes_data <- ind[, c("IDV_CDCCODE", "IDV", attitudes[1:4], "attitudes_mfi", "IDV_CDCCODE", "IDS_TYPES")]
#attitudes <- c(attitudes, "attitudes_mfi")

attitudes_data <- ind[, c("IDV_CDCCODE", "IDV", attitudes[1:5], "IDV_CDCCODE", "IDS_TYPES", "q011_sex")]

nrow(attitudes_data)

round(cor(ind[, c("qg008_women2", "qg011_women_pres2", "qg009_mistreatment2", "qg010_socioadmin2", "attitudes_mfi")], use = "pairwise.complete.obs"),3) 

# Add in parity data at CDC level
attitudes_data <- merge(attitudes_data, 
                        Lottery[, c("IDV_CDCCODE", "pilot_lottery", "cdc_parity", "lott_bin")], 
                        by = "IDV_CDCCODE", all = TRUE)
nrow(attitudes_data)

attitudes_data <- attitudes_data[attitudes_data$pilot_lottery==1,]
nrow(attitudes_data)
sum(!is.na(attitudes_data$qg008_women) & (attitudes_data$qg008_women>=0) & (attitudes_data$pilot_lottery))

# Add in chief data at LLU level
attitudes_data <- merge(attitudes_data, chief,                        
                        by = "IDV", all = TRUE)
attitudes_data$IDV_CDCCODE <- attitudes_data$IDV_CDCCODE.x
attitudes_data <- attitudes_data[attitudes_data$pilot_lottery==1,]
nrow(attitudes_data)
sum(!is.na(attitudes_data$qg008_women2) & (attitudes_data$pilot_lottery))

nrow(attitudes_data)
attitudes_data <- attitudes_data[attitudes_data$pilot_lottery==1,]
nrow(attitudes_data)

# Only DML
  table(attitudes_data$qg008_women2, attitudes_data$IDS_TYPES)
  aggregate(attitudes_data$qg008_women2, list(attitudes_data$IDV[attitudes_data$qg008_women2!="NA"]), NROW)
  table(attitudes_data$IDV[attitudes_data$qg008_women2!="NA"])
  length(attitudes_data$qg008_women2)
  #hist(attitudes_data$qg008_women2)

attitudes_data <- merge(attitudes_data, IRC_tracking.collapsed, by = "IDV_CDCCODE", all = TRUE)

attitudes_data <- merge(attitudes_data, dplyr::select(vill_data, IDV, IDV_RAPID), by = "IDV", all = TRUE)

attitudes_data <- dplyr::filter(attitudes_data, pilot_lottery ==1)

###########
## RAPID projects
###########

rapid_projects <- dplyr::select(rapid_projects, IDV, b_23_project_has_been_chosen, IDV_RAPID)
table(as.numeric(vill_data$b_23_project_has_been_chosen))

rapid_projects$b_23_new <- rapid_projects$b_23_project_has_been_chosen
table(rapid_projects$b_23_new)

## create RAPID projects

rapid_projects <- within(rapid_projects, {
  
  RAPIDhealth <- (b_23_new=="health center / renovation" | b_23_new=="distribution medicines")
  RAPIDeducation <- (b_23_new=="school/ renovation")
  RAPIDtransport <- (b_23_new=="market" | b_23_new=="road" | b_23_new=="bridge")
  RAPIDwatsan <- (b_23_new=="wells (other water)" | b_23_new=="irrigation")
  RAPIDagriculture <- (b_23_new=="community field" | b_23_new=="mill/ renovation" | b_23_new=="distribution soil nutritions" | b_23_new=="goats raising" | b_23_new=="distribution peanuts seeds" | b_23_new=="distribution corn seeds" | b_23_new=="distribution agriculture tools" | b_23_new=="other raising"  | b_23_new=="distribution bean seeds"  | b_23_new=="poultry raising"  | b_23_new=="cows raising")
  RAPIDprivate <- (b_23_new=="private project" | b_23_new=="credit system"  | b_23_new=="other distribution" | b_23_new=="distribution money")
  RAPIDother <-  (b_23_new=="other" | b_23_new=="briques creation"   | b_23_new=="electricity" | b_23_new=="other construction/ renovation" | b_23_new=="social center/ renovation") 

  #RAPIDhealth <- (b_23_new==21 | b_23_new==16)
  #RAPIDeducation <- (b_23_new==20)
  #RAPIDtransport <- (b_23_new==26 | b_23_new==7 | b_23_new==46)
  #RAPIDwatsan <- (b_23_new==1 | b_23_new==2)
  #RAPIDagriculture <- (b_23_new==25 | b_23_new==19 | b_23_new==23 | b_23_new==8 | b_23_new==32 | b_23_new==13 | b_23_new==24 | b_23_new==11  | b_23_new==33  | b_23_new==10  | b_23_new==9)
  #RAPIDprivate <- (b_23_new==28 | b_23_new==6  | b_23_new==18 | b_23_new==12)
  #RAPIDother <-  (b_23_new==31 | b_23_new==29   | b_23_new==43 | b_23_new==5 | b_23_new==4) 
}
)
table(rapid_projects$RAPIDprivate)
table(rapid_projects$RAPIDhealth)
table(rapid_projects$RAPIDprivate, rapid_projects$b_23_new)
table(rapid_projects$RAPIDhealth, rapid_projects$b_23_new)

rapid_projects <- merge(rapid_projects, chief,                        
                        by = "IDV", all = TRUE)
rapid_projects <- merge(rapid_projects, 
                        Lottery[, c("IDV_CDCCODE", "pilot_lottery", "cdc_parity",  "lott_bin")], 
                        by = "IDV_CDCCODE", all = TRUE)
rapid_projects <- rapid_projects[rapid_projects$pilot_lottery==1,]

rapid_projects <- merge(rapid_projects, IRC_tracking.collapsed, by = "IDV_CDCCODE", all = TRUE)


###########
## Women's share of participants
###########

meetings_gender <- dplyr::select(vill_data, IDV, starts_with("am_8_women_beg"), starts_with("am_9_men_beg"))
meetings_gender$am_9_men_beg[meetings_gender$am_9_men_beg>1000]<-NA
meetings_gender <- within(meetings_gender, {
  sharewomenA <- am_8_women_beg/(am_8_women_beg+am_9_men_beg)
})
meetings_gender$sharewomenA[meetings_gender$am_8_women_beg==NA & meetings_gender$am_9_men_beg==NA] <- NA
meetings_gender <- merge(meetings_gender, chief,                        
                         by = "IDV", all = TRUE)
meetings_gender <- merge(meetings_gender, 
                         Lottery[, c("IDV_CDCCODE", "pilot_lottery", "cdc_parity", "lott_bin")], 
                         by = "IDV_CDCCODE", all = TRUE)
meetings_gender <- meetings_gender[meetings_gender$pilot_lottery==1,]

meetings_gender <- merge(meetings_gender, IRC_tracking.collapsed, by = "IDV_CDCCODE", all = TRUE)

meetings_gender <- merge(meetings_gender, dplyr::select(vill_data, IDV, IDV_RAPID), by = "IDV", all = TRUE)

meetings_gender <- dplyr::filter(meetings_gender, IDV_RAPID==1 & pilot_lottery ==1)


###########
## Women's share of comments (spoke)
###########

# Number of men. Number of women. By village
discussion_gender <- dplyr::select(discussion, IDV,  starts_with("ad1_3_gender"))
discussion_gender$female <-   1 - as.numeric(discussion_gender$ad1_3_gender=="male")
table(discussion_gender$female, discussion_gender$ad1_3_gender)

discussion_gender_village <-   aggregate(female ~ IDV, data = discussion_gender, FUN = mean, na.rm = TRUE)
head(discussion_gender_village)

discussion_gender_village <- merge(discussion_gender_village, chief,                        
                                   by = "IDV", all = TRUE)
discussion_gender_village <- merge(discussion_gender_village, 
                                   Lottery[, c("IDV_CDCCODE", "pilot_lottery", "cdc_parity", "lott_bin")], 
                                   by = "IDV_CDCCODE", all = TRUE)
discussion_gender_village <- discussion_gender_village[discussion_gender_village$pilot_lottery==1,]

discussion_gender_village <- merge(discussion_gender_village, IRC_tracking.collapsed, by = "IDV_CDCCODE", all = TRUE)

discussion_gender_village <- merge(discussion_gender_village, dplyr::select(vill_data, IDV, IDV_RAPID), by = "IDV", all = TRUE)
discussion_gender_village <- dplyr::filter(discussion_gender_village, IDV_RAPID==1 & pilot_lottery ==1)
nrow(discussion_gender_village)

###########
## Composition measure
###########

# Sums up men and women:
committee_gender <- dplyr::select(vill_data, IDV, starts_with("b13_rep_gender_Rep"))
committee_gender <- within(committee_gender, {
men   <- apply(committee_gender=="male", 1, sum, na.rm = TRUE)
women <- apply(committee_gender=="female", 1, sum, na.rm = TRUE)
  share_women <- women/(women+men)
})

table(is.na(committee_gender$share_women))

#hist(committee_gender$share_women, main = "share women on committee")

committee_gender <- merge(committee_gender, chief,                        
                          by = "IDV", all = TRUE)
committee_gender <- merge(committee_gender, 
                          Lottery[, c("IDV_CDCCODE", "pilot_lottery", "cdc_parity", "lott_bin")], 
                          by = "IDV_CDCCODE", all = TRUE)
committee_gender <- committee_gender[committee_gender$pilot_lottery==1,]

committee_gender <- merge(committee_gender, IRC_tracking.collapsed, by = "IDV_CDCCODE", all = TRUE)

committee_gender <- merge(committee_gender, dplyr::select(vill_data, IDV, IDV_RAPID), by = "IDV", all = TRUE)

committee_gender <- dplyr::filter(committee_gender, IDV_RAPID==1 & pilot_lottery ==1)

# END #
