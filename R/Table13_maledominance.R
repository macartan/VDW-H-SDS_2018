
###########
# Male Dominance of Preferences: Most complete measure uses Step D to measure actual project choice
###########

# Get a better measure of what project was selected; using step D.
project_recode <- function(var){
  car::recode(var, "
              1:2='watsan';
              c(15,20)='education';
              c(16,21)='health';
              c(7,26,46)='transport';
              c(9, 25,27, 34, 41, 45, 19,23,8,32,13,24,11,33,10, 42)='agric';
              c(28,6,18,12) = 'private';
              c(3, 14, 22, 31,29,43,5,4, 44, 49) = 'other'")
  }


project_recode2 <- function(var){
  car::recode(var, "
              c(4,5)='watsan';
              c(18,23)='education';
              c(19,24)='health';
              c(10,29,49)='transport';
              c(11,12,13,14,16,22,26,27,28,3035,36,37,44,45,48)='agric';
              c(9,15,21,31) = 'private';
              c(6,7,8,17,25,32,34,46,47,50) = 'other'")
}


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


############################
## Prepare dataset
############################

# Individual
 ABD_INDIV[] <- lapply(ABD_INDIV, unclass)
 table(ABD_INDIV$IDS_TYPES)
 ABD_INDIV  <- filter(ABD_INDIV, IDS_TYPES=="DMC")
 table(ABD_INDIV$av_10_gender)

 ABD_INDIV$female <- ABD_INDIV$av_10_gender == 4
 ABD_INDIV$male   <- ABD_INDIV$av_10_gender == 5

# LOTT_BIN
 ABD_INDIV$LOTT_BIN <- as.factor(ABD_INDIV$IDS_LOTT_BIN)
 levels(ABD_INDIV$LOTT_BIN) <- 1:length(levels(ABD_INDIV$LOTT_BIN))
 ABD_INDIV$LOTT_BIN <- as.numeric(ABD_INDIV$LOTT_BIN)

  CHOICE_D <- select(ABD_INDIV, qr001_project_choice, IDS, IDV) %>%
    mutate(projetChoice_d = ifelse(qr001_project_choice<4, NA, qr001_project_choice)) %>%
    mutate(coarseChoice_d = project_recode2(projetChoice_d))

# "Real" project choice is that one most-often mentioned
  CHOICE_VILL <- aggregate(cbind(coarseChoice_d, projetChoice_d) ~ IDV, CHOICE_D, Mode)

# Get a measure of male dominance using the answer from 5 people answering project preference from av_14 (DMC)
MALE_DOM <- dplyr::select(ABD_INDIV,
                          IDV,
                          IDS,
                          IDS_TYPES,
                          IDS_CDCCODE,
                          av_14_bis_project_1,
                          LOTT_BIN, male, female
 ) %>%
  dplyr::filter(IDS_TYPES == "DMC" | IDS_TYPES == "DC")

MALE_DOM <- MALE_DOM %>%
  mutate(projetPref_pop   = av_14_bis_project_1) %>%
  mutate(coarsePref_pop   = project_recode2(projetPref_pop))

# Add in actual project choice
MALE_DOM <- merge(MALE_DOM, CHOICE_VILL, by = "IDV")

# Generate correct measure for population
MALE_DOM <- within(MALE_DOM, {
  Correct_D_coarse <- coarsePref_pop == coarseChoice_d
  Correct_D_projet <- projetPref_pop == projetChoice_d})

# Lottery information
Lottery$IDV_CDCCODE <- Lottery$CDCCODE
MALE_DOM$CDCCODE <- MALE_DOM$IDS_CDCCODE
MALE_DOM <- merge(MALE_DOM, Lottery, by  = "CDCCODE")
MALE_DOM$cdc_parity_male <- MALE_DOM$cdc_parity * MALE_DOM$male

# Dominance
#summary(lm(Correct_D_coarse~ male, data = MALE_DOM))
#summary(lm(Correct_D_projet~ male, data = MALE_DOM))

#summary(lm(Correct_D_coarse~ male*cdc_parity, data = filter(MALE_DOM, pilot_lottery==1)))
#summary(lm(Correct_D_projet~ male*cdc_parity, data = filter(MALE_DOM, pilot_lottery==1)))



lm_cluster_robust("Correct_D_coarse~cdc_parity+male+cdc_parity_male + as.factor(lott_bin)",
                  data = filter(MALE_DOM, pilot_lottery==1),
                  cluster_name = "CDCCODE")

corrects <- c("Correct_D_coarse", "Correct_D_projet")

male_dominance <-
  sapply(corrects, function(k) {
    output_function(lm_cluster_robust(paste(k, "~ cdc_parity+male+cdc_parity_male + as.factor(lott_bin)"),
                                      data = MALE_DOM[MALE_DOM$pilot_lottery==1,],
                                      cluster_name = "CDCCODE"), coefrow = 2:4) })



####################################### Create table

T_dominance <- rbind(
  "\\begin{tabular}{lcc}",
  "\\mc{3}{c}{Preferences realized} \\\\ \\hline",
  "	&	Coarse measure	&	Fine measure  \\\\ \\hline \\hline",
  mat_to_tex(male_dominance, rownames = c("Parity Condition", "(se)", "Male", "(se)", "Interaction", "(se)", "N")),
  "\\hline \\hline \\mc{3}{l}{\\parbox{5in}{\\small\\singlespace
  \\textit{Notes:} Effect of parity requirement on whether individual's ex ante project correspond to RAPID project choice.
  The 'fine measure' is a disaggregation of project sector.
  We report sample average treatment effects. Regressions use block fixed effects. Standard errors clustered at the village cluster level. Includes only villages where RAPID was implemented.  $* p \\le 0.10, ** p \\le 0.05, *** p \\le  0.01$.}
  }",
  "\\label{tab:dominance}",
  "\\end{tabular}"
)

if(saving){
  sink(paste0(output_folder, "/Table13_maledominance.tex"))
  tablr(T_dominance)
  sink()
  }
