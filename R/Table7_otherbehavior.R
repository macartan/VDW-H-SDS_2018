
# Table 7: Other Behavioral: Composition, attendance, discussion

tab_input <- cbind(mapply(df = list(meetings_gender, discussion_gender_village, committee_gender),
                          depvar = c("sharewomenA", "female", "share_women"),
                          function(df, depvar){
                            col <-  output_function(lm_cluster_robust(paste0(depvar, "~cdc_parity + as.factor(lott_bin)"),
                                                   data = df, cluster_name = "IDV_CDCCODE"), coefrows=2)
                            col <- c(col[1:2],
                                     format(round(mean(df[df$cdc_parity==0 & df$pilot_lottery==1, depvar], na.rm = TRUE),3), nsmall=3),
                                     col[3])
                            return(col)
                            }))

T7 <- rbind(
  "\\begin{tabular}{lccc}",
  "& \\mc{3}{c}{Share of women among those that...} \\\\ \\hline",
  "	&	were present	&	spoke	&	were on committee	\\\\ \\hline \\hline",
  # mat_to_tex(cbind(present, interventions, composition), rownames = c("Parity Effect", "(se)", "Control", "N")),
  mat_to_tex(tab_input, rownames = c("Parity Effect", "(se)", "Control", "N")),
  "\\hline \\hline \\mc{4}{l}{\\parbox{3.7in}{\\small\\singlespace
  \\textit{Notes:} Effect of parity requirement.
  We report sample average treatment effects. Regressions use block fixed effects. Based on questions: AM8, AD1, and B13. $* p \\le 0.10, ** p \\le 0.05, *** p \\le  0.01$.}
  }",
  "\\label{tab:otherbehavioral}",
  "\\end{tabular}"
 )



if(saving){
  sink(paste0(output_folder, "/Table7_otherbehavior.tex"))
   tablr(T7)
  sink()
  }

## BASED ON PROJECT DATA


tab_input2 <- cbind(mapply(df = list(meetings_gender, discussion_gender_village, committee_gender),
                          depvar = c("sharewomenA", "female", "share_women"),
                          function(df, depvar){
                            col <-  output_function(lm_cluster_robust(paste0(depvar, "~cdv_parity + as.factor(lott_bin)"),
                                                                      data = df, cluster_name = "IDV_CDCCODE"), coefrows=2)
                            col <- c(col[1:2],
                                     format(round(mean(df[df$cdv_parity==0 & df$pilot_lottery==1, depvar], na.rm = TRUE),3), nsmall=3),
                                     col[3])
                            return(col)
                          }))

colnames(tab_input2) <- c("Were Present", "Spoke", "Were on committee")
rownames(tab_input2) <- c("Estimate", "se.", "Nonparity", "N")

Trob3 <- rbind(
  "\\begin{tabular}{lccc}",
  "& \\mc{3}{c}{Share of women among those that...} \\\\ \\hline",
  "	&	were present	&	spoke	&	were on committee	\\\\ \\hline \\hline",
  mat_to_tex(tab_input2, rownames = c("Parity Effect", "(se)", "Control", "N")),
  "\\hline \\hline \\mc{4}{l}{\\parbox{3.7in}{\\small\\singlespace
  \\textit{Notes:} Effect of parity requirement.
  We report sample average treatment effects. Regressions use block fixed effects. Based on questions: AM8, AD1, and B13. $* p \\le 0.10, ** p \\le 0.05, *** p \\le  0.01$.}
  }",
  "\\label{tab:rob3}",
  "\\end{tabular}"
)

if(saving){
sink(paste0(output_folder, "/Table_rob3.tex"))
tablr(Trob3)
sink()
}


## LATE

tab_inputLATE <- cbind(mapply(df = list(meetings_gender, discussion_gender_village, committee_gender),
                           depvar = c("sharewomenA", "female", "share_women"),
                           function(df, depvar){
                             D <- df[,c(depvar, "cdv_parity", "cdc_parity", "lott_bin", "IDV_CDCCODE")]
                             D <- D[complete.cases(D),]
                             M <- ivreg(as.formula(paste(depvar, "~ cdv_parity + as.factor(lott_bin) | cdc_parity + as.factor(lott_bin)")),  data = D, na.action="na.exclude")
                             col <-  output_function(cluster_robust(M, D$IDV_CDCCODE))
                             col <- c(col[1:2],
                                      format(round(mean(df[df$cdv_parity==0 & df$pilot_lottery==1, depvar], na.rm = TRUE),3)),
                                      col[rownames(col)=="N"])
                             return(col)
                           }))

colnames(tab_inputLATE) <- c("were present", "spoke", "were on committee")
rownames(tab_inputLATE) <- c("Estimate", "se.", "Nonparity", "N")

Tlate3 <- rbind(
  "\\begin{tabular}{lccc}",
  "& \\mc{3}{c}{Share of women among those that...} \\\\ \\hline",
  "	&	were present	&	spoke	&	were on committee	\\\\ \\hline \\hline",
  mat_to_tex(tab_inputLATE, rownames = c("Parity Effect", "(se)", "Control", "N")),
  "\\hline \\hline \\mc{4}{l}{\\parbox{3.7in}{\\small\\singlespace
  \\textit{Notes:} Effect of parity requirement.
  We report sample average treatment effects. Regressions use block fixed effects. Based on questions: AM8, AD1, and B13. $* p \\le 0.10, ** p \\le 0.05, *** p \\le  0.01$.}
  }",
  "\\label{tab:late3}",
  "\\end{tabular}"
)

if(saving){
  sink(paste0(output_folder, "/Table_late3.tex"))
  tablr(Tlate3)
  sink()
  }

