
# Table 8: Attitudes

attitudes_table <-
  sapply(attitudes, function(k) {
    output_function(lm_cluster_robust(paste(k, "~cdc_parity + as.factor(lott_bin)"),
                                      data = attitudes_data,
                                      cluster_name = "IDV_CDCCODE"), coefrows=2)})

## Controls means in
means <-  apply(filter(attitudes_data, cdc_parity==0, pilot_lottery==1)[, attitudes], 2, mean, na.rm=TRUE)
attitudes_table <-rbind(attitudes_table[1:2,], round(means,3), attitudes_table[3,])

# print(attitudes_table)

T8 <- rbind(
  "\\begin{tabular}{lcccc|c}",
  "	&	Same rights	&	Complain 	    &	Socio-admin	  	&	Eligible for &	Index	 \\\\ ",
  "	&	as men 	&	    if mistreated	&	positions	&	 president 	&		 \\\\ \\hline \\hline",
  mat_to_tex(attitudes_table, rownames = c("Parity Effect", "(se)", "Control", "N")) ,
  "\\hline \\hline \\mc{6}{l}{\\parbox{5.1in}{\\small\\singlespace
  \\textit{Notes:} Effect of parity requirement.
  We report sample average treatment effects. Regressions use block fixed effects. Standard errors clustered at the village cluster level. Based on questionss QG8 - QG11. $* p \\le 0.10, ** p \\le 0.05, *** p \\le  0.01$.}
  }",
  "\\label{tab:attitudes}",
  "\\end{tabular}"
)

if(saving){
  sink(paste0(output_folder, "/Table8_attitudes.tex"))
  tablr(T8)
sink()
}

## BASED ON PROJECT DATA
attitudes_tableR <-
  sapply(attitudes, function(k) {
    output_function(lm_cluster_robust(paste(k, "~cdv_parity + as.factor(lott_bin)"),
                                      data = attitudes_data,
                                      cluster_name = "IDV_CDCCODE"))})

means <-  apply(filter(attitudes_data, cdv_parity==0, pilot_lottery==1)[, attitudes], 2, mean, na.rm=TRUE)
attitudes_tableR <-rbind(attitudes_tableR[1:2,], round(means,3), attitudes_tableR[7,])

Trob4 <- rbind(
  "\\begin{tabular}{lcccc|c}",
  "	&	Same rights	&	Complain 	    &	Socio-admin	  	&	Eligible for &	Index	 \\\\ ",
  "	&	as men 	&	    if mistreated	&	positions	&	 president 	&		 \\\\ \\hline \\hline",
  mat_to_tex(attitudes_tableR, rownames = c("Parity Effect", "(se)", "Control", "N")) ,
  "\\hline \\hline \\mc{6}{l}{\\parbox{5.1in}{\\small\\singlespace
  \\textit{Notes:} Effect of parity requirement.
  We report sample average treatment effects. Regressions use block fixed effects. Standard errors clustered at the village cluster level. Based on questionss QG8 - QG11. $* p \\le 0.10, ** p \\le 0.05, *** p \\le  0.01$.}
  }",
  "\\label{tab:rob4}",
  "\\end{tabular}"
)

if(saving){
  sink(paste0(output_folder, "/Table_rob4.tex"))
  tablr(Trob4)
  sink()
}


## LATE

late_A <- sapply(attitudes, function(y) {
  D <- attitudes_data[attitudes_data$pilot_lottery==1,][c(y,  "cdc_parity", "cdv_parity", "lott_bin", "IDV_CDCCODE")]
  D <- D[complete.cases(D),]

  D$Y <- as.vector(unlist(D[y]))
  M <- ivreg(Y~cdv_parity + as.factor(lott_bin) | cdc_parity + as.factor(lott_bin), data = D, na.action="na.exclude")
  output_function((cluster_robust(M, D$IDV_CDCCODE)))
})

meansL <-  apply(filter(attitudes_data, cdv_parity==0, pilot_lottery==1)[, attitudes], 2, mean, na.rm=TRUE)
late_A <-rbind(late_A[1:2,], round(meansL,3), late_A[7,])

Tlate4 <- rbind(
  "\\begin{tabular}{lcccc|c}",
  "	&	Same rights	&	Complain 	    &	Socio-admin	  	&	Eligible for &	Index	 \\\\ ",
  "	&	as men 	&	    if mistreated	&	positions	&	 president 	&		 \\\\ \\hline \\hline",
  mat_to_tex(late_A, rownames = c("Parity Effect", "(se)", "Control", "N")) ,
  "\\hline \\hline \\mc{6}{l}{\\parbox{5.1in}{\\small\\singlespace
  \\textit{Notes:} Effect of parity requirement.
  We report sample average treatment effects. Regressions use block fixed effects. Standard errors clustered at the village cluster level. Based on questionss QG8 - QG11. $* p \\le 0.10, ** p \\le 0.05, *** p \\le  0.01$.}
  }",
  "\\label{tab:late4}",
  "\\end{tabular}"
)

if(saving){
  sink(paste0(output_folder, "/Table_late4.tex"))
  tablr(Tlate4)
  sink()
  }

