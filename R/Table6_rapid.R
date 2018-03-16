
## Table 6: RAPID Project Choice

project_choice_rapid <-
  sapply(RAPIDprojects, function(k) {
    output_function(lm_cluster_robust(paste(k, "~cdc_parity + as.factor(lott_bin)"),
                                      data = rapid_projects[rapid_projects$pilot_lottery==1,],
                                      cluster_name = "IDV_CDCCODE"), coefrow = 2) })

## Controls means in
rap_means <-  apply(filter(rapid_projects, cdc_parity==0, pilot_lottery==1)[, RAPIDprojects], 2, mean, na.rm=TRUE)
project_choice_rapid <-rbind(project_choice_rapid[1:2,], round(rap_means,3), project_choice_rapid[3,])

T6 <- rbind(
  "\\begin{tabular}{lcccccc}",
  "\\mc{7}{c}{RAPID project choice} \\\\ \\hline",
  "	&	Health	&	Education	&	Transport	&	Watsan	&	Agriculture	& Private  \\\\ \\hline \\hline",
  mat_to_tex(project_choice_rapid, rownames = c("Parity Effect", "(se)", "Control", "N")),
  "\\hline \\hline \\mc{7}{l}{\\parbox{5in}{\\small\\singlespace
  \\textit{Notes:} Effect of parity requirement.
  We report sample average treatment effects. Regressions use block fixed effects. Includes only villages where RAPID implemented. Based on question B23. $* p \\le 0.10, ** p \\le 0.05, *** p \\le  0.01$.}
  }",
  "\\label{tab:project_rapid}",
  "\\end{tabular}"
  )

  if(saving){
  sink(paste0(output_folder, "/Table6_rapid.tex"))
   tablr(T6)
  sink()
  }
## BASED ON PROJECT DATA

  project_choice_rapid_project <-
    sapply(RAPIDprojects, function(k) {
      output_function(lm_cluster_robust(paste(k, "~cdv_parity + as.factor(lott_bin)"),
                                        data = rapid_projects[IRC_tracking$pilot_lottery==1,],
                                        cluster_name = "IDV_CDCCODE"), coefrow = 2) })
  project_choice_rapid_project
  rap_means2 <-  apply(filter(rapid_projects, cdv_parity==0, pilot_lottery==1)[, RAPIDprojects], 2, mean, na.rm=TRUE)
  project_choice_rapid_project <-rbind(project_choice_rapid_project[1:2,], round(rap_means2,3), project_choice_rapid_project[3,])

  Trob2 <- rbind(
    "\\begin{tabular}{lcccccc}",
    "\\mc{7}{c}{RAPID project choice} \\\\ \\hline",
    "	&	Health	&	Education	&	Transport	&	Watsan	&	Agriculture	& Private  \\\\ \\hline \\hline",
    mat_to_tex(project_choice_rapid_project, rownames = c("Parity Effect", "(se)", "Control", "N")),
    "\\hline \\hline \\mc{7}{l}{\\parbox{5in}{\\small\\singlespace
  \\textit{Notes:} Effect of parity requirement.
  We report sample average treatment effects. Regressions use block fixed effects. Includes only villages where RAPID implemented. Based on question B23. $* p \\le 0.10, ** p \\le 0.05, *** p \\le  0.01$.}
  }",
    "\\label{tab:rob2}",
    "\\end{tabular}"
  )

  if(saving){
  sink(paste0(output_folder, "/Table_rob2.tex"))
  tablr(Trob2)
  sink()
  }
## LATE

  D <- rapid_projects[rapid_projects$pilot_lottery==1,][c(RAPIDprojects,  "cdc_parity", "cdv_parity", "lott_bin", "IDV_CDCCODE")]

  D <- D[complete.cases(D),]

  late_R <- sapply(RAPIDprojects, function(y) {
    D$Y <- as.vector(unlist(D[y]))
    M <- ivreg(Y~cdv_parity + as.factor(lott_bin) | cdc_parity + as.factor(lott_bin), data = D, na.action="na.exclude")
    output_function(cluster_robust(M, D$IDV_CDCCODE))
  })

    colnames(late_R) <-  RAPID_projects_labs
    rap_means3 <-  apply(filter(rapid_projects, cdv_parity==0, pilot_lottery==1)[, RAPIDprojects], 2, mean, na.rm=TRUE)
    late_R <-rbind(late_R[1:2,], round(rap_means3,3), late_R[7,])

  Tlate2 <- rbind(
    "\\begin{tabular}{lcccccc}",
    "\\mc{7}{c}{RAPID project choice} \\\\ \\hline",
    "	&	Health	&	Education	&	Transport	&	Watsan	&	Agriculture	& Private  \\\\ \\hline \\hline",
    mat_to_tex(late_R, rownames = c("Parity Effect", "(se)", "Control", "N")),
    "\\hline \\hline \\mc{7}{l}{\\parbox{5in}{\\small\\singlespace
    \\textit{Notes:} Effect of parity requirement. We report sample average treatment effects. Regressions use block fixed effects. Includes only villages where RAPID implemented. Based on question B23. $* p \\le 0.10, ** p \\le 0.05, *** p \\le  0.01$.}
    }",
    "\\label{tab:tab:late2}",
    "\\end{tabular}"
  )

  if(saving){
    sink(paste0(output_folder, "/Table_late2.tex"))
    tablr(Tlate2)
    sink()
    }


