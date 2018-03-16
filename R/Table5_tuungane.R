
## Table 5: TUUNGANE Project Choice

  project_choice_main <-
    sapply(projects, function(k) {
      #    sapply(X, function(j) {
      output_function(lm_cluster_robust(paste(k, "~cdc_parity + as.factor(lott_bin)"),
                                        data = IRC_tracking[IRC_tracking$pilot_lottery==1,],
                                        cluster_name = "IDV_CDCCODE"), coefrow = 2) })


  means <-  apply(filter(IRC_tracking, cdc_parity==0, pilot_lottery==1)[, projects], 2, mean, na.rm=TRUE)
  project_choice_main <-rbind(project_choice_main[1:2,], round(means,3), project_choice_main[3,])

T5 <- rbind(
  "\\begin{tabular}{lccccc}",
  "\\mc{6}{c}{\\textit{Tuungane} project choice} \\\\ \\hline",
  "	&	Health	&	Education	&	Transport	&	Watsan	&	Agriculture	 \\\\ \\hline \\hline",

    mat_to_tex(project_choice_main, rownames = c("Parity Effect", "(se)", "Control", "N")) ,
  "\\hline \\hline \\mc{6}{l}{\\parbox{4.5in}{\\small\\singlespace
  \\textit{Notes:} Effect of parity requirement.
  We report sample average treatment effects. Regressions use block fixed effects. Standard errors clustered at the village cluster level. Based on implementing partner's project data and includes villages that were
  and were not surveyed by the research teams.  $* p \\le 0.10, ** p \\le 0.05, *** p \\le  0.01$.}
  }",
  "\\label{tab:project}",
  "\\end{tabular}"
  )

  if(saving){
    sink(paste0(output_folder, "/Table5_tuungane.tex"))
    tablr(T5)
    sink()
  }

## ROBUSTNESS 1: BASED ON PROJECT CODING

  project_choice_robust <-
    sapply(projects, function(k) {
      #    sapply(X, function(j) {
      output_function(lm_cluster_robust(paste(k, "~cdv_parity + as.factor(lott_bin)"),
                                        data = IRC_tracking[IRC_tracking$pilot_lottery==1,],
                                        cluster_name = "IDV_CDCCODE"), coefrow = 2) })

  means2 <-  apply(filter(IRC_tracking, cdv_parity==0, pilot_lottery==1)[, projects], 2, mean, na.rm=TRUE)

  project_choice_robust <-   rbind(project_choice_robust[1:2,], round(means2,2), project_choice_robust[3,])


  Trob1 <- rbind(
    "\\begin{tabular}{lccccc}",
    "\\mc{6}{c}{\\textit{Tuungane} project choice} \\\\ \\hline",
    "	&	Health	&	Education	&	Transport	&	Watsan	&	Agriculture	 \\\\ \\hline \\hline",

    mat_to_tex(project_choice_robust, rownames = c("Parity Effect", "(se)", "Control", "N")) ,
    "\\hline \\hline \\mc{6}{l}{\\parbox{4.5in}{\\small\\singlespace
  \\textit{Notes:} Effect of parity requirement.
  We report sample average treatment effects. Regressions use block fixed effects. Standard errors clustered at the village cluster level. Based on implementing partner's project data and includes villages that were
  and were not surveyed by the research teams.  $* p \\le 0.10, ** p \\le 0.05, *** p \\le  0.01$.}
  }",
    "\\label{tab:rob1}",
    "\\end{tabular}"
  )

  if(saving){
  sink(paste0(output_folder, "/Table_rob1.tex"))
  tablr(Trob1)
  sink()
  }

## ROBUSTNESS 1: LATE ANALYSIS

  D <- IRC_tracking[c(projects, "pilot_lottery", "cdc_parity", "cdv_parity", "lott_bin", "IDV_CDCCODE")]
  D <- D[complete.cases(D),]

  late_T <- sapply(projects, function(y) {
    D$Y <- as.vector(unlist(D[y]))
    M <- ivreg(Y~cdv_parity + as.factor(lott_bin) | cdc_parity + as.factor(lott_bin), data = D, na.action="na.exclude")
    output_function(cluster_robust(M, D$IDV_CDCCODE))
  })
  projects_labs <- c("Health", "Education", "Transport", "WatSan", "Agric.")
  colnames(late_T) <- projects_labs

  means3 <-  apply(filter(IRC_tracking, cdv_parity==0, pilot_lottery==1)[, projects], 2, mean, na.rm=TRUE)

  late_T <- rbind(late_T[c(1,2),], round(means3,2), late_T[7,])


  Tlate1 <- rbind(
    "\\begin{tabular}{lccccc}",
    "\\mc{6}{c}{\\textit{Tuungane} project choice} \\\\ \\hline",
    "	&	Health	&	Education	&	Transport	&	Watsan	&	Agriculture	 \\\\ \\hline \\hline",

    mat_to_tex(late_T, rownames = c("Parity Effect", "(se)", "Control", "N")) ,
    "\\hline \\hline \\mc{6}{l}{\\parbox{4.5in}{\\small\\singlespace
    \\textit{Notes:} Effect of parity requirement.
    We report sample average treatment effects. Regressions use block fixed effects. Standard errors clustered at the village cluster level. Based on implementing partner's project data and includes villages that were
    and were not surveyed by the research teams.  $* p \\le 0.10, ** p \\le 0.05, *** p \\le  0.01$.}
    }",
    "\\label{tab:late1}",
    "\\end{tabular}"
  )

  if(saving){
  sink(paste0(output_folder, "/Table_late1.tex"))
  tablr(Tlate1)
  sink()
  }

  
## Only for the 105 RAPID villages
  
  IRC_trackingRAPID<-IRC_tracking
  IRC_trackingRAPID <- dplyr::filter(IRC_trackingRAPID, IDV_CDCCODE==1101 | IDV_CDCCODE==1104 | IDV_CDCCODE==1106 | IDV_CDCCODE==1108 | IDV_CDCCODE==1110 | IDV_CDCCODE==1112 | IDV_CDCCODE==1114 | IDV_CDCCODE==1148 | IDV_CDCCODE==1202 | IDV_CDCCODE==1203 | IDV_CDCCODE==1207 | IDV_CDCCODE==1209 | IDV_CDCCODE==1212 | IDV_CDCCODE==1213 | IDV_CDCCODE==1216 | IDV_CDCCODE==1217 | IDV_CDCCODE==1220 | IDV_CDCCODE==1222 | IDV_CDCCODE==1312 | IDV_CDCCODE==1313 | IDV_CDCCODE==1315 | IDV_CDCCODE==1319 | IDV_CDCCODE==1322 | IDV_CDCCODE==1323 | IDV_CDCCODE==1326 | IDV_CDCCODE==1329 | IDV_CDCCODE==1338 | IDV_CDCCODE==1339 | IDV_CDCCODE==1401 | IDV_CDCCODE==1404 | IDV_CDCCODE==1406 | IDV_CDCCODE==1410 | IDV_CDCCODE==1413 | IDV_CDCCODE==1417 | IDV_CDCCODE==1419 | IDV_CDCCODE==1422 | IDV_CDCCODE==1423 | IDV_CDCCODE==1425 | IDV_CDCCODE==1426 | IDV_CDCCODE==1427 | IDV_CDCCODE==1430 | IDV_CDCCODE==1433 | IDV_CDCCODE==1436 | IDV_CDCCODE==1437 | IDV_CDCCODE==1438 | IDV_CDCCODE==1444 | IDV_CDCCODE==1445 | IDV_CDCCODE==1449 | IDV_CDCCODE==1450 | IDV_CDCCODE==2242 | IDV_CDCCODE==2247 | IDV_CDCCODE==2248 | IDV_CDCCODE==2250 | IDV_CDCCODE==2252 | IDV_CDCCODE==2301 | IDV_CDCCODE==2306 | IDV_CDCCODE==2307 | IDV_CDCCODE==2316 | IDV_CDCCODE==2320 | IDV_CDCCODE==2326 | IDV_CDCCODE==2426 | IDV_CDCCODE==4113 | IDV_CDCCODE==4114 | IDV_CDCCODE==4147 | IDV_CDCCODE==4149 | IDV_CDCCODE==4150 | IDV_CDCCODE==4151 | IDV_CDCCODE==4154 | IDV_CDCCODE==4156 | IDV_CDCCODE==4157 | IDV_CDCCODE==4158 | IDV_CDCCODE==4162 | IDV_CDCCODE==4168 | IDV_CDCCODE==4169 | IDV_CDCCODE==4170 | IDV_CDCCODE==4171 | IDV_CDCCODE==4173 | IDV_CDCCODE==4174 | IDV_CDCCODE==4175 | IDV_CDCCODE==4177 | IDV_CDCCODE==4214 | IDV_CDCCODE==4217 | IDV_CDCCODE==4219 | IDV_CDCCODE==4222 | IDV_CDCCODE==4226 | IDV_CDCCODE==4230 | IDV_CDCCODE==4231 | IDV_CDCCODE==4257 | IDV_CDCCODE==4308 | IDV_CDCCODE==4309 | IDV_CDCCODE==4318 | IDV_CDCCODE==4323 | IDV_CDCCODE==4325 | IDV_CDCCODE==4326 | IDV_CDCCODE==4327 | IDV_CDCCODE==4340 | IDV_CDCCODE==4341 | IDV_CDCCODE==4345 | IDV_CDCCODE==4346 | IDV_CDCCODE==4347 | IDV_CDCCODE==4348 | IDV_CDCCODE==4350 | IDV_CDCCODE==4353 | IDV_CDCCODE==4355 | IDV_CDCCODE==4361)
  
    
  project_choice_mainRAPID <-
    sapply(projects, function(k) {
      output_function(lm_cluster_robust(paste(k, "~cdc_parity + as.factor(lott_bin)"),
                                        data = IRC_trackingRAPID[IRC_trackingRAPID$pilot_lottery==1,],
                                        cluster_name = "IDV_CDCCODE"), coefrow = 2) })
  
  
  means <-  apply(filter(IRC_trackingRAPID, cdc_parity==0, pilot_lottery==1)[, projects], 2, mean, na.rm=TRUE)
  project_choice_mainRAPID <-rbind(project_choice_mainRAPID[1:2,], round(means,3), project_choice_mainRAPID[3,])
  
  T5R <- rbind(
    "\\begin{tabular}{lccccc}",
    "\\mc{6}{c}{\\textit{Tuungane} project choice} \\\\ \\hline",
    "	&	Health	&	Education	&	Transport	&	Watsan	&	Agriculture	 \\\\ \\hline \\hline",
    
    mat_to_tex(project_choice_mainRAPID, rownames = c("Parity Effect", "(se)", "Control", "N")) ,
    "\\hline \\hline \\mc{6}{l}{\\parbox{4.5in}{\\small\\singlespace
  \\textit{Notes:} Effect of parity requirement.
  We report sample average treatment effects. Regressions use block fixed effects. Standard errors clustered at the village level. Based on implementing partner's project data and includes villages that were
  and were not surveyed by the research teams.  $* p \\le 0.10, ** p \\le 0.05, *** p \\le  0.01$.}
  }",
    "\\label{tab:projectRAPID}",
    "\\end{tabular}"
  )
  
  if(saving){
    sink(paste0(output_folder, "/Table5_tuungane_RAPIDonly.tex"))
    tablr(T5R)
    sink()
  }
  
# END #