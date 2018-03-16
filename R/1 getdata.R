# Import Data from local folder or web

# Function to grab data from dataverse / local path set for data extraction

Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")
f <- function(filename){
  f   <- get_file(paste0(filename, ".tab"), "doi:10.7910/DVN/BSASJR")
  tmp <- tempfile(fileext = ".dta")
  writeBin(as.vector(f), tmp)
  # a <- rio::import(tmp)
  b <- read.dta13(tmp, nonint.factors=T)
  rm(f)
  rm(tmp)
  # return(a)
  return(b)
}

# Datasets     
##  Lottery info (CDC level: has info on parity or not at CDC level; "cdc_parity")
Lottery <- f("Lottery_v2")

## Village data
vill_data <- f("DRC2012_ABD_VILL_v2")

##  IRC tracking data (VDC level) For projects
IRC_tracking <- f("IRC_tracking_v2")

## Indiv data
ind <- ABD_INDIV <- DRC2012_ABD_INDIV <-  f("DRC2012_ABD_INDIV_v2")

## DMC data
DMC <- f("DRC2012_ABD_INDIV_v2")

## Roster data
roster <- f("DRC2012_D_ROSTER_v2")

## Discussion data
discussion <- f("DRC2012_A_DISC_v2")

#  Attitudes data for non-Tuungane subset
Tuungane_data <- f("TUUNGANE_v2")

## RAPID projects
rapid_projects <- vill_data

## Time allocation data
time_data <- DMC

# END #
