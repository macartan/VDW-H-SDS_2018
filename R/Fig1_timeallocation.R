
# Creates Figure 1

# Data
data <- data.frame(time_data)

#####################
## PREP DATA
#####################
vars = c("ut000_0to1", "ut001_1to2", "ut002_2to3", "ut003_3to4", "ut004_4to5", "ut005_5to6", "ut006_6to7", "ut007_7to8", "ut008_8to9", "ut009_9to10", "ut010_10to11", "ut011_11to12", "ut012_12to13", "ut013_13to14", "ut014_14to15", "ut15_15to16", "ut16_16to17", "ut17_17to18", "ut18_18to19", "ut19_19to20", "ut20_20to21", "ut21_21to22", "ut22_22to23", "ut23_23to24")

  recode_hours <- function(var){
    v <- NA
    v[var=='Personal care'] <-10
    v[var=='Domestic Work'] <-20
    v[var=='Laundry'] <-21
    v[var=='Cooking'] <-22
    v[var=='Wood collection/coal'] <-23
    v[var=='Water collection'] <-24
    v[var=='House cleaning'] <-25
    v[var=='Purchases of goods and services'] <-26
    v[var=='Work in the field for self consumption'] <-27
    v[var=='Personal services'] <-30
    v[var=='In class/homework'] <-40
    v[var=='Leisure and sports'] <-50
    v[var=='Income generating activities'] <-60
    v[var=='Transport due to work'] <-61
    v[var=='Job search'] <-62
    v[var=='Domestic work for others'] <-63
    v[var=='Self work in village'] <-64
    v[var=='Self work in fields'] <-65
    v[var=='Hunting/Fishing'] <-66
    v[var=='Hired work'] <-67
    v[var=='Work for the state'] <-68
    v[var=='Other work'] <-69
    v[var=='Organisation, civic and religious activities'] <-70
    v[var=='Religious activities'] <-71
    v[var=='Village meetings'] <-72
    v[var=='Work for the village'] <-73
    v[var=='Other associative activities'] <-74
    v[var=='Other'] <-80
    v[var=='Sick'] <-81
    v[var=='Transport unrelated to income generating activities'] <-82
    v }

## create separate activities

  counthours <- function(threshold = 1000){
    z <- sapply(vars, function(v) {
      x <- recode_hours(data[,v])
      #x <- data[,v]
     sapply(c("no", "yes"), function(i) {sum(!is.na(x) & x>0  & x<threshold & data$q011_sex==i, na.rm= TRUE)})})
    #sapply(c(0, 1), function(i) {sum(!is.na(x) & x>0  & x<threshold & data$q011_sex==i, na.rm= TRUE)})})
rownames(z) <- c("w","m")
    z
  }

  all       <- counthours()   #All
  org       <- counthours(80)   ## organizational, civil and religious activities
  money     <- counthours(70)   ## money generating activities
  sport     <- counthours(60)     ## leisure and sport
  edu       <-  counthours(50)     ## educational activities
  personal  <-  counthours(40)     ## personal services
  domestic  <-  counthours(30)       ## OTHER domestic work (26, 27. 21 and 23 as well but they are (as good as) empty)
  cleaning  <-  counthours(26) ## CLEANING HOUSE: 25
  water     <-  counthours(25)   ## COLLECT WATER: 24
  cooking   <-  counthours(23)   ## COOKING: 22 (21 is emptyish)
  sleep     <-  counthours(20)     ## sleep etc


  ############################################

  other<-100-all/all*100
  org<-100-org/all*100
  money<-100-money/all*100
  sport<-100-sport/all*100
  edu  <-100-edu/all*100
  personal<-100-personal/all*100
  domestic<-100-domestic/all*100
  cleaning<-100-cleaning/all*100
  water<-100-water/all*100
  cooking<-100-cooking/all*100
  sleep<-100-sleep/all*100

#####################
## DRAW FIGURE
#####################

par(mfrow=c(2,1))
x <- seq(1,24,1)
yZERO <- rep(0,24)
yHUNDRED <- rep(100,24)

#####################
## FOR WOMEN
#####################

par(mar = c(2, 3, 4, 1)) # b, l, t, r

  plot(0,0, xlim=c(1,24), ylim=c(0,100), xlab="", ylab="", cex.lab=1.5, axes=F, frame.plot=F, type="n", main="Women", cex.main=1.5)
axis(1, at=seq(1,24,1), cex.axis=1)
axis(2, at=seq(0,100,20), cex.axis=1, label=c("0%", "20%", "40%", "60%", "80%", "100%"), las=1)

# draw polygons
  polygon(c(x,rev(x)),c(other[1,],rev(yHUNDRED)),col="white", border = "gray")
  polygon(c(x,rev(x)),c(org[1,],rev(yHUNDRED)),col="white", border = "gray")
  polygon(c(x,rev(x)),c(money[1,],rev(yHUNDRED)),col="white", border = "gray")
  polygon(c(x,rev(x)),c(sport[1,],rev(yHUNDRED)),col="white", border = "gray")
  polygon(c(x,rev(x)),c(edu[1,],rev(yHUNDRED)),col="white", border = "gray")
  polygon(c(x,rev(x)),c(personal[1,],rev(yHUNDRED)),col="white", border = "gray")
  polygon(c(x,rev(x)),c(domestic[1,],rev(yHUNDRED)),col="white", border = "gray")
  polygon(c(x,rev(x)),c(cleaning[1,],rev(yHUNDRED)),col="darkgray", border = "gray")
  polygon(c(x,rev(x)),c(water[1,],rev(yHUNDRED)),col="gray", border = "gray")
  polygon(c(x,rev(x)),c(cooking[1,],rev(yHUNDRED)),col="lightgray", border = "gray")
  polygon(c(x,rev(x)),c(sleep[1,],rev(yHUNDRED)),col="white", border = "gray")

# Add text
  text(9, 1, "Other", cex=1.2,adj = c(0,0))
  text(10, 5, "Social", cex=1.2,adj = c(0,0))
  text(8, 20, "Income-generating", cex=1.2,adj = c(0,0))
  text(16, 15, "Leisure", cex=1.2,adj = c(0,0))
  text(21.2, 12, "Education", cex=1.2,adj = c(0,0))
  lines(c(19,21),c(8,13), cex=2, lwd=1.2)
  text(16, 22, "Services", cex=1.2,adj = c(0,0))
  text(9.5, 50, "Field", cex=1.2,adj = c(0,0))
  text(8, 63, "Cleaning", cex=1.2,adj = c(0,0))
  text(8.5, 73, "Water", cex=1.2,adj = c(0,0))
  text(14, 45, "Cooking", cex=1.2,adj = c(0,0))
  text(16, 90, "Sleep and personal care", cex=1.2,adj = c(0,0))


#####################
## FOR MEN
#####################

  par(mar = c(2, 3, 4, 1)) # b, l, t, r

  plot(0,0, xlim=c(1,24), ylim=c(0,100), xlab="", ylab="", cex.lab=1.5, axes=F, frame.plot=F, type="n", main="Men", cex.main=1.5)
  axis(1, at=seq(1,24,1), cex.axis=1)
  axis(2, at=seq(0,100,20), cex.axis=1, label=c("0%", "20%", "40%", "60%", "80%", "100%"), las=1)

  # draw polygons
  polygon(c(x,rev(x)),c(other[2,],rev(yHUNDRED)),col="white", border = "gray")
  polygon(c(x,rev(x)),c(org[2,],rev(yHUNDRED)),col="white", border = "gray")
  polygon(c(x,rev(x)),c(money[2,],rev(yHUNDRED)),col="white", border = "gray")
  polygon(c(x,rev(x)),c(sport[2,],rev(yHUNDRED)),col="white", border = "gray")
  polygon(c(x,rev(x)),c(edu[2,],rev(yHUNDRED)),col="white", border = "gray")
  polygon(c(x,rev(x)),c(personal[2,],rev(yHUNDRED)),col="white", border = "gray")
  polygon(c(x,rev(x)),c(domestic[2,],rev(yHUNDRED)),col="white", border = "gray")
  polygon(c(x,rev(x)),c(cleaning[2,],rev(yHUNDRED)),col="darkgray", border = "gray")
  polygon(c(x,rev(x)),c(water[2,],rev(yHUNDRED)),col="gray", border = "gray")
  polygon(c(x,rev(x)),c(cooking[2,],rev(yHUNDRED)),col="lightgray", border = "gray")
  polygon(c(x,rev(x)),c(sleep[2,],rev(yHUNDRED)),col="white", border = "gray")

  # Add text
  text(9, 1, "Other", cex=1.2,adj = c(0,0))
  text(10, 7, "Social", cex=1.2,adj = c(0,0))
  text(8, 28, "Income-generating", cex=1.2,adj = c(0,0))
  text(16.5, 30, "Leisure", cex=1.2,adj = c(0,0))
  text(20.2, 38.1, "Education", cex=1.2,adj = c(0,0))
  lines(c(17.5,20),c(38.1,38.1), cex=2, lwd=1.2)
  text(16, 45, "Services", cex=1.2,adj = c(0,0))
  text(9.5, 70, "Field", cex=1.2,adj = c(0,0))
  text(19.2, 58, "Cleaning", cex=1.2,adj = c(0,0))
  lines(c(17,19),c(58,59), cex=2, lwd=1.2)
  text(16.2, 65, "Water", cex=1.2,adj = c(0,0))
  lines(c(16.5,16),c(59,66), cex=2, lwd=1.2)
  text(13.5, 54, "Cooking", cex=1.2,adj = c(0,0))
  text(16, 90, "Sleep and personal care", cex=1.2,adj = c(0,0))

gender_plot <-  recordPlot()

# END #
