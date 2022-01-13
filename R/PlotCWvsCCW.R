

PlotCWvsCCW<- function(exp){


# Load required data based on function arugment
Ldata<- sprintf("data/%s_Localizations.csv", exp)
Adata<- sprintf("data/%s_Tap_Angles.csv", exp)
Loc<-read.csv(Ldata, header = TRUE)
Ang<-read.csv(Adata, header = TRUE)

#Duplicate the data to use later
CW<-Loc
CCW<-Loc

#Make the localizations to CCW directions NA
for (p in 2:33){

CW[,p][Ang[,p] == 65] <- NA
CW[,p][Ang[,p] == 85] <- NA
CW[,p][Ang[,p] == 105] <- NA
CW[,p][Ang[,p] == 125] <- NA
}

#Make the localizations to CW directions NA
for (p in 2:33){
  
  CCW[,p][Ang[,p] == 55] <- NA
  CCW[,p][Ang[,p] == 75] <- NA
  CCW[,p][Ang[,p] == 95] <- NA
  CCW[,p][Ang[,p] == 115] <- NA
}


#Plot all three sets of localizations to see if there is a system difference in the direction of localization for CW and CCW target locations
#AveLoc<-rowMeans(Loc[,2:33], na.rm = TRUE)
#plot(AveLoc, type = "l", col = "Red", main = exp)
plot(rowMeans(CW[,2:33], na.rm = TRUE), type = "l", col = "Blue", main = exp, 
     xlab = "Trials", ylab = "Hand Location Change[°]", axes = F, ylim = c(-15,15), cex.lab = 1.5)
lines(rowMeans(CCW[,2:33], na.rm = TRUE), type = "l", col = "Orange")
legend(
  100,
  5,
  legend = c(
     "CW", "CCW"),
  col = c("Blue", "Orange"),
  lty = c(1,1),
  lwd = c(2,2),
  bty = 'n', cex = 1.25
)
# lines(c(1, 64, 64, 224, 224, 240, 240),
#       c(0, 0, 30, 30, -30, -30, 0),
#       col = rgb(0., 0., 0.))
# lines(c(240, 288),
#       c(0, 0),
#       lty = 2,
#       col = rgb(0., 0., 0.))
axis(2, at = c(-15, 0, 15), cex.axis = 1.5,
     las = 2)
axis(1, at = c(1, 64, 224, 240, 288), cex.axis = 1.5,las=2)
}