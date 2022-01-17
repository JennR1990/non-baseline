CompareCWvsCCW<- function(exp, direction){
  library(effsize)
  library(SMCL)
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


# We want the Average of 4 trials from 1)Aligned 2)Beginning of Rot 1 3)End of Rot 1 4) Rot 2 5) Beginning of EC
# because of counterbalancing, if i take 8 trials that will get me 4 actual trials, 4 of those 8 are the other direction and are NA's


# 1-64 Aligned
# 65-224 Rot 1
# 225-240 Rot 2
# 240-288 EC


CWRM<-TUnbaselinedCombine(CW)
CWRM$Direction<- "CW"
CCWRM<-TUnbaselinedCombine(CCW)
CCWRM$Direction<- "CCW"
RM<-rbind(CWRM,CCWRM)

print(sprintf("Comparison of CW and CCW localization targets for %s feedback", exp))
print(t.test(RM$Aligned[RM$Direction == "CW"],RM$Aligned[RM$Direction == "CCW"], alternative = direction))
print(etaSquaredTtest(RM$Aligned[RM$Direction == "CW"],RM$Aligned[RM$Direction == "CCW"], na.rm = TRUE))
print(t.test(RM$R1_Early[RM$Direction == "CW"],RM$R1_Early[RM$Direction == "CCW"], alternative = direction))
print(etaSquaredTtest(RM$R1_Early[RM$Direction == "CW"],RM$R1_Early[RM$Direction == "CCW"], na.rm = TRUE))
print(t.test(RM$R1_Late[RM$Direction == "CW"],RM$R1_Late[RM$Direction == "CCW"], alternative = direction))
print(etaSquaredTtest(RM$R1_Late[RM$Direction == "CW"],RM$R1_Late[RM$Direction == "CCW"], na.rm = TRUE))
print(t.test(RM$R2[RM$Direction == "CW"],RM$R2[RM$Direction == "CCW"], alternative = direction))
print(etaSquaredTtest(RM$R2[RM$Direction == "CW"],RM$R2[RM$Direction == "CCW"], na.rm = TRUE))
print(t.test(RM$EC[RM$Direction == "CW"],RM$EC[RM$Direction == "CCW"], alternative = direction))
print(etaSquaredTtest(RM$EC[RM$Direction == "CW"],RM$EC[RM$Direction == "CCW"], na.rm = TRUE))
}



#This function takes the data and combines it into Long format for the t-test
TUnbaselinedCombine<- function(data) {
  ParticipantRM<- data.frame()
  participants <- c(2:ncol(data))
  for (participant in participants){
    Aligned<- mean(unlist(data[33:64,participant]), na.rm = TRUE)
    R1_Early<- mean(unlist(data[65:72,participant]), na.rm = TRUE)
    R1_Late<- mean(unlist(data[217:224,participant]), na.rm = TRUE)
    R2<- mean(unlist(data[233:240,participant]), na.rm = TRUE)
    EC<- mean(unlist(data[241:256,participant]), na.rm = TRUE)
    EC_Late<- mean(unlist(data[257:288,participant]), na.rm = TRUE)
    RM<- data.frame(Aligned,R1_Early, R1_Late, R2, EC, EC_Late)
    if (prod(dim(ParticipantRM)) == 0) {
      ParticipantRM <- RM
    } else {
      ParticipantRM <- rbind(ParticipantRM, RM)
    }
  }
  return(ParticipantRM)
}



