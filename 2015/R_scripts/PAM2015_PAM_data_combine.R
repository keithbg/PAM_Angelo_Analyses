## Aggregate cleaned up PAM data from the 4 dates into a single dataframe


## Read in the cleaned up data
  source("2015/R_scripts/PAM2015_transplant_09Jun_script.R")
  source("2015/R_scripts/PAM2015_transplant_10Jun_script.R")
  source("2015/R_scripts/PAM2015_transplant_11Jun_script.R")
  source("2015/R_scripts/PAM2015_transplant_15Jun_script.R")

## Drop the Mark column in lc10 and lc11
  lc10 <- subset(lc10, select=-c(Mark))
  lc11 <- subset(lc11, select=-c(Mark))

## Inspect data
  #str(lc09)
  #str(lc10)
  #str(lc11)
  #str(lc15)

## Combine data into a single dataframe
  pdat <- rbind(lc09, lc10, lc11, lc15)
  pdat$Treatment <- factor(pdat$Treatment, levels(pdat$Treatment)[c(2,1)])
  pdat <- pdat[which(pdat$Curve != "Induction"),]
  pdat$uniqueID <- as.factor(paste(as.character(pdat$ArrayID), as.character(pdat$Algae), sep="."))

  pdat$Site.Unique <- as.numeric(ifelse(pdat$Loc == "1T", 1,
                                 ifelse(pdat$Loc == "2T", 2,
                                 ifelse(pdat$Loc == "3T", 3,
                                  ifelse(pdat$Loc == "4T", 4,
                                  ifelse(pdat$Loc == "1M", 5,
                                  ifelse(pdat$Loc == "2M", 6,
                                  ifelse(pdat$Loc == "3M", 7,
                                  ifelse(pdat$Loc == "4M", 8,""
                                  )))))))))

## Remove Oedogonium PAR values that were higher than 3000
  pdat <- subset(pdat, !(Algae != "Clad" & PAR > 3000))

## Remove PAR values at 68 and 154 that were on dropped light curve collections
 pdat <- subset(pdat, !(Date != "2015-06-09" & Algae == "Clad" & Treatment == "Marg" & PAR == 154))
 pdat <- subset(pdat, !(Algae != "Clad" & Site == 2 & (ArrayID == "2.5" | ArrayID == "c3.1") & PAR == 68))

## Final data frame
  pdat <- droplevels(pdat)
  #str(pdat)

## Write data frame as .csv
  write.table(pdat, file="2015/PAM_data/PAM2015_data_aggregated.tsv", row.names=FALSE, col.names= TRUE, quote= FALSE, sep= "\t")



