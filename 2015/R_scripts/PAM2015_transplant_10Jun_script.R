## This script formats data from the PAM flurometer from the transplant experiments in June 2015
## Created by KBG Oct-2015



## Source the R script that reformats data from the PAM .csv output files
source('2015/R_scripts/PAM.data.read_Oct2015.R')


##### Read in the data #####
dir_input <- file.path("2015", "PAM_data", "PAM_data_raw")

## PAM data file to read in
  pathway <- file.path(dir_input, "Transplant_PAM_10Jun2015_edit.csv")

## Reformat the data from the PAM
  PAMdf10 <- ParsePAM_KBG(pathway)
  str(PAMdf10)

## Drop bad Memory lines based on redos in notes
  mem.drop10 <- c(92, 140, 199, 209, 328, 395, 609, 640, 718, 746)
  PAMdf10 <- lapply(PAMdf10,
                    function(x) subset(x, !(Mem %in% mem.drop10)))

## Extract regression parameters and make a separate dataframe
  rp <- as.data.frame(PAMdf10[1])
  colnames(rp) <- gsub("Regression.parameters.", "", colnames(rp))
  rp$Mem <- as.factor(rp$Mem)

## Extract light curve data and make a separate dataframe
  lc <- as.data.frame(PAMdf10[2])
  colnames(lc) <- gsub("Light.curve.data.", "", colnames(lc))
  lc$Mem <- as.factor(lc$Mem)
  lc$Curve <- as.factor(lc$Curve)

## Experimental metadata file to read in
  info <- read.csv(file.path(dir_input, "Metadata_10Jun2015_edit.csv"))
  info$Mem <- as.factor(info$Mem)
  info$Date <- as.Date(info$Date, format ="%d-%b-%y")
  info$CapID <- factor(info$CapID, levels(info$CapID)[c(1,2,3,5,6,7,4)])

## Inspect data
  #str(rp)
  #str(lc)
  #str(info)

## Check how the memory lines match between the lc/rp and info dataframes
  # unique(lc$Mem[-which(lc$Mem %in% info$Mem)])
  # unique(lc$Mem[which(rp$Mem %in% info$Mem)])

## Merge metadata and PAM output data
  lc10 <- merge(info,lc)
  rp10 <- merge(info,rp)

