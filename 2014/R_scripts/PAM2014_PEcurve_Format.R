## Read in and format and plot pi.df curve data  from the PAM 2014 experiment
## with Yvonne, Paula, and Mary

## Created by KBG January-2016
## Revised by KBG August-2018

#### LIBRARIES #################################################################
  library(tidyverse)
  library(ggplot2)
  library(stringr)
  library(broom)
################################################################################

#### FILE PATHS ################################################################
dir_input <- file.path("2014", "PAM_data", "raw_data")
dir_output <- file.path("2014", "PAM_data")
################################################################################


#### Read in data from the two days of sampling
 d0 <- read_csv(file.path(dir_input, "Algae_Type_Day0_PI_KBG.csv"))
 d1 <- read_csv(file.path(dir_input, "Algae_Type_Day1_PI_KBG.csv"))
 
new.column.names <- c("Date", "Time", "Rep", "Treatment", "Location", "Algae", "Type", "F0_Menupoint", "Mem", "F", "Fm_p", "PAR", "Y.II", "ETR", "Fo_p", "ETRf", "qP", "qN", "qL", "NPQ", "Fo", "Fm", "FvFm")

## Format data
 pi.df <- rbind(d0, d1) %>%
         filter(Algae != "") %>%
         select(-Comments) %>%
         rename_all(funs(c(new.column.names))) %>%
         #filter(PAR < 1250) %>% # Trim off the end of the light curves, only some of the Cladophora had PAR >1250
         mutate(Date= as.Date(Date, "%m/%d/%y"),
                F0_Menupoint= as.character(F0_Menupoint),
                Location= ifelse((Rep == 1 | Rep ==3 | Rep == 5), "Benthic", "Floating"),
                Day= as.character(ifelse(Date == "2014-07-23", 0, 1)),
                Fo_p= as.numeric(str_replace(.$Fo_p, "~", "")))

## Change algae names
 new.algae.names <- c("Cyano_Spires", "Nostoc", "Phorm", "Riv", "Clad_R", "Clad_Y", "Blank")
 for(i in 1:length(unique(pi.df$Algae))){
   pi.df$Algae <- str_replace(pi.df$Algae, unique(pi.df$Algae)[i], new.algae.names[i])
 }

## Create treatment and unique ID labels
  pi.df <-  pi.df %>%
    mutate(TreatID= paste(Day, Algae, Location, sep="."),
           uniqueID= paste(Algae, Treatment, Rep, sep="."))

## Remove the replicate pi.df curves (only the better curve was kept)
 pi.df[which(pi.df$uniqueID == "Nostoc.E.4" & pi.df$Fo == 178), "uniqueID"] <- "Nostoc.E.4.b"
 pi.df[which(pi.df$uniqueID == "Riv.B.6" & pi.df$Fo == 341), "uniqueID"] <- "Riv.B.6.b"
 pi.df[which(pi.df$uniqueID == "Riv.A.4" & pi.df$Fo == 321), "uniqueID"] <- "Riv.A.4.b"

 pi.df <- pi.df %>%
   filter(pi.df$uniqueID != "Nostoc.E.4.b" & pi.df$uniqueID != "Riv.B.6.b" & pi.df$uniqueID != "Riv.A.4.b") %>%
   mutate_at(vars(Y.II:NPQ), funs(as.numeric(.)))

## Write data to tsv file (order rows by date and mem #)
  pi.df %>%
    arrange(Date, Mem) %>%
    select(Date, Day, Time:Algae, TreatID, uniqueID, Day, everything()) %>%
    write_tsv(path= file.path(dir_output, "PAM2014_clean_light_curve.tsv"))


####  CALCULATE REGRESSION PARAMETERS (ALPHA, Ek, ETRmax)
  source("PAM.regression.formulas.R") # Function written by Yvonne's colleague Katy (I think that is her name)

  reg.params <- pi.df %>%
    filter(F0_Menupoint == "17") %>%
    group_by(Day, uniqueID) %>%
    do(get.Pmax(dat= .)) %>%
    ungroup() %>%
    select(-Date, -Mem) %>%
    left_join(., distinct(select(pi.df, Date, Day, Rep, Treatment, Location, Algae, TreatID, uniqueID))) %>%
    select(Date, Day, uniqueID, Rep, Treatment, Location, Algae, TreatID, everything())

  ## Calculate REG2 parameters for Nostoc.B.5 Day == 1
  ## Default initial parameters failed to create a regression
  nostoc.params <- pi.df %>%
    filter(Day == "1", uniqueID == "Nostoc.B.5", F0_Menupoint == "17") %>%
    arrange(PAR) %>%
    do(tidy(nls(ETR ~ ETRm * tanh(alpha * PAR/ETRm), data= ., start=list(ETRm= 20, alpha= 0.05)))) # Use regression equation from  PAM.regression.formulas.R

  reg.params[which(reg.params$Day == "1" & reg.params$uniqueID == "Nostoc.B.5"), "REG2.ETRm"] <- nostoc.params[1, 2]
  reg.params[which(reg.params$Day == "1" & reg.params$uniqueID == "Nostoc.B.5"), "REG2.alpha"] <- nostoc.params[2, 2]

## Add Fv/Fm and write tsv
pi.df %>%
  filter(F0_Menupoint == "17", PAR == 1) %>%
  select(Day, uniqueID, FvFm) %>%
  distinct() %>%
  left_join(reg.params, .) %>%
  write_tsv(., path= file.path(dir_output, "PAM2014_clean_REG_params.tsv"))






