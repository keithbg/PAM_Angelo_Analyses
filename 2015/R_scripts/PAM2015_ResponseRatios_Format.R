## Calculate Response ratios for the PAM 2015 experiment
## Script created September 2018 by Keith Bouma-Gregson

#### Libraries #################################################################
library(tidyverse)
library(lubridate)
################################################################################


#### FILE PATHS ################################################################
dir_input <- file.path("2015", "PAM_data")
dir_out <- file.path("2015", "PAM_data")
################################################################################


#### CALCULATE LIGHT CURVE REGRESSION PARAMETERS ###############################
################################################################################

## See file, PAM.regression.formulas.R, for calculations
#################################################################################
## Function to estimate PAM regression parameters for light curve data         ##
## Uses nonlinear regression ('nls')                                           ##
## Returns list: [[1]] regression parameters and [[2]] regression RSS          ##
## See also 6.4 Light Curves (p. 48-51) in the Junior PAM Operator's guide     ##
##     source: http://www.walz.com/downloads/manuals/junior-pam/jpm_071206.pdf ##
## -------------                                                               ##
## REG1                                                                        ##
##   ETR = ETRmPot*(1 - e^(-1*alpha*PPFD/ETRmPot))*(e^(-1*beta*PPFD/ETRmPot))  ##
##   with ETRm = ETRmPot*(alpha/(alpha+beta))*(beta/(alpha+beta))^(beta/alpha) ##
## REG2                                                                        ##
##   ETR = ETRm*TANH(alpha*PPFD/ETRm)                                          ##
## Ek = ETRm/alpha                                                             ##
## Ib = ETRmPot/beta                                                           ##
## -------------                                                               ##
## 'REG1'   Parameters fit by regression 1 (Platt et al., 1980)                ##
## 'REG2'   Parameters fit by regression 2 (Jassby and Platt, 1976)            ##
##--------------                                                               ##
## Regression parameters:                                                      ##
## 'alpha'  init. slope of rapid light curve (RLC) ~ photosynth. efficiency    ##
## 'ETRm'   Maximum electron transport rate [umol/m2/s of electrons]           ##
## 'Ek'     Minimum saturating irradiance [umol/m2/s of photons]               ##
## (Note: Ek = ETRm/alpha)                                                     ##
## REG1 also returns two additonal parameters: 'beta' and 'ETRmPot'            ##
## with ETRmPot/beta representing the 'photoinhibition index'                  ##
## (i.e., PAR needed to photoinhibit ETRmPot by the factor of 1/e)             ##
#################################################################################


#### READ IN DATA
reg.data <- read.table(file.path(dir_input, "PAM2015_data_aggregated.tsv"), sep= "\t", header= TRUE) %>%
  mutate(Date= ymd(Date),
    Mem= as.factor(Mem))


#### COLUMN HEADERS
## Site: replicate transect (1, 2, 3, 4)
## Treatment: channel margin or thalweg
## Site.Unique: combination of transect and treatment (2T = site 2, thalweg treatment)
## ArrayID: Site (1-4 )and pseudo-replicate (1-3 for thalweg treatment, 4-6 for margin treatment) of that site. (ArrayID: 3.5= site 3, margin pseudoreplicate 2)
## UniqueID: site.pseudoreplicate(1-3 thalweg; 4-6 margin).algae


#### RUN SCRIPT TO CALCULATE REGRESSION PARAMETERS (ALPHA, Ek, ETRmax)
source("2015/R_scripts/PAM.get.reg.output.R")
lc.parameters <- PAM.regression.batch(input.df=reg.data) %>%
  mutate(rss= ifelse(REG1.RSS < REG2.RSS, "Y", "N"),
         Day= yday(Date) - yday(unique(Date)[1]))

## Combine with reg.data
lc.reg <- reg.data %>%
              select(Date, ArrayID, Site, Algae, Treatment, PAR, Fv.Fm, Fo, NPQ) %>%
              distinct() %>%
              left_join(., lc.parameters) %>%
              as_tibble()

#### CALCULATE RESPONSE RATIOS
## Extract day0 information
d0.reg <- lc.reg %>%
  filter(Day == 0, PAR == 1) %>%
  rename(d0.alpha.REG2= REG2.alpha, d0.ETRm.REG2 = REG2.ETRm, d0.Ek.REG2= REG2.Ek, d0.Fv.Fm= Fv.Fm) %>%
  select(Site, Algae, Treatment, ArrayID, d0.alpha.REG2, d0.ETRm.REG2, d0.Ek.REG2, d0.Fv.Fm) %>%
  distinct()


## Calculate response ratios
lc.reg.rr <- lc.reg %>%
  left_join(., d0.reg) %>%
  group_by(Day, Site, Algae, Treatment) %>%
  mutate(rr.alpha.REG2= log(REG2.alpha / d0.alpha.REG2),
         rr.ETRm.REG2= log(REG2.ETRm / d0.ETRm.REG2),
         rr.Ek.REG2= log(REG2.Ek / d0.Ek.REG2),
         rr.Fv.Fm= log(Fv.Fm / d0.Fv.Fm)) %>%
  ungroup()


#### AVERAGE OVER PSEUDOREPLICATES
lc.reg.pseu <- lc.reg.rr %>%
  filter(PAR == 1, Algae != "Blank") %>%
  group_by(Date, Site, Algae, Treatment) %>%
  summarize(n= length(rr.alpha.REG2),
            pseu.alpha.REG2 = mean(rr.alpha.REG2, na.rm= TRUE),
            pseu.sd.alpha.REG2 = sd(rr.alpha.REG2, na.rm= TRUE),
            pseu.ETRm.REG2= mean(rr.ETRm.REG2, na.rm= TRUE),
            pseu.sd.ETRm.REG2= sd(rr.ETRm.REG2, na.rm= TRUE),
            pseu.Ek.REG2 = mean(rr.Ek.REG2, na.rm= TRUE),
            pseu.sd.Ek.REG2 = sd(rr.Ek.REG2, na.rm= TRUE),
            pseu.Fv.Fm = mean(rr.Fv.Fm, na.rm= TRUE),
            pseu.sd.Fv.Fm = sd(rr.Fv.Fm, na.rm= TRUE)) %>%
  ungroup() %>%
  mutate(Day= yday(Date) - yday(unique(Date)[1]),
         ID= str_c(Site, Algae, Treatment, sep= "."))



#### AVERAGE OVER REPLICATES
lc.reg.reps <- lc.reg.pseu %>%
  filter(Date != "2015-06-09") %>%
  group_by(Date, Algae, Treatment) %>%
  summarize(n= length(pseu.alpha.REG2),
            mean.rr.alpha.REG2 = mean(pseu.alpha.REG2, na.rm= TRUE),
            sd.rr.alpha.REG2 = sd(pseu.alpha.REG2, na.rm= TRUE),
            se.rr.alpha.REG2 = sd.rr.alpha.REG2 / sqrt(n),
            mean.rr.ETRm.REG2= mean(pseu.ETRm.REG2, na.rm= TRUE),
            sd.rr.ETRm.REG2= sd(pseu.ETRm.REG2, na.rm= TRUE),
            se.rr.ETRm.REG2 = sd.rr.ETRm.REG2 / sqrt(n),
            mean.rr.Ek.REG2= mean(pseu.Ek.REG2, na.rm= TRUE),
            sd.rr.Ek.REG2= sd(pseu.Ek.REG2, na.rm= TRUE),
            se.rr.Ek.REG2 = sd.rr.Ek.REG2 / sqrt(n),
            mean.rr.Fv.Fm= mean(pseu.Fv.Fm, na.rm= TRUE),
            sd.rr.Fv.Fm= sd(pseu.Fv.Fm, na.rm= TRUE),
            se.rr.Fv.Fm = sd.rr.Fv.Fm / sqrt(n)) %>%
  ungroup() %>%
  mutate(ID= str_c(Algae, Treatment, sep= "."))

#### PARAMETERS ###########################
#### AVERAGE OVER PSEUDOREPLICATES
lc.reg.pseu.param <- lc.reg %>%
  filter(PAR == 1, Algae != "Blank") %>%
  group_by(Date, Site, Algae, Treatment) %>%
  summarize(n= length(REG2.alpha),
            pseu.alpha.REG2 = mean(REG2.alpha, na.rm= TRUE),
            pseu.sd.alpha.REG2 = sd(REG2.alpha, na.rm= TRUE),
            pseu.ETRm.REG2= mean(REG2.ETRm, na.rm= TRUE),
            pseu.sd.ETRm.REG2= sd(REG2.ETRm, na.rm= TRUE),
            pseu.Fv.Fm = mean(Fv.Fm, na.rm= TRUE),
            pseu.sd.Fv.Fm = sd(Fv.Fm, na.rm= TRUE)) %>%
  ungroup() %>%
  mutate(Day= yday(Date) - yday(unique(Date)[1]),
         ID= str_c(Site, Algae, Treatment, sep= "."))



lc.reg.reps.param <- lc.reg.pseu.param %>%
  group_by(Date, Algae, Treatment) %>%
  summarize(n= length(pseu.alpha.REG2),
            mean.alpha.REG2 = mean(pseu.alpha.REG2, na.rm= TRUE),
            sd.alpha.REG2 = sd(pseu.alpha.REG2, na.rm= TRUE),
            se.alpha.REG2 = sd.alpha.REG2 / sqrt(n),
            mean.ETRm.REG2= mean(pseu.ETRm.REG2, na.rm= TRUE),
            sd.ETRm.REG2= sd(pseu.ETRm.REG2, na.rm= TRUE),
            se.ETRm.REG2 = sd.ETRm.REG2 / sqrt(n),
            mean.Fv.Fm= mean(pseu.Fv.Fm, na.rm= TRUE),
            sd.Fv.Fm= sd(pseu.Fv.Fm, na.rm= TRUE),
            se.Fv.Fm = sd.Fv.Fm / sqrt(n)) %>%
  ungroup() %>%
  mutate(ID= str_c(Algae, Treatment, sep= "."))




#### MERGE AND EXPORT DATA FRAMES ##############################################
################################################################################

#left_join(lc.reg.reps, pdata.reps) %>%
#write_tsv(path= file.path(dir_out, "PAM_2015_response_ratios_figures2.tsv"))

lc.reg %>%
  write_tsv(path= file.path(dir_out, "PAM2015_regression_parameters.tsv"))

lc.reg.reps.param %>% 
  write_tsv(path= file.path(dir_out, "PAM2015_parameters_figures.tsv"))

lc.reg.reps %>%
write_tsv(path= file.path(dir_out, "PAM2015_response_ratios_figures.tsv"))

#left_join(lc.reg.rr, pdata.rr) %>%
lc.reg.pseu %>%
write_tsv(path= file.path(dir_out, "PAM2015_response_ratios_stats.tsv"))





