## Investigate the impact of time of day on the PAM regression parameters
## 2015 experiment
## other publications show that photosynthesis can have diel patterns
## we want to make sure these diel changes did not impact our experimental results

## Created by KBG Nov-2018

## Reps 1, 3, 5 = Benthic
## Reps 2, 4, 6 = Floating
## Rep pairs 1&2, 3&4, 5&6

#### LIBRARIES #################################################################
library(tidyverse)
library(lme4)
library(lmerTest)
library(lmtest)
library(ggplot2)
################################################################################

#### FILE PATHS ################################################################
dir_input <- file.path("/Users", "KeithBG", "Dropbox", "PAM_Angelo", "PAM_Angelo_Analyses", "2015", "PAM_data")
dir_out <- file.path("/Users", "KeithBG", "Dropbox", "PAM_Angelo", "PAM_Angelo_Analyses", "2015", "Figures")
################################################################################


#### READ TIME OF DAY DATA ####
pam.times <- read_tsv(file.path(dir_input, "PAM2015_measurement_times.tsv")) %>%
  mutate(Date= mdy(Date))
         #Day= paste0("d", yday(Date) - yday(unique(Date)[1])))

#### READ IN PAM DATA ####
reg.data <- read.table(file.path(dir_input, "PAM2015_data_aggregated.tsv"), sep= "\t", header= TRUE) %>%
  mutate(Date= ymd(Date),
         Mem= as.factor(Mem))

#### RUN SCRIPT TO CALCULATE REGRESSION PARAMETERS (ALPHA, Ek, ETRmax)
source("/Users/KeithBG/R_Functions/PAM.get.reg.output.R")
lc.parameters <- PAM.regression.batch(input.df=reg.data) %>%
  mutate(rss= ifelse(REG1.RSS < REG2.RSS, "Y", "N"),
         Day= yday(Date) - yday(unique(Date)[1]))

## Combine with reg.data and time of day data
lc.reg <- reg.data %>%
  select(Date, ArrayID, Site, Algae, Treatment, PAR, Fv.Fm, Fo, NPQ) %>%
  distinct() %>%
  left_join(., lc.parameters) %>% # regression parameters
  left_join(., pam.times) %>%  # time of day data
  filter(Algae != "Blank", PAR == 1) %>%
  mutate(Algae= as.character(Algae),
         deploy_hour= as.numeric(str_replace(paste(hour(lc.reg$deploy), round(minute(lc.reg$deploy)/60, 2), sep= "."), "0.", ""))) %>% # make hours a decimal and class numeric
  as_tibble() %>%
  filter(Day != 0) %>%
  arrange(Date, deploy)

## Add in deploy times for the 4 NA values
   # These were repeat measurements that were taken at the end of the day
   # the last time measured was 19:10, so I added 20 minutes to give a value of 19.5 (19:30)
lc.reg[is.na(lc.reg$deploy_hour), "deploy_hour"] <- 19.5

## Make long for plotting
lc.reg.l <- lc.reg %>%
              gather(param, value, c(Fv.Fm, REG2.alpha, REG2.ETRm))

## Make a figure
tod.plot <- ggplot(data= lc.reg.l, aes(x= deploy_hour, y= value))

tod.plot +
  geom_point(aes(color= Treatment)) +
  geom_smooth(aes(color= Treatment), method= "lm", se= FALSE) +
  facet_grid(param ~ Algae, scales= "free_y") +
  labs(x= "Hours", y= "Parameter value", title= "Time of Day") +
  theme_bw()
ggsave(last_plot(), filename = "TimeOfDay_plot.pdf", width= 8, height= 8, units= "in", path= dir_out)

#### DOES TIME OF DAY AFFECT PAM PARAMETERS? ############################
#### STATISTICAL MODEL #########################################################
# Treatment = fixed effect = p = 2 (Thalweg and margin)
# Algae = fixed effect = Algae = j = 3 (Cladophora, Oedogoneum, Periphyton)
# Replicates = Site = n = 4
# Pseudo-replicates = subsamples = 3 (3 subsamples at each Site)
# Time = 4 different sampling events = t = 4
# Total samples per sampling event = N = 2*3*4*3 = 72
# Total samples without pseudoreplicates per sampling event = 2*3*4 = 24

# When using the sampling over the 4 dates, will treat site as a random effect
# deploy_hour= the time the sample was deployed after measurement, and is the proxy for the time of day measurement

lmer(REG2.alpha ~ Treatment*Algae + (1|Site), data= lc.reg)
names(lc.reg)[7]
str(lc.reg)
fit.algae.alpha <- lmer(REG2.alpha ~ Treatment*Algae + (1|Site), data= lc.reg)
summary(fit.algae.alpha)
anova(fit.algae.alpha)

# Time of day included
fit.algae.alpha.time <- lmer(REG2.alpha ~ Treatment*Algae + deploy_hour + (1|Site), data= lc.reg)
model.sum <- summary(fit.algae.alpha.time)
anova(fit.algae.alpha.time)


fit.algae.ETRm <- lmer(REG2.ETRm ~ Treatment*Algae + (1|Site), data= lc.reg)
msummary(fit.algae.ETRm)
anova(fit.algae.ETRm)

fit.algae.ETRm.time <- lmer(REG2.ETRm ~ Treatment*Algae + deploy_hour + (1|Site), data= lc.reg)
summary(fit.algae.ETRm.time)
anova(fit.algae.ETRm.time)


fit.algae.FvFm <- lmer(Fv.Fm ~ Treatment*Algae + (1|Site), data= lc.reg)
summary(fit.algae.FvFm)
anova(fit.algae.FvFm)

fit.algae.FvFm.time <- lmer(Fv.Fm ~ Treatment*Algae + deploy_hour + (1|Site), data= lc.reg)
summary(fit.algae.FvFm.time)
anova(fit.algae.FvFm.time)


#### WRITE STATISTICAL OUTPUT TO A TXT FILE ####################################

## Use a loop with the columns for FvFm, ETRm, and Alpha in lc.reg
## capture.output() and cat() enable me to write statistical output to a file

for(col in c(7, 19, 20)){
  param <- names(lc.reg[, col])
  dat <- pull(lc.reg[, col])

  cat(c(param, "\n"), file="model_outputs_TimeOfDay.txt", sep="\n", append=TRUE)

  ## Time NOT included
  out.sum <- capture.output(summary(lmer(dat ~ Treatment*Algae + (1|Site), data = lc.reg)))
  cat(c("Summary Table no Time", out.sum, "\n"), file=file.path(dir_out, "TimeOfDay_model_outputs.txt"), sep="\n", append=TRUE)

  out.anova <- capture.output(anova(lmer(dat ~ Treatment*Algae + (1|Site), data = lc.reg)))
  cat(c("ANOVA Table no Time", out.anova, "\n"), file=file.path(dir_out, "TimeOfDay_model_outputs.txt"), sep="\n", append=TRUE)

  ## Time included
  out.sum <- capture.output(summary(lmer(dat ~ Treatment*Algae + deploy_hour + (1|Site), data = lc.reg)))
  cat(c("Summary Table with Time", out.sum, "\n"), file=file.path(dir_out, "TimeOfDay_model_outputs.txt"), sep="\n", append=TRUE)

  out.anova <- capture.output(anova(lmer(dat ~ Treatment*Algae + deploy_hour + (1|Site), data = lc.reg)))
  cat(c("ANOVA Table with Time", out.anova, "\n"), file=file.path(dir_out, "TimeOfDay_model_outputs.txt"), sep="\n", append=TRUE)
}

