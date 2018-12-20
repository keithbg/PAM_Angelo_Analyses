## Calculate statistics for Response ratios for the PAM 2015 experiment
## Script created September 2018 by Keith Bouma-Gregson

## Response ratios calculated in PAM2015_ResponseRatios_Format.R
## TSV file exported from this script: PAM2015_response_ratios_stats.tsv
## data in: /Users/kbg/Dropbox/PAM_Angelo/2015/PAM_data


#### Libraries #################################################################
library(tidyverse)
library(lme4)
library(lmerTest)
library(lmTest)
################################################################################


#### FILE PATHS ################################################################
dir_input <- file.path("/Users","kbg","Dropbox","PAM_Angelo", "PAM_Angelo_Analyses", "2015", "PAM_data")
################################################################################

#### READ IN DATA ##############################################################
rr.stats <- read_tsv(file.path(dir_input, "PAM2015_response_ratios_stats.tsv")) %>%
             filter(Date != "2015-06-09") # remove first day because all ratios are zero
################################################################################

#### STATISTICAL MODEL #########################################################
# Treatment = fixed effect = p = 2 (Thalweg and margin)
# Algae = fixed effect = Algae = j = 3 (Cladophora, Oedogoneum, Periphyton)
# Replicates = Site = n = 4
# Pseudo-replicates = subsamples = 3 (3 subsamples at each Site)
# Time = 4 different sampling events = t = 4
# Total samples per sampling event = N = 2*3*4*3 = 72
# Total samples without pseudoreplicates per sampling event = 2*3*4 = 24

# When using the sampling over the 4 dates, will treat site as a random effect


#### DO ALGAE RESPOND DIFFERENTLY TO THE TREATMENT? ############################
# Tests: Algae, Treatment, and interaction
# Random effect: Site

fit.algae.alpha <- lmer(pseu.alpha.REG2 ~ Treatment*Algae + (1|Site), data= rr.stats)
summary(fit.algae.alpha)
anova(fit.algae.alpha)

fit.algae.ETRm <- lmer(pseu.ETRm.REG2 ~ Treatment*Algae + (1|Site), data= rr.stats)
summary(fit.algae.ETRm)
anova(fit.algae.ETRm)

fit.algae.FvFm <- lmer(pseu.Fv.Fm ~ Treatment*Algae + (1|Site), data= rr.stats)
summary(fit.algae.FvFm)
anova(fit.algae.FvFm)


corrected.algae.pvalues <- as.data.frame(matrix(c(anova(fit.algae.alpha)[ ,"Pr(>F)"], anova(fit.algae.ETRm)[ ,"Pr(>F)"], anova(fit.algae.FvFm)[ ,"Pr(>F)"]),
                            nrow= 3,
                            dimnames= list(c("Treatment", "Algae", "Treatment:Algae"), c("Alpha", "ETRm", "FvFm"))))
corrected.algae.pvalues <- sapply(corrected.algae.pvalues, function(x) p.adjust(x, method= "bonferroni"))

alga.stats <- rr.stats %>%
  filter(Algae == "Clad") %>%
  mutate(Date = as.factor(Date))

fit.ETRm <- lm(pseu.ETRm.REG2 ~ Treatment*Date, data= alga.stats)

#### DOES THE ALGAL RESPONSE CHANGE OVER TIME? ############################
# Tests: Treatment, Date, and interaction
# Random effect: N/A


## FUNCTION TO RUN SAME STATISTICAL TEST ON EACH ALGA
# lm(paramter ~ Treatment*Date, data= alga.stats)
alga.time.test <- function(){
anova.list <- list()
summary.list <- list()
count <- 1

for(alga in unique(rr.stats$Algae)){

## Subset data for each algal type
alga.stats <- rr.stats %>%
  filter(Algae == alga) %>%
  mutate(Date = as.factor(Date))

fit.alpha <- lm(pseu.alpha.REG2 ~ Treatment*Date, data= alga.stats)
summary.list[[count]] <- summary(fit.alpha)
anova.list[[count]] <- anova(fit.alpha)
names(summary.list)[count] <- paste0(alga, ".alpha")
names(anova.list)[count] <- paste0(alga, ".alpha")
count <- count + 1

fit.ETRm <- lm(pseu.ETRm.REG2 ~ Treatment*Date, data= alga.stats)
summary.list[[count]] <- summary(fit.ETRm)
anova.list[[count]] <- anova(fit.ETRm)
names(summary.list)[count] <- paste0(alga, ".ETRm")
names(anova.list)[count] <- paste0(alga, ".ETRm")
count <- count + 1

fit.FvFm <- lm(pseu.Fv.Fm ~ Treatment*Date, data= alga.stats)
summary.list[[count]] <- summary(fit.FvFm)
anova.list[[count]] <- anova(fit.FvFm)
names(summary.list)[count] <- paste0(alga, ".FvFm")
names(anova.list)[count] <- paste0(alga, ".FvFm")
count <- count + 1

## Extract anova pvalues
anova.pvalues <-  setNames(data.frame(matrix(ncol = 4, nrow = length(anova.list))), c("Test", "Treatment", "Date", "Interaction"))
for(i in 1:length(anova.list)){
  anova.pvalues$Test[i] <- names(anova.list)[i]
  anova.pvalues$Treatment[i] <- anova.list[[i]]$`Pr(>F)`[1]
  anova.pvalues$Date[i] <- anova.list[[i]]$`Pr(>F)`[2]
  anova.pvalues$Interaction[i] <- anova.list[[i]]$`Pr(>F)`[3]
}
anova.pvalues[, 2:4] <- sapply(anova.pvalues[, 2:4], function(x) round(x, 5))

}
return(list(summary.list= summary.list, anova.list= anova.list, anova.pvalues= anova.pvalues))
}

## Returns the summary output, anova output, and the anova p-values
alga.summary.list <- alga.time.test()$summary.list
alga.anova.list <- alga.time.test()$anova.list
alga.anova.pvalues <- alga.time.test()$anova.pvalues


names(alga.summary.list)
alga.summary.list$Clad.ETRm
alga.anova.list$Clad.FvFm
p.adjust(rep(0.01033, 3))
##### OLD CODE #################################################################

## CLADOPHORA
clad.stats <- rr.stats %>%
                filter(Algae == "Clad") %>%
                mutate(Date = as.factor(Date))

fit.clad.alpha <- lm(pseu.alpha.REG2 ~ Treatment*Date, data= clad.stats)
summary(fit.clad.alpha)
anova(fit.clad.alpha)

fit.clad.ETRm <- lm(pseu.ETRm.REG2 ~ Treatment*Date, data= clad.stats)
mytable <- summary(fit.clad.ETRm)
anova(fit.clad.ETRm)

fit.clad.FvFm <- lm(pseu.Fv.Fm ~ Treatment*Date, data= clad.stats)
summary(fit.clad.FvFm)
anova(fit.clad.FvFm)


