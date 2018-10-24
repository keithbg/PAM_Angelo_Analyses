## Statistics on REG2 light curve parameters from PAM 2014 experiment
## with Yvonne, Paula, and Mary

## Created by KBG January-2016
## Revised by KBG August-2018

#### LIBRARIES #################################################################
library(tidyverse)

################################################################################

#### FILE PATHS ################################################################
dir_input <- file.path("/Users", "KeithBG", "Dropbox", "PAM_Angelo", "2014", "Data")
dir_out_table <- file.path("/Users", "KeithBG", "Dropbox", "PAM_Angelo", "2014", "Data")
################################################################################


#### Read in and format data ####
  rr.stats <- read_tsv(file.path(dir_input, "PAM2014_clean_REG2_response_ratios.tsv")) %>%
                filter(Algae != "Blank") %>%
                select(-Date, -Day, -Treatment, -Rep, -(Alpha:FvFm.d0)) %>%
                rename(Treatment = Location) %>%
                mutate(Algae= ifelse(Algae == "Cyano_Spires", "Ana_Spires",
                                     ifelse(Algae == "Phorm", "Microcoleus", Algae)))


#### STATISTICAL MODEL #########################################################
# Treatment = fixed effect = p = 2 (Benthic and Floating)
# Algae = fixed effect = Algae = j = 3 (Clad_Y, Clad_R, Anabaena Spires, Nostoc, Rivularia, Microcoleus)
# Replicates = Site = n = 3
# Time = 2 different sampling events = t = 2



#### DO ALGAE RESPOND DIFFERENTLY TO THE TREATMENT? ############################
# Tests: Algae, Treatment, and interaction
fit.algae.alpha <- lm(Alpha.rr ~ Treatment*Algae, data= rr.stats)
summary(fit.algae.alpha)
anova(fit.algae.alpha)

fit.algae.ETRm <- lm(ETRm.rr ~ Treatment*Algae, data= rr.stats)
summary(fit.algae.ETRm)
anova(fit.algae.ETRm)

fit.algae.FvFm <- lm(FvFm.rr ~ Treatment*Algae, data= rr.stats)
summary(fit.algae.FvFm)
anova(fit.algae.FvFm)

pvalues <- as.data.frame(matrix(round(c(anova(fit.algae.alpha)[ ,"Pr(>F)"], anova(fit.algae.ETRm)[ ,"Pr(>F)"], anova(fit.algae.FvFm)[ ,"Pr(>F)"]), 5),
                                                nrow= 4,
                                                dimnames= list(c("Treatment", "Algae", "Treatment:Algae", "NA"), c("Alpha", "ETRm", "FvFm"))))[-4, ]


pvalues.corrected <- pvalues %>%
                       mutate_all(funs(round(p.adjust(., method= "bonferroni"), 5))) %>%
                       mutate(Test= c("Treatment", "Algae", "Treatment:Algae")) %>%
                       select(Test, everything())

