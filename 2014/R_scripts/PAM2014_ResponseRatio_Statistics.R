## Statistics on REG2 light curve parameters from PAM 2014 experiment
## with Yvonne, Paula, and Mary

## Created by KBG January-2016
## Revised by KBG August-2018

## Reps 1, 3, 5 = Benthic
## Reps 2, 4, 6 = Floating
## Rep pairs 1&2, 3&4, 5&6

#### LIBRARIES #################################################################
library(tidyverse)
################################################################################

#### FILE PATHS ################################################################
dir_input <- file.path("/Users", "KeithBG", "Dropbox", "PAM_Angelo", "PAM_Angelo_Analyses", "2014", "PAM_data")
dir_out_table <- file.path("/Users", "KeithBG", "Dropbox", "PAM_Angelo", "PAM_Angelo_Analyses", "2014", "PAM_data")
################################################################################


#### Read in and format data ####
  rr.stats <- read_tsv(file.path(dir_input, "PAM2014_clean_REG2_response_ratios.tsv")) %>%
                filter(Algae != "Blank") %>%
                select(-Date, -Day, -Treatment, -Rep, -(Alpha:FvFm.d0)) %>%
                rename(Treatment = Location) %>%
                mutate(Rep= str_sub(rr.stats$uniqueID, start= -1),
                       Algae= ifelse(Algae == "Cyano_Spires", "Ana_Spires",
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

#### T-TESTS ############################################################
## Separate t-tests of response ratios for each algae type and each parameter
## Alpha, ETRm, and Fv/Fm
## These are not paired because we are not comparing the response ratio of one rep to itself
## this is not a repeated measures design, we have already accounted for the repeated measures
## with the response ratio

## degrees of freedom= 3 + 3 - 2= 4

## Initialize data frame

t.test.output <- data.frame(parameter= as.character(NA),
                            algae= as.character(NA),
                            t_value= as.numeric(NA),
                            p_value= as.numeric(NA),
                            stringsAsFactors = FALSE)

## Loop over each algal type and parameter, save t and p values in the data frame
count <- 1
for(param in c("Alpha.rr", "ETRm.rr", "FvFm.rr")){
  for(alga in unique(rr.stats$Algae)){
    output <- rr.stats %>%
      gather(parameter, value, Alpha.rr:FvFm.rr) %>%
      filter(Algae == alga, parameter== param) %>%
      arrange(Rep) %>%
      t.test(value ~ Treatment,
             paired= FALSE,
             var.equal= FALSE,
             data= .)
    t.test.output[count, ] <- c(str_replace(param, ".rr", ""), alga, round(output$statistic, 2), round(output$p.value, 4))

    #print(alga)
    #print(output)
    count <- count + 1
  }
}
#t.test.output
#arrange(t.test.output, p_value)
write_tsv(t.test.output, path= file.path(dir_out_table, "t_test_output_2014.tsv"))

