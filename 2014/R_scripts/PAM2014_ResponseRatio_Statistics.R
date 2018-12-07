## Statistics on REG2 light curve parameters from PAM 2014 experiment
## with Yvonne, Paula, and Mary
## response ratios calculated in the script: PAM2014_RegParam_Format_Figures.R

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
                mutate(Rep= str_sub(.$uniqueID, start= -1),
                       Algae= ifelse(Algae == "Cyano_Spires", "Ana_Spires",
                                     ifelse(Algae == "Phorm", "Microcoleus", Algae)))

  param.stats <- read_tsv(file.path(dir_input, "PAM2014_clean_REG2_response_ratios.tsv")) %>%
                    filter(Algae != "Blank") %>%
                    select(-Date, -Day, -Treatment, -Rep, -(contains(".rr"))) %>%
                    rename(Treatment = Location) %>%
                    mutate(Rep= str_sub(.$uniqueID, start= -1),
                           Algae= ifelse(Algae == "Cyano_Spires", "Ana_Spires",
                                         ifelse(Algae == "Phorm", "Microcoleus", Algae)))

  ## Lump algae into 3 functional categories:
  ## Chlorophytes= Clad_R and Clad_Y
  ## Motile cyanos= Anabaena and Microcoleus
  ## Non-motile cyanos= Rivularia and Nostoc
  ## Goal to increase degrees of freedom for statistical tests

  func.group.stats <- read_tsv(file.path(dir_input, "PAM2014_clean_REG2_response_ratios.tsv")) %>%
                       filter(Algae != "Blank") %>%
                       select(-Date, -Day, -Treatment, -Rep, -(Alpha:FvFm.d0)) %>%
                       rename(Treatment = Location) %>%
                       mutate(Rep= str_sub(.$uniqueID, start= -1),
                              func_group= ifelse(Algae == "Clad_R" | Algae == "Clad_Y", "Chlorophyte",
                                                 ifelse(Algae == "Cyano_Spires" | Algae == "Phorm", "Cyano_motile", "Cyano_nonmotile")))

#### STATISTICAL MODEL #########################################################
# Treatment = fixed effect = p = 2 (Benthic and Floating)
# Algae = fixed effect = Algae = j = 3 (Clad_Y, Clad_R, Anabaena Spires, Nostoc, Rivularia, Microcoleus)
# Replicates = Site = n = 3
# Time = 2 different sampling events = t = 2



#### DO ALGAE RESPOND DIFFERENTLY TO THE TREATMENT? ############################
  # Tests: Algae, Treatment, and interaction


# CALCULATED WITH RESPONSE RATIOS
fit.algae.alpha.rr <- lm(Alpha.rr ~ Treatment*Algae, data= rr.stats)
summary(fit.algae.alpha.rr)
anova(fit.algae.alpha.rr)

## Calculating paired t-test by hand on Clad_R alpha to confirm R t.test function
## This by-hand calculation matches the output from t.test
  ## Clad_R response ratios for Alpha
  # ben <- c(0.126, -0.258, -0.232)
  # flo <- c(-0.324, -0.513, -0.446)
  # n <- 3
  #
  # mean.difference <- mean(ben - flo)
  # sd.difference <- sd(ben - flo)
  #
  # t.value <- mean.difference / (sd.difference / sqrt(n))
  # p.value <- (1 - pt(t.value, df= 2))*2


fit.algae.ETRm.rr <- lm(ETRm.rr ~ Treatment*Algae, data= rr.stats)
summary(fit.algae.ETRm.rr)
anova(fit.algae.ETRm.rr)


fit.algae.FvFm.rr <- lm(FvFm.rr ~ Treatment*Algae, data= rr.stats)
summary(fit.algae.FvFm.rr)
anova(fit.algae.FvFm.rr)


pvalues.rr <- as.data.frame(matrix(round(c(anova(fit.algae.alpha.rr)[ ,"Pr(>F)"], anova(fit.algae.ETRm.rr)[ ,"Pr(>F)"], anova(fit.algae.FvFm.rr)[ ,"Pr(>F)"]), 5),
                                                nrow= 4,
                                                dimnames= list(c("Treatment", "Algae", "Treatment:Algae", "NA"), c("Alpha", "ETRm", "FvFm"))))[-4, ]



# CALCULATED ON ACTUAL PARAMETER VALUES
# DAY 0
  # Tests: Algae, Treatment, and interaction
  fit.algae.alpha.d0 <- lm(Alpha.d0 ~ Algae*Treatment, data= param.stats)
  summary(fit.algae.alpha.d0)
  anova(fit.algae.alpha.d0)

  fit.algae.ETRm.d0 <- lm(ETRm.d0 ~ Algae*Treatment, data= param.stats)
  summary(fit.algae.ETRm.d0)
  anova(fit.algae.ETRm.d0)

  fit.algae.FvFm.d0 <- lm(FvFm.d0 ~ Algae*Treatment, data= param.stats)
  summary(fit.algae.FvFm.d0)
  anova(fit.algae.FvFm.d0)

# DAY 1
  # Tests: Algae, Treatment, and interaction
  fit.algae.alpha.d1 <- lm(Alpha ~ Algae*Treatment, data= param.stats)
  summary(fit.algae.alpha.d1)
  anova(fit.algae.alpha.d1)

  fit.algae.ETRm.d1 <- lm(ETRm ~ Algae*Treatment, data= param.stats)
  summary(fit.algae.ETRm.d1)
  anova(fit.algae.ETRm.d1)

  fit.algae.FvFm.d1 <- lm(FvFm ~ Algae*Treatment, data= param.stats)
  summary(fit.algae.FvFm.d1)
  anova(fit.algae.FvFm.d1)





#### T-TESTS ############################################################
## Separate t-tests of response ratios for each algae type and each parameter
## Alpha, ETRm, and Fv/Fm
## Conducted both paired and un-paired t-tests

## degrees of freedom: Unpaired= 3 + 3 - 2 = 4
## degrees of freedom: Paired= 3 - 1 = 2

## Initialize data frame

t.test.output <- data.frame(parameter= as.character(NA),
                            algae= as.character(NA),
                            test= as.character(NA),
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
    t.test.output[count, ] <- c(str_replace(param, ".rr", ""), alga, "unpaired", round(output$statistic, 2), round(output$p.value, 4))
    count <- count + 1

    output.paired <- rr.stats %>%
      gather(parameter, value, Alpha.rr:FvFm.rr) %>%
      filter(Algae == alga, parameter== param) %>%
      arrange(Rep) %>%
      t.test(value ~ Treatment,
             paired= TRUE,
             var.equal= FALSE,
             data= .)
    t.test.output[count, ] <- c(str_replace(param, ".rr", ""), alga, "paired", round(output.paired$statistic, 2), round(output.paired$p.value, 4))

    #print(alga)
    #print(output)
    count <- count + 1
  }
}
#t.test.output
#arrange(t.test.output, p_value)
write_tsv(t.test.output, path= file.path(dir_out_table, "t_test_output_2014.tsv"))

## See effect of increasing degrees of freedom on p values, for the given t values
# sum((1 - pt(abs(as.numeric(subset(t.test.output, test == "paired")$t_value)), df= 2))*2 < 0.05)
# sum((1 - pt(abs(as.numeric(subset(t.test.output, test == "paired")$t_value)), df= 3))*2 < 0.05)
# sum((1 - pt(abs(as.numeric(subset(t.test.output, test == "paired")$t_value)), df= 4))*2 < 0.05)
# sum((1 - pt(abs(as.numeric(subset(t.test.output, test == "paired")$t_value)), df= 5))*2 < 0.05)



#### CALCULATE T-TESTS ON THE THREE FUNCTIONAL GROUPS ########################
# Initialize empty data frame
t.test.func.group.output <- data.frame(parameter= as.character(NA),
                            group= as.character(NA),
                            test= as.character(NA),
                            t_value= as.numeric(NA),
                            df= as.numeric(NA),
                            p_value= as.numeric(NA),
                            stringsAsFactors = FALSE)

## Loop over each functional type and parameter, save t and p values in the data frame
count <- 1
for(param in c("Alpha.rr", "ETRm.rr", "FvFm.rr")){
  for(group in unique(func.group.stats$func_group)){
    output.paired <- func.group.stats %>%
      gather(parameter, value, Alpha.rr:FvFm.rr) %>%
      filter(func_group == group, parameter== param) %>%
      arrange(Rep) %>%
      t.test(value ~ Treatment,
             paired= TRUE,
             var.equal= FALSE,
             data= .)

    t.test.func.group.output[count, ] <- c(str_replace(param, ".rr", ""), group, "paired", round(output.paired$statistic, 2), output.paired$parameter, round(output.paired$p.value, 4))

    count <- count + 1
  }
}

write_tsv(t.test.func.group.output, path= file.path(dir_out_table, "t_test_output_func_group_2014.tsv"))


