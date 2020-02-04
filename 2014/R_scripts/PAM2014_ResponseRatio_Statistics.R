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
library(lsmeans)
################################################################################

#### FILE PATHS ################################################################
dir_input <- file.path("2014", "PAM_data")
dir_out_table <- file.path("2014", "PAM_data")
################################################################################


#### Read in and format data ####
  rr.stats <- read_tsv(file.path(dir_input, "PAM2014_clean_REG2_response_ratios.tsv")) %>%
                filter(Algae != "Blank") %>%
                select(-Date, -Day, -Treatment, -Rep, -(Alpha:FvFm.d0)) %>%
                rename(Treatment = Location) %>%
                mutate(Rep= str_sub(.$uniqueID, start= -1),
                       Algae= ifelse(Algae == "Cyano_Spires", "Ana_Spires",
                                     ifelse(Algae == "Phorm", "Microcoleus", Algae))) %>% 
                mutate(Algae= as.factor(Algae),
                       Treatment= as.factor(Treatment))
  rr.stats$Treatment <- factor(rr.stats$Treatment, levels= levels(rr.stats$Treatment)[c(2, 1)])


  param.stats <- read_tsv(file.path(dir_input, "PAM2014_clean_REG2_response_ratios.tsv")) %>%
                    filter(Algae != "Blank") %>%
                    select(-Date, -Day, -Treatment, -Rep, -(contains(".rr"))) %>%
                    rename(Treatment = Location) %>%
                    mutate(Rep= str_sub(.$uniqueID, start= -1),
                           Algae= ifelse(Algae == "Cyano_Spires", "Ana_Spires",
                                         ifelse(Algae == "Phorm", "Microcoleus", Algae)))
  param.stats$Treatment <- factor(param.stats$Treatment, levels= levels(param.stats$Treatment)[c(2, 1)])
  
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
  func.group.stats$Treatment <- factor(func.group.stats$Treatment, levels= levels(func.group.stats$Treatment)[c(2, 1)])
  
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



fit.algae.ETRm.rr <- lm(ETRm.rr ~ Treatment*Algae, data= rr.stats)
summary(fit.algae.ETRm.rr)
anova(fit.algae.ETRm.rr)


fit.algae.FvFm.rr <- lm(FvFm.rr ~ Treatment*Algae, data= rr.stats)
summary(fit.algae.FvFm.rr)
anova(fit.algae.FvFm.rr)




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


#### PLANNED CONTRASTS ##################################################

# There are 6 factor levels in the variable "Algae", so we can make 5 orthogonal contrasts
# Orthogonal means you cannot make the same contrast multiple times, 
# nor can you make an implicit contrast that is the result of prior contrasts. 
# Therefore, there are k-1 orthogonal contrasts where, k is the number of levels in a variable
# Constraints on contrast matrix to ensure orthogonality
  # 1) The sum of values within a contrast must = zero
  # 2) The sumn of the products of any two columns in the contrast matrix must = zero

# Degrees of freedom for T-test are df = N-k
# N = number of samples included in test = 36
# k = number of comparisons in our model = 11
# df = 36 - 11 = 24

# We will contrast:
# c1: Cladophora vs. all the cyanobacteria
# c2: Motile cyanos (Anabaena & Microcoleus) vs. non-motile cyanos (Nostoc & Rivularia)
# c3: Yellow Cladophora vs. Red Cladophora
# c4: Anabaena vs. Microcoleus
# c5: Nostoc vs. Rivularia



# Make new data frame to assign new contrasts for the Algae variable
pc.stats <- rr.stats
contrasts(pc.stats$Algae)
levels(pc.stats$Algae)

# Create new contrast matrix
clad.cyano.contrasts <- cbind(c(1, -2, -2, 1, 1, 1), # Cladophora vs. all cyanos
                              c(-1, 0, 0, -1, 1, 1), # Motile vs. non motile
                              c(0, 1, -1, 0, 0, 0), # Clad_R vs. Clad_Y
                              c(1, 0, 0, -1, 0, 0), # Anabaena vs. Microcoleus
                              c(0, 0, 0, 0, 1, -1)) # Nostoc vs. Rivularia


colnames(clad.cyano.contrasts) <- c("Clad.v.Cyano", "Motile.v.NonMotile", "CladR.v.CladY", "Ana.v.Micro", "Nostoc.v.Riv")
aov.contrast.names <- list(Algae=list("Clad.v.Cyano"=1, "Motile.v.NonMotile" = 2, "CladR.v.CladY"=3, "Ana.v.Micro"=4, "Nostoc.v.Riv"=5))

# 
# pc.stats %>% 
#   group_by(Algae) %>% 
#   summarize(mean(Alpha.rr))
# 
# mean(filter(pc.stats, Treatment == "Floating")$Alpha.rr)
# 
# out <- lm(Alpha.rr ~ Algae * Treatment, data= pc.stats)
# summary(out)
# summary.aov(out, split=list(Algae=list("Clad.v.Cyano"=1, "Motile.v.NonMotile" = 2, "CladR.v.CladY"=3, "Ana.v.Micro"=4, "Nostoc.v.Riv"=5))) 
# 
# 
# 
# sout <- summary(lm(Alpha.rr ~ Algae, data= pc.stats))
# 
# cfs <- coef(sout)[, 1]
# 
# cfs[1] + sum(cfs[-1]*clad.cyano.contrasts[6, ])
# 
# 
# summary.aov(lm(Alpha.rr ~ Algae, data= pc.stats))
# 
# 
# 
# summary.aov(model1, split=list(animal=list("Canines vs. Felines"=1, "Cougars vs House Cats" = 2, "Wolves vs Dogs"=3))) 


# Check to make sure product of columns sums to zero, which means that comparisons are orthogonal
# sum(clad.cyano.contrasts[, 1] * clad.cyano.contrasts[, 2])

# Assign the contrast matrix
contrasts(pc.stats$Algae) <- clad.cyano.contrasts

# Run statistical models with the new contrasts
pc.alpha.lm <- lm(Alpha.rr ~ Treatment*Algae, data= pc.stats)
pc.alpha.sum <- summary(pc.alpha.lm)
summary.aov(pc.alpha.lm, split= aov.contrast.names)


summary(fit.algae.alpha.rr) # Can check results with the default treatment contrast matrix

# ANOVA tables between default and new contrast give same results, 
# so our new contrast matrix is performing as expected
# anova(pc.alpha.lm)
# anova(fit.algae.alpha.rr)

pc.fvfm.lm <- lm(FvFm.rr ~ Treatment*Algae, data= pc.stats)
pc.fvfm.sum <- summary(pc.fvfm.lm)
summary.aov(pc.fvfm.lm, split= aov.contrast.names)

pc.ETRm.lm <- lm(ETRm.rr ~ Treatment*Algae, data= pc.stats)
pc.ETRm.sum <- summary(pc.ETRm.lm)
summary.aov(pc.ETRm.lm, split= aov.contrast.names)

#anova(pc.ETRm.lm)


## Manually calculate means of different Alpha.rr contrasts to check the planned contrasts results

pc.stats$c1 <- ifelse(pc.stats$Algae == "Clad_Y" | pc.stats$Algae == "Clad_R", "Clad", "Cyano")
pc.stats$c2 <- ifelse(pc.stats$Algae == "Ana_Spires" | pc.stats$Algae == "Microcoleus", 
                      "Motile", pc.stats$c1)


treatment.means <- tapply(pc.stats$Alpha.rr, pc.stats$Treatment, mean)
algae.means <- tapply(pc.stats$Alpha.rr, list(pc.stats$Treatment, pc.stats$Algae), mean)
c1.means <- tapply(pc.stats$Alpha.rr, list(pc.stats$Treatment, pc.stats$c1), mean)
c2.means <- tapply(pc.stats$Alpha.rr, list(pc.stats$Treatment, pc.stats$c2), mean)


## C1 Interpretation
## The difference of mean cyanobacterial response from the overall mean
-(treatment.means[1] - c1.means[1, 2])
pc.alpha.sum$coefficients[3, 1] # Manual calculation matches statistical output

## C2 Interpretation
## The difference between the Motile and Non-motile cyano means, divided by 2
(c2.means[1, 2] - c2.means[1, 3]) / 2
pc.alpha.sum$coefficients[4, 1] # Manual calculation matches statistical output


#### T-TESTS ############################################################
## Separate t-tests of response ratios for each algae type and each parameter
## Alpha, ETRm, and Fv/Fm
## Conducted both paired and un-paired t-tests

## degrees of freedom: Unpaired= 3 + 3 - 2 = 4
## degrees of freedom: Paired= 3 - 1 = 2


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
param= "Alpha.rr"
for(param in c("Alpha.rr", "ETRm.rr", "FvFm.rr")){
  for(group in unique(func.group.stats$func_group)){
    output.paired <- func.group.stats %>%
      gather(parameter, value, Alpha.rr:FvFm.rr) %>%
      #filter(func_group == group, parameter== param) %>%
      filter(parameter== param) %>%
      arrange(Rep) %>%
      t.test(value ~ Treatment,
             paired= TRUE,
             var.equal= FALSE,
             data= .)

    t.test.func.group.output[count, ] <- c(str_replace(param, ".rr", ""), group, "paired", round(output.paired$statistic, 2), output.paired$parameter, round(output.paired$p.value, 4))

    count <- count + 1
  }
}

#write_tsv(t.test.func.group.output, path= file.path(dir_out_table, "t_test_output_func_group_2014.tsv"))


new.df <- func.group.stats %>%
  gather(parameter, value, Alpha.rr:FvFm.rr) %>%
  #filter(func_group == group, parameter== param) %>%
  filter(parameter== param) %>%
  arrange(Rep)

summary(lm(param ~ func_group, data= new.df))
