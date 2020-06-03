## Plot ibutton water temperature data for PAM 2015 experiment
## ibuttons were deployed on each array: 3 arrays per site per treatment
## ibuttons put in water the afternoon of June 9 and
## and removed after the last PAM measurement on the afternoon of June 15
## ibuttons recorded temperature in Celsius every 10 minutes

## data in: 2015/PAM_data/ibutton_data

#### Libraries #################################################################
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(lme4)
library(lmerTest)
library(lemon)
library(ggpubr)
source("ibutton_BatchImport_20151014.R")
################################################################################


#### FILE PATHS ################################################################
dir_input <- file.path("2015", "PAM_data")
dir_out_fig <- file.path("2015", "Figures")
dir_out_fig_manuscript <- file.path("..", "Manuscript_Drafts", "Manuscript_Figures")
dir_out_table <- file.path("2015", "PAM_data")
################################################################################

#### READ IN AND FORMAT DATA ###################################################

## Pathway where ibutton files are located
import.pathway <- file.path(dir_input, "ibutton_data")

## Run import function and format data frame
pam.ib <- ibutton.batch.import(import.pathway) %>%
  as_tibble() %>%
  filter(DateTimeR >= "2015-06-10 00:00:00" & DateTimeR <= "2015-06-15 17:00:00") %>% # filter by times the iButtons were in the water
  mutate(ID= str_replace(ID, "^.*ibutton_data\\/", "")) %>% 
  mutate(ArrayID= str_replace(ID, "sc", ""),
         Site= str_replace(ArrayID, ".[0-9]", ""),
         Pseudorep= str_replace(ID, "sc[0-9].", ""),
         Treatment= ifelse(Pseudorep == "1" | Pseudorep == "2" | Pseudorep == "3", "Thalweg",
                           ifelse(Pseudorep == "4" | Pseudorep == "5" | Pseudorep == "6", "Margin", "cooler"))) %>%
  select(-ID, -DateTime)

## Summarize for plotting
pam.ib.s <- pam.ib %>%
             filter(Site != "cooler") %>%
             mutate(DateTimeRound= round_date(DateTimeR, "30 mins")) %>%  # round to every 30 minutes
             group_by(DateTimeRound, Site, Treatment) %>%
             summarize(
               N = length(Value),
               mean.t = mean(Value),
               min.t = min(Value),
               max.t = max(Value),
               sd.t = sd(Value),
               se.t = sd.t/sqrt(N))


## Summarize for statistics
pam.ib.stats <- pam.ib %>%
  filter(Site != "cooler") %>%
  group_by(DOY, Site, Treatment) %>%
  summarize(
    N = length(Value),
    mean.t = mean(Value),
    min.t = min(Value),
    max.t = max(Value),
    sd.t = sd(Value),
    se.t = sd.t/sqrt(N)) %>%
  ungroup() %>%
  rename(Date = DOY)
#write_tsv(pam.ib.stats, path= file.path(dir_out_table, "PAM2015_temperature_daily_summary.tsv"))


## Calculate daily min, max, and mean between all 4 sites
pam.ib.stats %>% 
  group_by(Treatment) %>% 
  summarize(
    N = length(min.t),
    mean.min.t = mean(min.t),
    min.min.t = min(min.t),
    max.min.t = max(min.t),
    sd.min.t = sd(min.t)
  )

pam.ib.stats %>% 
  group_by(Treatment) %>% 
  summarize(
    N = length(max.t),
    mean.max.t = mean(max.t),
    max.max.t = max(max.t),
    max.max.t = max(max.t),
    sd.max.t = sd(max.t)
  )

pam.ib.stats %>% 
  group_by(Treatment) %>% 
  summarize(
    N = length(max.t),
    mean.mean.t = mean(mean.t),
    max.mean.t = max(mean.t),
    max.mean.t = max(mean.t),
    sd.mean.t = sd(mean.t)
  )




#### STATISTICS ################################################################

## Removed site 4 because its temperature behaved differently than the other 3 sites
## Site 4 had clearer patterns

fit.mean.thalweg <- lm(mean.t ~ Site, data= filter(pam.ib.stats, Treatment == "Thalweg"))
summary(fit.mean.thalweg)
anova(fit.mean.thalweg)
TukeyHSD(aov(mean.t ~ Site, data= filter(pam.ib.stats, Treatment == "Thalweg")))


fit.mean.margin <- lm(mean.t ~ Site, data= filter(pam.ib.stats, Treatment == "Margin"))
summary(fit.mean.margin)
anova(fit.mean.margin)
TukeyHSD(aov(mean.t ~ Site, data= filter(pam.ib.stats, Treatment == "Margin")))

fit.max.t <- lmer(max.t ~ Treatment + (1|Site), data= subset(pam.ib.stats, Site != "4"))
#fit.max.t <- lmer(max.t ~ Treatment + (1|Site), data= pam.ib.stats)
summary(fit.max.t)

fit.mean.t <- lmer(mean.t ~ Treatment + (1|Site), data= subset(pam.ib.stats, Site != "4"))
#fit.mean.t <- lmer(mean.t ~ Treatment + (1|Site), data= pam.ib.stats) # check for significance if including Site 4
summary(fit.mean.t)
anova(fit.mean.t)

fit.min.t <- lmer(min.t ~ Treatment + (1|Site), data= subset(pam.ib.stats, Site != "4"))
summary(fit.min.t)
anova(fit.min.t)$"Pr(>F)"

p.adjust(c(anova(fit.max.t)$"Pr(>F)", anova(fit.mean.t)$"Pr(>F)", anova(fit.min.t)$"Pr(>F)"))

plot(max.t ~ as.factor(Treatment), data= subset(pam.ib.stats, Site != "4"))
plot(mean.t ~ as.factor(Treatment), data= subset(pam.ib.stats, Site != "4"))
plot(min.t ~ as.factor(Treatment), data= subset(pam.ib.stats, Site != "4"))


##### PLOTTING PARAMETERS ######################################################
treatment.order <- c("Thalweg", "Margin")
treatment.labels <- c("Thalweg", "Margin")
treatment.fill <- c("firebrick2", "DodgerBlue")
site.facet.labels <- as_labeller(c(`1` = "Site 1", `2` = "Site 2", `3` = "Site 3", `4`= "Site 4"))
facet.by.site <- facet_grid(Site~., labeller = labeller(Site= site.facet.labels))

## ggplot themes
# theme_freshSci
source("ggplot_themes.R")


#### MAKE PLOTS ################################################################

temp.plot <- ggplot(data= pam.ib.s, aes(x= DateTimeRound,
                                         y= mean.t,
                                         ymin= min.t,
                                         ymax= max.t))

temp.2015.fig <- temp.plot +
  geom_ribbon(aes(fill= Treatment), alpha= 0.5) +
  labs(x="", y=expression('Temperature ('*degree*C*')')) +
  scale_y_continuous(limits= c(15, 33), labels= c("15", "20", "25", "30", "")) +
  scale_x_datetime(date_breaks = "1 day", labels = date_format("%d-%b"), expand= c(0, 0)) +
  scale_fill_manual(values= treatment.fill, breaks= treatment.order, labels= treatment.labels) +
  #facet.by.site +
  facet_rep_wrap(~Site, ncol= 1, labeller = labeller(Site= site.facet.labels)) +
  theme_freshSci +
  theme(legend.position = "top",
        legend.box.margin = margin(0, 0, 0, 0, unit= "cm"),
        legend.box.spacing = unit(0, "cm"),
        legend.key.size = unit(0.25, "cm"),
        axis.title.x = element_blank())
temp.2015.fig
  
#ggsave(last_plot(), filename = file.path(dir_out_fig, "temperature_plot_PAM2015.pdf"), height= 6.4, width= 8, units= "in")
ggsave(last_plot(), filename = file.path(dir_out_fig, "temperature_plot_PAM2015.eps"), height= 12, width= 8.4, units= "cm", device= cairo_ps)


## COMBINE TEMP AND WATER VELOCITY INTO A MULTIPANEL FIGURE
setwd("/Users/kbg/Dropbox/PAM_Angelo/PAM_Angelo_Analyses")
source("2015/R_scripts/PAM2015_Water_Velocity.R")

vel.temp.fig <- ggarrange(velocity.2015.fig, temp.2015.fig, 
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2,
                    heights= c((8.4/12*0.5), 1-(8.4/12*0.5)))
ggsave(vel.temp.fig, filename = file.path(dir_out_fig, "vel_temp_PAM2015.png"), height= 20.4, width= 8.4, units= "cm")
ggsave(vel.temp.fig, filename = file.path(dir_out_fig, "vel_temp_PAM2015.eps"), height= 20.4, width= 8.4, units= "cm", device= cairo_ps)

ggsave(vel.temp.fig, filename = file.path(dir_out_fig_manuscript, "Fig_6.eps"), height= 20.4, width= 8.4, units= "cm", device= cairo_ps)




