## Plot Response ratios for the PAM 2015 experiment
## Script created September 2018 by Keith Bouma-Gregson

## Response ratios calculated in PAM2015_ResponseRatios_Format.R
## and exported as a TSV file: PAM_2015_response_ratios_figures.tsv
## data in: /Users/KeithBG/Dropbox/PAM_Angelo/2015/PAM_data


#### Libraries #################################################################
library(tidyverse)
library(lubridate)
library(ggplot2)
################################################################################


#### FILE PATHS ################################################################
dir_input <- file.path("/Users","KeithBG","Dropbox","PAM_Angelo","2015", "PAM_data")
dir_out_fig <- file.path("/Users","KeithBG","Dropbox","PAM_Angelo","2015", "Figures_Keith_2015")
################################################################################


#### GGPLOT THEMES #############################################################
theme_pam <- theme(panel.grid = element_blank(),
                   plot.margin = unit(c(1, 1, 1, 1), "cm"),
                   text = element_text(size= 14),
                   plot.background = element_rect(fill = "transparent"), # bg of the plot
                   panel.background = element_rect(fill= "transparent", color="black"),
                   axis.text = element_text(colour="black"),
                   axis.title.x = element_text(vjust = -0.75),
                   axis.title.y = element_text(vjust = 1.5),
                   legend.background = element_rect(size=0.25, color="black", fill= "transparent"),
                   legend.key = element_blank(),
                   strip.background=element_rect(fill="transparent", color="transparent"),
                   axis.text.x = element_text(angle= 45, hjust= 1),
                   legend.position = "top")



#### FORMAT AND PLOT DATA ###################################################
pam.times <- read_tsv(file.path(dir_input, "PAM2015_measurement_times.tsv")) %>%
  mutate(Date= mdy(Date),
         Day= paste0("d", yday(Date) - yday(unique(Date)[1])))

dark.times <- pam.times %>%
                select(ArrayID, dark, Day) %>%
                spread(Day, dark)

taken.times <- pam.times %>%
  select(ArrayID, taken, Day) %>%
  spread(Day, taken)

deploy.times <- pam.times %>%
  select(ArrayID, deploy, Day) %>%
  spread(Day, deploy)

## Max and min measurement times each day

pam.times %>%
  group_by(Date) %>%
  summarize(
    first_PAM= min(dark),
    last_PAM= max(dark))

## Calculate the different times dark incubations were started on the different days
dark.times.diff.plot <- dark.times %>%
  mutate(d1_d0= as.numeric( (d1 - d0) / 3600),
         d2_d0= as.numeric( (d2 - d0) / 3600),
         d6_d0= as.numeric( (d6 - d0) / 3600),
         d2_d1= as.numeric( (d2 - d1) / 3600),
         d6_d1= as.numeric( (d6 - d1) / 3600),
         d6_d2= as.numeric( (d6 - d2) / 3600)) %>%
  gather(key= day_diff, value= hours, d1_d0:d6_d2) %>%
  select(ArrayID, day_diff, hours) %>%
  ggplot(aes(x= day_diff, y= hours)) +
  geom_hline(yintercept = 0, size= 1) +
  geom_boxplot() +
  ggtitle("Difference in dark incubation start times between days") +
  theme_pam
dark.times.diff.plot
ggsave(dark.times.diff.plot, filename = file.path(dir_out_fig, "dark_incubation_start_diff.pdf"), height= 6.4, width= 8, units= "in")

taken.times.diff.plot <- taken.times %>%
  mutate(d1_d0= as.numeric( (d1 - d0) / 3600),
         d2_d0= as.numeric( (d2 - d0) / 3600),
         d6_d0= as.numeric( (d6 - d0) / 3600),
         d2_d1= as.numeric( (d2 - d1) / 3600),
         d6_d1= as.numeric( (d6 - d1) / 3600),
         d6_d2= as.numeric( (d6 - d2) / 3600)) %>%
  gather(key= day_diff, value= hours, d1_d0:d6_d2) %>%
  select(ArrayID, day_diff, hours) %>%
  ggplot(aes(x= day_diff, y= hours)) +
  geom_hline(yintercept = 0, size= 1) +
  geom_boxplot() +
  ggtitle("Difference in measurement times between days") +
  theme_pam
taken.times.diff.plot
ggsave(taken.times.diff.plot, filename = file.path(dir_out_fig, "measurement_time_diff.pdf"), height= 6.4, width= 8, units= "in")



deploy.times.diff.plot <- deploy.times %>%
  mutate(d1_d0= as.numeric( (d1 - d0) / 3600),
         d2_d0= as.numeric( (d2 - d0) / 3600),
         d6_d0= as.numeric( (d6 - d0) / 3600),
         d2_d1= as.numeric( (d2 - d1) / 3600),
         d6_d1= as.numeric( (d6 - d1) / 3600),
         d6_d2= as.numeric( (d6 - d2) / 3600)) %>%
  gather(key= day_diff, value= hours, d1_d0:d6_d2) %>%
  select(ArrayID, day_diff, hours) %>%
  ggplot(aes(x= day_diff, y= hours)) +
  geom_hline(yintercept = 0, size= 1) +
  geom_boxplot() +
  ggtitle("Difference in re-deployment times between days") +
  theme_pam
deploy.times.diff.plot
ggsave(deploy.times.diff.plot, filename = file.path(dir_out_fig, "redeployment_time_diff.pdf"), height= 6.4, width= 8, units= "in")


