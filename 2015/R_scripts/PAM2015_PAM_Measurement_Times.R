## Plot Response ratios for the PAM 2015 experiment
## Script created September 2018 by Keith Bouma-Gregson

## Response ratios calculated in PAM2015_ResponseRatios_Format.R
## and exported as a TSV file: PAM_2015_response_ratios_figures.tsv
## data in: /Users/kbg/Dropbox/PAM_Angelo/2015/PAM_data


#### Libraries #################################################################
library(tidyverse)
library(lubridate)
library(ggplot2)
source("ggplot_themes.R") #theme_freshSci
################################################################################


#### FILE PATHS ################################################################
dir_input <- file.path("2015", "PAM_data")
dir_out_fig <- file.path("2015", "Figures")
################################################################################


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
  theme_freshSci
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
  theme_freshSci
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
  theme_freshSci
deploy.times.diff.plot
ggsave(deploy.times.diff.plot, filename = file.path(dir_out_fig, "redeployment_time_diff.pdf"), height= 6.4, width= 8, units= "in")


