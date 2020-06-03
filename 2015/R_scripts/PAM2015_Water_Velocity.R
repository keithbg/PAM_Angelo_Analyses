## Plot water velocity from PAM 2015 experiment
## Water velocity measured with a Pygmy flow meter
## Depth in centimeters; velocity in meters / second


#### Libraries #################################################################
library(tidyverse)
library(lubridate)
library(ggplot2)
library(lemon)
################################################################################


#### FILE PATHS ################################################################
dir_input <- file.path("2015", "PAM_data")
dir_out_fig <- file.path("2015", "Figures")
################################################################################

#### READ IN AND FORMAT DATA ##############################################################
vel.df <- read_tsv(file.path(dir_input, "PAM2105_water_velocity_data.tsv")) %>%
            mutate(Date= dmy(Date),
                   Day= as.character(Date),
                   velocity_binary= ifelse(velocity_ms > 0, 1, 0)) %>%
            left_join(., read_tsv(file.path(dir_input, "arrayID_treatment_table.tsv")))



##### PLOTTING PARAMETERS ######################################################
# theme_freshSci
source("ggplot_themes.R")

yintercept <- geom_hline(yintercept = 0, size= 0.25)
treatment.order <- c("Thal", "Marg")
treatment.labels <- c("Thalweg", "Margin")
treatment.fill <- c("white", "black")
treatment.col <- c("white", "black")
treatment.shapes <- c(21, 21)


#### MAKE PLOTS ################################################################

vel.plot <- ggplot(data= vel.df, aes(x= rev(Treatment), y= velocity_ms * 100))

velocity.2015.fig <- vel.plot +
  yintercept +
  geom_boxplot(aes(), outlier.color = "transparent") +
  geom_point(aes(fill= Treatment, shape= Treatment), position= position_jitter(width= 0.2), size= 1) +
  labs(x= "Treatment", y= expression(paste("Water velocity (cm ", s^-1, ")"))) +
  scale_fill_manual(values= treatment.fill, breaks= treatment.order, labels= treatment.labels, guide= FALSE) +
  scale_shape_manual(values= treatment.shapes, breaks= treatment.order, labels= treatment.labels, guide= FALSE) +
  scale_x_discrete(breaks= c("Thal", "Marg"), labels= c("Margin", "Thalweg")) +
  scale_y_continuous(expand= c(0.02, 0)) +
  theme_freshSci
velocity.2015.fig

#ggsave(velocity.2015.fig, filename = file.path(dir_out_fig, "water_velocity_plot.pdf"), height= 6.4, width= 8, units= "in", device= cairo_pdf)
ggsave(velocity.2015.fig, filename = file.path(dir_out_fig, "water_velocity_plot.eps"), height= 8.4, width= 8.4, units= "cm")



#### STATISTICS ################################################################

## Non-parametric test of treatment effect on velocity for each day
## Result: Highly significant effect of treatment on velocity

#wilcox.test(velocity_ms ~ Treatment, data= subset(vel.df, Day == "2015-06-10"))
#wilcox.test(velocity_ms ~ Treatment, data= subset(vel.df, Day == "2015-06-11"))
#wilcox.test(velocity_ms ~ Treatment, data= subset(vel.df, Day == "2015-06-15"))
