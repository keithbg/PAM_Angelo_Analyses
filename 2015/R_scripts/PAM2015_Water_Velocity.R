## Plot water velocity from PAM 2015 experiment
## Water velocity measured with a Pygmy flow meter
## Depth in centimeters; velocity in meters / second

## data in: /Users/kbg/Dropbox/PAM_Angelo/2015/PAM_data


#### Libraries #################################################################
library(tidyverse)
library(lubridate)
library(ggplot2)
################################################################################


#### FILE PATHS ################################################################
dir_input <- file.path("/Users","kbg","Dropbox","PAM_Angelo", "PAM_Angelo_Analyses", "2015", "PAM_data")
dir_out_fig <- file.path("/Users","kbg","Dropbox","PAM_Angelo", "PAM_Angelo_Analyses", "2015", "Figures")
################################################################################

#### READ IN AND FORMAT DATA ##############################################################
vel.df <- read_tsv(file.path(dir_input, "PAM2105_water_velocity_data.tsv")) %>%
            mutate(Date= dmy(Date),
                   Day= as.character(Date),
                   velocity_binary= ifelse(velocity_ms > 0, 1, 0)) %>%
            left_join(., read_tsv(file.path(dir_input, "arrayID_treatment_table.tsv")))



##### PLOTTING PARAMETERS ######################################################
yintercept <- geom_hline(yintercept = 0, size= 0.25)
treatment.fill <- c("white", "black")
treatment.col <- c("white", "black")
treatment.shapes <- c(21, 21)

## ggplot themes
theme_velocity <- theme(panel.grid = element_blank(),
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
                   legend.position = "top")


#### MAKE PLOTS ################################################################

vel.plot1 <- ggplot(data= vel.df, aes(x= Date, y= velocity_ms * 100, group= interaction(Treatment, Date)))

vel.plot1 +
  geom_boxplot(aes()) +
  geom_point(aes(fill= Treatment, shape= Treatment), position= position_dodge(width= 0.5)) +
  x_axis_format +
  scale_fill_manual(values= treatment.fill) +
  scale_shape_manual(values= treatment.shapes) +
  scale_color_manual(values= treatment.col) +
  theme_pam


vel.plot2 <- ggplot(data= vel.df, aes(x= Treatment, y= velocity_ms * 100))

vel.plot2 +
  yintercept +
  geom_boxplot(aes()) +
  geom_point(aes(fill= Treatment, shape= Treatment), position= position_jitter(width= 0.2), size= 3) +
  labs(x= "Treatment", y= "Water velocity (cm/s)") +
  scale_fill_manual(values= treatment.fill) +
  scale_shape_manual(values= treatment.shapes) +
  scale_x_discrete(labels= c("Margin", "Thalweg")) +
  theme_velocity
ggsave(last_plot(), filename = file.path(dir_out_fig, "water_velocity_plot.pdf"), height= 6.4, width= 8, units= "in", device= cairo_pdf)



#### STATISTICS ################################################################

## Non-parametric test of treatment effect on velocity for each day
## Result: Highly significant effect of treatment on velocity

wilcox.test(velocity_ms ~ Treatment, data= subset(vel.df, Day == "2015-06-10"))
wilcox.test(velocity_ms ~ Treatment, data= subset(vel.df, Day == "2015-06-11"))
wilcox.test(velocity_ms ~ Treatment, data= subset(vel.df, Day == "2015-06-15"))
