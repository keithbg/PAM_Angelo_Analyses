## iButton temperature data from the PAM experiment in 2014
## with Yvonne and Paula
## Created by KBG Jan-2016


#### LIBRARIES #################################################################
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
################################################################################

#### FILE PATHS ################################################################
dir_input <- file.path("2014", "PAM_data", "raw_data", "iButton_data")
dir_out_fig <- file.path("2014", "Figures")
dir_out_fig_manuscript <- file.path("..", "Manuscript_Drafts", "Manuscript_Figures")
################################################################################

## Source importing function
source("2014/R_scripts/ibutton_BatchImport_14Oct2015.R")

## Run import function
pam.ib <- ibutton.batch.import(dir_input) %>% 
  as_tibble() %>% 
  filter(DateTimeR >= "2014-07-24 06:00:00" & DateTimeR <= "2014-07-24 16:30:00") %>%
  mutate(ID= str_replace(ID, "^.*data/", "")) %>%
  mutate(DateTimeRound= round_date(DateTimeR, "10 mins"), # round to every 10 minutes
         Location= as.factor(ifelse((ID == "Rep1" | ID == "Rep3" | ID == "Rep5"), "Submerged", "Floating")),
         Rep= ifelse(ID == "Rep1" | ID == "Rep2", "1",
                     ifelse(ID == "Rep3" | ID == "Rep4", "2", "3")))
pam.ib$Location <- factor(pam.ib$Location, levels= levels(pam.ib$Location)[c(2, 1)])


##### PLOTTING PARAMETERS ######################################################
  treatment.color <- c("DodgerBlue", "firebrick2")
  rep.color <- c("DodgerBlue", "limegreen", "purple4")
  treatment.linetype <- c("dashed", "solid")
  treatment.legend <- "Treatment"
  x.axis.labels <- paste0(str_pad(6:17, width = 2, pad= "0"), ":00")
  x.axis.labels <- c("06:00", "", "08:00", "", "10:00", "", "12:00", "", "14:00", "", "16:00", "")

  ## ggplot themes
  source("ggplot_themes.R") # theme_freshSci

##### Plot data #####

  temp.2014 <- ggplot(data= pam.ib, aes(x= DateTimeRound, y= Value, group= ID))

  temp.2014 +
    geom_line(aes(color= Location), size= 0.75) +
    #geom_point(aes(color= Location), size= 1) +
    labs(x="Hour of day (24-Jul-2014)", y=expression('Temperature ('*degree*C*')')) +
    scale_y_continuous(limits= c(18, 28), breaks= seq(18, 28, by= 1), labels= c("18", "", "20", "", "22", "", "24", "", "26", "", "28"), expand= c(0.02, 0)) +
    scale_x_datetime(date_breaks = "1 hour", labels = x.axis.labels, expand= c(0, 0)) +
    #scale_x_datetime(date_breaks = "1 hour", expand= c(0, 0)) +
    scale_color_manual(values= treatment.color, name= treatment.legend) +
    theme_ibutton
  #ggsave(last_plot(), filename = file.path(dir_out_fig, "PAM2014_temperature_plot.pdf"), height= 6.4, width= 8, units= "in")

  
  temp.2014 <- ggplot(data= pam.ib, aes(x= DateTimeRound, y= Value, group= ID))
  
  temp.2014 +
    geom_line(aes(linetype= Location, color= Rep), size= 0.5) +
    #geom_point(aes(color= Location), size= 1) +
    labs(x="Hour of day (24-Jul-2014)", y=expression('Temperature ('*degree*C*')')) +
    scale_y_continuous(limits= c(18, 28), breaks= seq(18, 28, by= 1), labels= c("18", "", "20", "", "22", "", "24", "", "26", "", "28"), expand= c(0.02, 0)) +
    scale_x_datetime(date_breaks = "1 hour", labels = x.axis.labels, expand= c(0, 0)) +
    #scale_color_manual(values= c(treatment.color), name= "Treatment") +
    scale_linetype_manual(values= c("solid", "dashed"), name= treatment.legend) +
    scale_color_manual(values= c(rep.color), name= "Replicate") +
    #theme_ibutton
    theme_freshSci +
    theme(legend.position = c(0.02, 0.98),
          legend.justification = c(0, 1),
          legend.margin = margin(0, 0, 0, 0, unit= "cm"),
          legend.spacing= unit(0.25, "cm"),
          legend.key.size = unit(0.5, "cm"),
          legend.text = element_text(size= 8),
          legend.title = element_text(size= 8),
          legend.direction = "vertical",
          legend.box= "horizontal",
          axis.text.x = element_text(angle= 0, hjust= 0.5))
  #ggsave(last_plot(), filename = file.path(dir_out_fig, "PAM2014_temperature_plot_reps.pdf"), height= 6.4, width= 8, units= "in")
  ggsave(last_plot(), filename = file.path(dir_out_fig, "PAM2014_temperature_plot_reps.eps"), height= 8.4, width= 8.4, units= "cm")
  ggsave(last_plot(), filename = file.path(dir_out_fig_manuscript, "Fig_3.eps"), height= 8.4, width= 8.4, units= "cm")
  



