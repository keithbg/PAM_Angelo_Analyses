## Figures for the PAM 2014 experiment
## with Yvonne, Paula, and Mary

## KBG Oct-2018

## Data formatted in PAM2014_PI_curve_Format.R script

#### LIBRARIES #################################################################
library(tidyverse)
library(ggplot2)
library(lemon)
library(cowplot)
library(extrafont)
#loadfonts(device= "postscript")
################################################################################

#### FILE PATHS ################################################################
dir_input <- file.path("2014", "PAM_data")
dir_out_fig <- file.path("2014", "Figures")
dir_out_fig_manuscript <- file.path("..", "Manuscript_Drafts", "Manuscript_Figures")
################################################################################

lc.df <- read_tsv(file.path(dir_input, "PAM2014_clean_light_curve.tsv")) %>%
           mutate_at(vars(PAR:FvFm), funs(as.numeric(.))) %>% 
           mutate(Location= as.factor(ifelse(Location == "Benthic", "Submerged", "Floating")))
lc.df$Location <- factor(lc.df$Location, levels= levels(lc.df$Location)[c(2, 1)])


## Summarize among the replicates
lc.df.s <- lc.df %>%
  filter(F0_Menupoint == "17") %>%
  group_by(Day, Location, Algae, TreatID, PAR) %>%
  summarise(
    N = length(ETR),
    mean_ETR = mean(ETR),
    sd_ETR = sd(ETR),
    se_ETR = sd_ETR / sqrt(N)) %>% 
  mutate(facet_order= ifelse(Algae == "Clad_R", "2", 
                      ifelse(Algae == "Clad_Y", "1", 
                             ifelse(Algae == "Cyano_Spires", "6", 
                                    ifelse(Algae == "Phorm", "5", 
                                           ifelse(Algae == "Nostoc", "4",
                                                  ifelse(Algae == "Riv", "3", "7")))))))




##### PLOTTING PARAMETERS ######################################################
plot_points_lc <- geom_point(aes(fill= Location, shape= Location),
                          color= "black",
                          size= 2)
plot_errorbars_lc <- geom_errorbar(width= 0.2)
plot_lines_lc <- geom_line(aes(group= Location, linetype= Location), size= 0.4)
yintercept <- geom_hline(yintercept = 0, size= 0.25)
x_axis_format <- scale_x_discrete(breaks= c(0, 1), labels= c("23-Jul", "24-Jul"))
treatment.fill <- c("black", "white")
treatment.linetype <- c("solid", "dashed")
treatment.shapes <- c(21, 21)
treatment.legend <- "Treatment"
ETR.label <- paste("rETRmax (± se)")

day.facet.labels <- as_labeller(c(`0` = "23 July 2014",
                                    `1` = "24 July 2014"))

## ggplot themes
# theme_freshSci
source("ggplot_themes.R")

#### MAKE PLOTS ################################################################


## LIGHT CURVES
lc.p <- ggplot(data= lc.df.s, aes(x= PAR,
                                  y= mean_ETR,
                                  ymax= mean_ETR + se_ETR,
                                  ymin= mean_ETR - se_ETR,
                                  group= Location))

lc.fig <- lc.p +
  plot_lines_lc +
  plot_errorbars_lc +
  plot_points_lc +
  scale_x_continuous(limits= c(0, 1250), expand= c(0.02, 0)) +
  scale_fill_manual(values= treatment.fill, name= treatment.legend) +
  scale_shape_manual(values= treatment.shapes, name= treatment.legend) +
  scale_linetype_manual(values= treatment.linetype, name= treatment.legend) +
  #labs(x=expression(paste("PAR (",mu,"Mols ",m^{-2}," ", s^{-1}, ")")), y= "Relative electron transfer rate (rETR ± SE)") +
  labs(x=expression(paste("Photosynthetic active radiation (",mu,"mol ",m^{2},"/s)")), y= "Relative electron transfer rate (rETR ± SE)") +
  #facet_rep_grid(Algae~Day, labeller= labeller(Algae= algae.facet.labels, Day= day.facet.labels)) +
  facet_rep_grid(facet_order~Day, labeller= labeller(Day= day.facet.labels)) +
  theme_freshSci +
  theme(strip.text.y = element_blank(),
        strip.text.x = element_text(face= "bold", size= 10),
        legend.position = "top",
        legend.margin = margin(0, 0, 0, 0, unit= "cm"),
        legend.key.width = unit(1, "cm"),
        legend.box.spacing = unit(0, "cm"),
        axis.line = element_line(size= 0.25))
lc.fig

lc.fig.anno <- plot_grid(lc.fig, ncol= 1) +
  draw_label(label= expression(paste(italic("Cladophora")," Yellow")), 
                                          x= 0.12, y= 0.85, size= 8, hjust= 0) +
  draw_label(label= expression(paste(italic("Cladophora")," Red")), 
             x= 0.12, y= 0.72, size= 8, hjust= 0) +
  draw_label(label= expression(italic("Rivularia")), 
             x= 0.12, y= 0.59, size= 8, hjust= 0) +
  draw_label(label= expression(italic("Nostoc")), 
             x= 0.12, y= 0.465, size= 8, hjust= 0) +
  draw_label(label= expression(italic("Microcoleus")), 
             x= 0.12, y= .34, size= 8, hjust= 0) +
  draw_label(label= expression(paste(italic("Anabaena")," Spires")), 
             x= 0.12, y= 0.21, size= 8, hjust= 0)
lc.fig.anno

ggsave(lc.fig.anno, filename = file.path(dir_out_fig, "PAM2014_LightCurves.eps"), height= 12.7, width= 12.7, units= "cm")
ggsave(lc.fig.anno, filename = file.path(dir_out_fig_manuscript, "Fig_4.eps"), family= "Arial", height= 12.7, width= 12.7, units= "cm", device= cairo_ps)

#ggsave(last_plot(), filename = file.path(dir_out_fig, "PAM2014_LightCurves.pdf"), height= 8, width= 6.4, units= "in", device = cairo_pdf)


# lc.p +
#   plot_lines_lc +
#   plot_errorbars_lc +
#   plot_points_lc +
#   scale_x_continuous(limits= c(0, 1250), expand= c(0.02, 0)) +
#   scale_fill_manual(values= treatment.fill, name= treatment.legend) +
#   scale_shape_manual(values= treatment.shapes, name= treatment.legend) +
#   scale_linetype_manual(values= treatment.linetype, name= treatment.legend) +
#   labs(x=expression(paste("PAR (",mu,"Mols ",m^{-2}," ", s^{-1}, ")")), y="rETR (± se)") +
#   facet_grid(Algae~Day, labeller= labeller(Algae= algae.facet.labels, Day= day.facet.labels)) +
#   theme_pam
# 
