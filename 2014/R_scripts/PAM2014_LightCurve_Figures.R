## Figures for the PAM 2014 experiment
## with Yvonne, Paula, and Mary

## KBG Oct-2018

## Data formatted in PAM2014_PI_curve_Format.R script

#### LIBRARIES #################################################################
library(tidyverse)
library(ggplot2)
################################################################################

#### FILE PATHS ################################################################
dir_input <- file.path("/Users", "kbg", "Dropbox", "PAM_Angelo", "PAM_Angelo_Analyses", "2014", "PAM_data")
dir_out_fig <- file.path("/Users", "kbg", "Dropbox", "PAM_Angelo", "PAM_Angelo_Analyses", "2014", "Figures")
################################################################################

lc.df <- read_tsv(file.path(dir_input, "PAM2014_clean_light_curve2.tsv")) %>%
           mutate_at(vars(PAR:FvFm), funs(as.numeric(.)))

## Summarize among the replicates
lc.df.s <- lc.df %>%
  filter(F0_Menupoint == "17") %>%
  group_by(Day, Location, Algae, TreatID, PAR) %>%
  summarise(
    N = length(ETR),
    mean_ETR = mean(ETR),
    sd_ETR = sd(ETR),
    se_ETR = sd_ETR / sqrt(N))




##### PLOTTING PARAMETERS ######################################################
plot_points_lc <- geom_point(aes(fill= Location, shape= Location),
                          color= "black",
                          size= 2)
plot_errorbars_lc <- geom_errorbar(width= 0.2)
plot_lines_lc <- geom_line(aes(group= Location, linetype= Location), size= 0.5)
yintercept <- geom_hline(yintercept = 0, size= 0.25)
x_axis_format <- scale_x_discrete(breaks= c(0, 1), labels= c("23-Jul", "24-Jul"))
treatment.fill <- c("black", "white")
treatment.linetype <- c("solid", "dashed")
treatment.shapes <- c(21, 21)

ETR.label <- paste("Mean ETRmax (± se)")


algae.facet.labels <- as_labeller(c(`Cyano_Spires` = "Anabaena\nSpires",
                                    `Nostoc` = "Nostoc",
                                    `Phorm` = "Microcoleus",
                                    `Riv` = "Rivularia",
                                    `Clad_R` = "Cladophora\nRed",
                                    `Clad_Y` = "Cladophora\nYellow"))

day.facet.labels <- as_labeller(c(`0` = "23-Jul",
                                    `1` = "24-Jul"))

## ggplot themes
theme_pam <- theme(panel.grid = element_blank(),
                   plot.margin = unit(c(1, 1, 1, 1), "cm"),
                   text = element_text(size= 14),
                   plot.background = element_rect(fill = "transparent", color= "transparent"), # bg of the plot
                   panel.background = element_rect(fill= "transparent", color= "transparent"),
                   panel.border= element_rect(fill= NA, color= "black", linetype= "solid", size= 1),
                   panel.ontop = TRUE,
                   axis.text = element_text(colour="black"),
                   axis.title.x = element_text(vjust = -0.75),
                   axis.title.y = element_text(vjust = 1.5),
                   legend.background = element_rect(size=0.25, color="black", fill= "transparent"),
                   legend.key = element_blank(),
                   strip.background = element_rect(fill="transparent", color= "transparent"),
                   axis.text.x = element_text(angle= 45, hjust= 1),
                   legend.position = "top")
unique(lc.df.s$Algae)

#### MAKE PLOTS ################################################################


## LIGHT CURVES
lc.p <- ggplot(data= lc.df.s, aes(x= PAR,
                                  y= mean_ETR,
                                  ymax= mean_ETR + se_ETR,
                                  ymin= mean_ETR - se_ETR,
                                  group= Location))

lc.p +
  plot_lines_lc +
  plot_errorbars_lc +
  plot_points_lc +
  #scale_x_continuous(limits= c(0, 1250), expand= c(0.02, 0)) +
  scale_fill_manual(values= treatment.fill) +
  scale_shape_manual(values= treatment.shapes) +
  scale_linetype_manual(values= treatment.linetype) +
  labs(x=expression(paste("PAR (",mu,"Mols ",m^{-2}," ", s^{-1}, ")")), y="Mean relative electron transport rate (± se)") +
  facet_grid(Algae~Day, labeller= labeller(Algae= algae.facet.labels, Day= day.facet.labels)) +
  theme_pam
ggsave(last_plot(), filename = file.path(dir_out_fig, "PAM2014_LightCurves_all_Irradiance.pdf"), height= 8, width= 6.4, units= "in", device = cairo_pdf)


