## Plot Response ratios for the PAM 2015 experiment
## Script created September 2018 by Keith Bouma-Gregson

## Response ratios calculated in PAM2015_ResponseRatios_Format.R
## TSV file exported from this script: PAM_2015_response_ratios_figures.tsv
## data in: /Users/kbg/Dropbox/PAM_Angelo/2015/PAM_data


#### Libraries #################################################################
library(tidyverse)
library(ggplot2)
library(grDevices)
################################################################################


#### FILE PATHS ################################################################
dir_input <- file.path("/Users","kbg","Dropbox","PAM_Angelo", "PAM_Angelo_Analyses", "2015", "PAM_data")
dir_out_fig <- file.path("/Users","kbg","Dropbox","PAM_Angelo", "PAM_Angelo_Analyses", "2015", "Figures")
################################################################################

#### READ IN DATA ##############################################################
rr.figs <- read_tsv(file.path(dir_input, "PAM2015_response_ratios_figures.tsv"))
################################################################################


##### PLOTTING PARAMETERS ######################################################
plot_points <- geom_point(aes(fill= Treatment, shape= Treatment),
                          position= position_dodge(width= 0.6),
                          color= "black",
                          size= 3)
plot_errorbars <- geom_errorbar(position= position_dodge(width= 0.6), width= 0.3)
yintercept <- geom_hline(yintercept = 0, size= 0.25)
x_axis_format <- scale_x_date(breaks= seq.Date(as.Date("2015-06-10"), as.Date("2015-06-15"), by= 1),
                              labels=c("Jun-10", "Jun-11", "", "", "", "Jun-15"))

treatment.order <- c("Thal", "Marg")
treatment.labels <- c("Thalweg", "Margin")
treatment.fill <- c("white", "black")
treatment.shapes <- c(21, 21)
algae.facet.labels <- as_labeller(c(`Clad` = expression(italic("Cladophora")), `Oed` = expression(italic("Oedogonium")), `Peri` = "Periphyton"))
algae.facet.labels <- as_labeller(c(`Clad` = "Cladophora", `Oed` = "Oedogonium", `Peri` = "Periphyton"))
facet.by.algae <- facet_grid(.~Algae, labeller= labeller(Algae= algae.facet.labels))


## ggplot theme for response ratio plots
# theme_freshSci
source(file.path("/Users", "kbg", "Dropbox", "PAM_Angelo","PAM_Angelo_Analyses", "ggplot_themes.R"))


theme_rr <- theme(panel.grid = element_blank(),
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
                   strip.background=element_rect(fill="transparent", color="transparent"),
                   axis.text.x = element_text(angle= 45, hjust= 1),
                   legend.position = "top")





#### MAKE PLOTS ################################################################
################################################################################


#### Fv/Fm #####################################################################

p.fv.fm <- ggplot(data= rr.figs, aes(x= Date,
                                        y= mean.rr.Fv.Fm,
                                        ymax= mean.rr.Fv.Fm + se.rr.Fv.Fm,
                                        ymin= mean.rr.Fv.Fm - se.rr.Fv.Fm,
                                        group= ID))

fvfm.rr <- p.fv.fm +
  yintercept +
  plot_errorbars +
  plot_points +
  #scale_y_continuous(limits= c(-0.4, 0.15), breaks= seq(-0.4, 0.1, by= 0.1)) +
  scale_y_continuous(limits= c(-0.4, 0.15), breaks= seq(-0.4, 0.1, by= 0.1), labels= c("-0.4", "", "-0.2", "", "0", "")) +
  labs(x= "", y= bquote(F[v]/F[m] ~ " response ratio (± se)")) +
  scale_fill_manual(values= treatment.fill, breaks= treatment.order, labels= treatment.labels) +
  scale_shape_manual(values= treatment.shapes, breaks= treatment.order, labels= treatment.labels) +
  x_axis_format +
  facet.by.algae +
 # theme_rr +
  theme_freshSci
fvfm.rr
ggsave(fvfm.rr, filename = file.path(dir_out_fig, "FvFm_rr.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)

#### ALPHA REG 2 ###############################################################

p.alpha.reg2 <- ggplot(data= rr.figs, aes(x= Date,
                                      y= mean.rr.alpha.REG2,
                                      ymax= mean.rr.alpha.REG2 + se.rr.alpha.REG2,
                                      ymin= mean.rr.alpha.REG2 - se.rr.alpha.REG2,
                                      group= ID))

alpha.rr <- p.alpha.reg2 +
  yintercept +
  plot_errorbars +
  plot_points +
  scale_y_continuous(limits= c(-0.62, 0.3), breaks= seq(-0.6, 0.3, by= 0.1), labels= c("-0.6", "", "-0.4", "", "-0.2", "", "0", "", "0.2", "")) +
  labs(x= "", y= bquote("Alpha response ratio (± se)")) +
  scale_fill_manual(values= treatment.fill, breaks= treatment.order, labels= treatment.labels) +
  scale_shape_manual(values= treatment.shapes, breaks= treatment.order, labels= treatment.labels) +
  x_axis_format +
  facet.by.algae +
  theme_rr
ggsave(alpha.rr, filename = file.path(dir_out_fig, "alphaREG2_rr.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)

#### ETR MAX REG 2 #############################################################

p.ETRmax.reg2 <- ggplot(data= rr.figs, aes(x= Date,
                                     y= mean.rr.ETRm.REG2,
                                     ymax= mean.rr.ETRm.REG2 + se.rr.ETRm.REG2,
                                     ymin= mean.rr.ETRm.REG2 - se.rr.ETRm.REG2,
                                     group= ID))

p.ETRmax.reg2 +
  yintercept +
  plot_errorbars +
  plot_points +
  scale_y_continuous(limits= c(-0.35, 0.4), breaks= seq(-0.3, 0.4, by= 0.1), labels= c("", "-0.2", "", "0", "",  "0.2", "", "0.4")) +
  labs(x= "", y= bquote(rETR[max] ~ " response ratio (± se)")) +
  scale_fill_manual(values= treatment.fill, breaks= treatment.order, labels= treatment.labels) +
  scale_shape_manual(values= treatment.shapes, breaks= treatment.order, labels= treatment.labels) +
  x_axis_format +
  facet.by.algae +
  theme_rr +
  theme(axis.text.x = element_text(angle= 45, hjust= 1),
        legend.position = "top")
ggsave(last_plot(), filename = file.path(dir_out_fig, "ETRmaxREG2_rr.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)

#### Ek    #####################################################################

p.Ek.reg2 <- ggplot(data= rr.figs, aes(x= Date,
                                           y= mean.rr.Ek.REG2,
                                           ymax= mean.rr.Ek.REG2 + se.rr.Ek.REG2,
                                           ymin= mean.rr.Ek.REG2 - se.rr.Ek.REG2,
                                           group= ID))

p.Ek.reg2 +
  yintercept +
  plot_errorbars +
  plot_points +
 scale_y_continuous(limits= c(-0.12, 0.6), breaks= seq(-0.1, 0.6, by= 0.1), labels= c("", "0", "", "0.2", "",  "0.4", "", "0.6")) +
  labs(x= "", y= bquote("Mean" ~ E[k] ~ "± se")) +
  scale_fill_manual(values= treatment.fill) +
  scale_shape_manual(values= treatment.shapes) +
  x_axis_format +
  facet.by.algae +
  theme_rr
ggsave(last_plot(), filename = file.path(dir_out_fig, "EkREG2_rr.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)







