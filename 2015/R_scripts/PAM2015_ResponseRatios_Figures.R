## Plot Response ratios for the PAM 2015 experiment
## Script created September 2018 by Keith Bouma-Gregson

## Response ratios calculated in PAM2015_ResponseRatios_Format.R
## TSV file exported from this script: PAM_2015_response_ratios_figures.tsv
## data in: /Users/kbg/Dropbox/PAM_Angelo/2015/PAM_data


#### Libraries #################################################################
library(tidyverse)
library(ggplot2)
library(grDevices)
library(lemon)
library(cowplot)
library(ggpubr)
library(extrafont) #https://github.com/wch/extrafont
loadfonts(device= "postscript")
################################################################################


#### FILE PATHS ################################################################
dir_input <- file.path("2015", "PAM_data")
dir_out_fig <- file.path("2015", "Figures")
dir_out_fig_manuscript <- file.path("..", "Manuscript_Drafts", "Manuscript_Figures")

################################################################################

#### READ IN DATA ##############################################################
rr.figs <- read_tsv(file.path(dir_input, "PAM2015_response_ratios_figures.tsv")) %>% 
  mutate(Algae= factor(.$Algae, labels= c(expression(bolditalic("Cladophora")), 
                                          expression(bolditalic("Oedogonium")), 
                                          expression(bold("Epilithic diatoms")))))
################################################################################


##### PLOTTING PARAMETERS ######################################################
## ggplot theme for response ratio plots
source("ggplot_themes.R") # theme_freshSci


plot_points <- geom_point(aes(fill= Treatment, shape= Treatment),
                          position= position_dodge(width= 0.6),
                          color= "black",
                          size= 2)
plot_errorbars <- geom_errorbar(position= position_dodge(width= 0.6), width= 0.3)
yintercept <- geom_hline(yintercept = 0, size= 0.25)
x_axis_format <- scale_x_date(breaks= seq.Date(as.Date("2015-06-10"), as.Date("2015-06-15"), by= 1),
                              labels=c("10-Jun", "11-Jun", "", "", "", "15-Jun"))

treatment.order <- c("Thal", "Marg")
treatment.labels <- c("Thalweg", "Margin")
treatment.fill <- c("white", "black")
treatment.shapes <- c(21, 21)
facet.by.algae <- facet_rep_grid(.~Algae, labeller= label_parsed)




#### MAKE PLOTS ################################################################


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
  theme_freshSci +
  theme(
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "top",
        legend.margin = margin(0, 0, 0, 0, unit= "cm"),
        legend.box.spacing = unit(0, "cm"),
        axis.line = element_line(size= 0.25))
#fvfm.rr
#ggsave(fvfm.rr, filename = file.path(dir_out_fig, "FvFm_rr.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)

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
  theme_freshSci +
  theme(strip.text = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

ggsave(alpha.rr, filename = file.path(dir_out_fig, "alphaREG2_rr.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)

#### ETR MAX REG 2 #############################################################

p.ETRmax.reg2 <- ggplot(data= rr.figs, aes(x= Date,
                                     y= mean.rr.ETRm.REG2,
                                     ymax= mean.rr.ETRm.REG2 + se.rr.ETRm.REG2,
                                     ymin= mean.rr.ETRm.REG2 - se.rr.ETRm.REG2,
                                     group= ID))

etrm.rr <- p.ETRmax.reg2 +
  yintercept +
  plot_errorbars +
  plot_points +
  scale_y_continuous(limits= c(-0.35, 0.4), breaks= seq(-0.3, 0.4, by= 0.1), labels= c("", "-0.2", "", "0", "",  "0.2", "", "0.4")) +
  labs(x= "", y= bquote(rETR[max] ~ " response ratio (± se)")) +
  scale_fill_manual(values= treatment.fill, breaks= treatment.order, labels= treatment.labels) +
  scale_shape_manual(values= treatment.shapes, breaks= treatment.order, labels= treatment.labels) +
  x_axis_format +
  facet.by.algae +
  theme_freshSci +
  theme(strip.text = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle= 45, hjust= 1, vjust= 1)
  )

ggsave(last_plot(), filename = file.path(dir_out_fig, "ETRmaxREG2_rr.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)



#### COMBINED PLOT FOR MANUSCRIPT #####


rr.fig.2015 <- plot_grid(fvfm.rr, 
                    alpha.rr + theme(legend.position="none"), 
                    etrm.rr + theme(legend.position= "none"), 
                    rel_heights = c(1.3, 1, 1.2),
                    ncol= 1, nrow= 3) +
  draw_label(label= expression(bold(paste("F"[v]~"/"~"F"[m]))), x= 0.01, y= 0.89, size= 10, hjust= 0) +
  draw_label(label= expression(bold("Alpha (\U03B1)")), x= 0.01, y= 0.61, size= 10, hjust= 0) +
  draw_label(label= expression(bold(paste("rETR"[max]))), x= 0.01, y= 0.33, size= 10, hjust= 0) +
  # Significant treatment labels
  draw_label(label= "Treatment: p < 0.05", x= 0.23, y= 0.85, size= 6, hjust= 0) +
  draw_label(label= "Treatment: p < 0.05", x= 0.53, y= 0.85, size= 6, hjust= 0) +
  draw_label(label= "Treatment: p < 0.05", x= 0.83, y= 0.85, size= 6, hjust= 0) +
  draw_label(label= "Treatment: p < 0.05", x= 0.23, y= 0.58, size= 6, hjust= 0) + 
  draw_label(label= "Treatment: p < 0.05", x= 0.53, y= 0.58, size= 6, hjust= 0) +
  draw_label(label= "Treatment: p < 0.05", x= 0.83, y= 0.58, size= 6, hjust= 0) +
  draw_label(label= "Treatment: p < 0.05", x= 0.53, y= 0.30, size= 6, hjust= 0) 
#rr.fig.2015

rr.fig.2015.anno <-  annotate_figure(rr.fig.2015, 
                                     left = text_grob(label= "Response ratio (± SE)", 
                                                      rot = 90, size= 10))

ggsave(rr.fig.2015.anno, filename = file.path(dir_out_fig_manuscript, "Fig_8.eps"), height= 12.7, width= 17.8, units= "cm", device= cairo_ps)
ggsave(rr.fig.2015.anno, filename = file.path(dir_out_fig, "PAM2015_ResponseRatios_combined.eps"), height= 12.7, width= 17.8, units= "cm", device= cairo_ps)


##### SUPPLEMENTARY FIGURE OF PARAMETER VALUES #####
lc.reg.reps.param <- read_tsv(file.path(dir_input, "PAM2015_parameters_figures.tsv")) %>% 
  mutate(Algae= factor(.$Algae, labels= c(expression(bolditalic("Cladophora")), 
                                          expression(bolditalic("Oedogonium")), 
                                          expression(bold("Epilithic diatoms")))))
str(lc.reg.reps.param)

levels(factor(lc.reg.reps.param$Algae))

## Plotting variables
plot_points_params <- geom_point(aes(fill= Treatment, shape= Treatment),
                                position= position_dodge(width= 0.6),
                                color= "black",
                                size= 1.5)
x_axis_format_params <- scale_x_date(breaks= seq.Date(as.Date("2015-06-09"), as.Date("2015-06-15"), by= 1),
                                     labels=c("09-Jun", "10-Jun", "11-Jun", "", "", "", "15-Jun"))



## Make plots
fv.fm <- ggplot(data= lc.reg.reps.param, aes(x= Date,
                                     y= mean.Fv.Fm,
                                     ymax= mean.Fv.Fm + se.Fv.Fm,
                                     ymin= mean.Fv.Fm - se.Fv.Fm,
                                     group= ID))

fvfm <- fv.fm +
  yintercept +
  plot_errorbars +
  plot_points_params +
  scale_y_continuous(limits= c(0.2, 0.65), expand= c(0, 0)) +
  labs(x= "", y= bquote(F[v]/F[m] ~ " (± SE)")) +
  scale_fill_manual(values= treatment.fill, breaks= treatment.order, labels= treatment.labels) +
  scale_shape_manual(values= treatment.shapes, breaks= treatment.order, labels= treatment.labels) +
  x_axis_format_params +
  facet.by.algae +
  # theme_rr +
  theme_freshSci +
  theme(
    #axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "top",
    legend.margin = margin(0, 0, 0, 0, unit= "cm"),
    legend.box.spacing = unit(0, "cm"),
    axis.line = element_line(size= 0.25))
#fvfm


### Alpha REG2
p.alpha.reg2.param <- ggplot(data= lc.reg.reps.param, aes(x= Date,
                                          y= mean.alpha.REG2,
                                          ymax= mean.alpha.REG2 + se.alpha.REG2,
                                          ymin= mean.alpha.REG2 - se.alpha.REG2,
                                          group= ID))

alpha <- p.alpha.reg2.param +
  yintercept +
  plot_errorbars +
  plot_points_params +
  scale_y_continuous(limits= c(0, 0.25), expand= c(0, 0)) +
  labs(x= "", y= bquote("\U03B1 (± SE)")) +
  scale_fill_manual(values= treatment.fill, breaks= treatment.order, labels= treatment.labels) +
  scale_shape_manual(values= treatment.shapes, breaks= treatment.order, labels= treatment.labels) +
  x_axis_format_params +
  facet.by.algae +
  theme_freshSci +
  theme(strip.text = element_blank(),
        #axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")
#alpha


#### ETR MAX REG 2 #############################################################

p.ETRmax.reg2.param<- ggplot(data= lc.reg.reps.param, aes(x= Date,
                                           y= mean.ETRm.REG2,
                                           ymax= mean.ETRm.REG2 + se.ETRm.REG2,
                                           ymin= mean.ETRm.REG2 - se.ETRm.REG2,
                                           group= ID))

etrm <- p.ETRmax.reg2.param +
  yintercept +
  plot_errorbars +
  plot_points_params +
  scale_y_continuous(limits= c(0, 175), expand= c(0, 0)) +
  labs(x= "", y= bquote(rETR[max] ~ " (± SE)")) +
  scale_fill_manual(values= treatment.fill, breaks= treatment.order, labels= treatment.labels) +
  scale_shape_manual(values= treatment.shapes, breaks= treatment.order, labels= treatment.labels) +
  x_axis_format_params +
  facet.by.algae +
  theme_freshSci +
  theme(strip.text = element_blank(),
        #axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle= 45, hjust= 1, vjust= 1))
#etrm


#### COMBINED PLOT FOR MANUSCRIPT #####
param.fig.2015 <- plot_grid(fvfm, 
                         alpha + theme(legend.position="none"), 
                         etrm + theme(legend.position= "none"), 
                         rel_heights = c(1.3, 1, 1.2),
                         ncol= 1, nrow= 3) +
  draw_label(label= expression(bold(paste("F"[v]~"/"~"F"[m]))), x= 0.01, y= 0.89, size= 10, hjust= 0) +
  draw_label(label= expression(bold("Alpha (\U03B1)")), x= 0.01, y= 0.63, size= 10, hjust= 0) +
  draw_label(label= expression(bold(paste("rETR"[max]))), x= 0.01, y= 0.34, size= 10, hjust= 0)
#param.fig.2015


ggsave(param.fig.2015, filename = file.path(dir_out_fig_manuscript, "Fig_S5.eps"), height= 12.7, width= 17.8, units= "cm", device= cairo_ps)

