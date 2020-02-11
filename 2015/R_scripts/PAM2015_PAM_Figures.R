## Plot light curves and regression paramters for PAM 2015 experiment
## Script created September 2018 by Keith Bouma-Gregson}Pl

## Data formating in  PAM2015_ResponseRatios_Format.R
## and exported as a TSV file: PAM2015_regression_parameters.tsv
## data in: /Users/kbg/Dropbox/PAM_Angelo/2015/PAM_data


#### Libraries #################################################################
library(tidyverse)
library(ggplot2)
library(lubridate)
library(lemon)
library(cowplot)
################################################################################


#### FILE PATHS ################################################################
dir_input <- file.path("/Users","kbg","Dropbox","PAM_Angelo", "PAM_Angelo_Analyses", "2015", "PAM_data")
dir_out_fig <- file.path("/Users","kbg","Dropbox","PAM_Angelo", "PAM_Angelo_Analyses", "2015", "Figures")
################################################################################

#### READ IN AND FORMAT DATA ##############################################################
pam.figs <- read_tsv(file.path(dir_input, "PAM2015_regression_parameters.tsv")) %>%
              select(Date, Day, ArrayID, CapID, Site, Loc, Algae, Treatment, Mem, PAR, ETR, contains("REG2"), Fv.Fm, Fo, NPQ)

#### SUMMARIZE LIGHT CURVES
# Summarize over pseudoreplicates

lc.site.figs <- pam.figs %>%
                  filter(Algae != "Blank") %>%
                  group_by(Date, Site, Algae, Treatment, PAR) %>%
                  summarize(
                    pseu.n= length(ETR),
                    mean.site.ETR= mean(ETR),
                    sd.site.ETR= sd(ETR),
                    se.site.ETR= sd.site.ETR/sqrt(pseu.n)
                  ) %>%
                  mutate(
                    uniqueID= paste(Site, Algae, Treatment, sep= ".")
                  )


# Summarize over replicates
lc.trt.figs <- lc.site.figs %>%
                  group_by(Date, Algae, Treatment, PAR) %>%
                  summarize(
                    n= length(mean.site.ETR),
                    mean.trt.ETR= mean(mean.site.ETR, na.rm = TRUE),
                    sd.trt.ETR= sd(mean.site.ETR),
                    se.trt.ETR= sd.trt.ETR/sqrt(n)
                  ) %>%
                  mutate(
                    uniqueID= paste(Algae, Treatment, sep= ".")
                  )





#### SUMMARIZE NPQ CURVES
# Summarize over pseudoreplicates

npq.site.figs <- pam.figs %>%
  filter(Algae != "Blank" & PAR > 1) %>%
  group_by(Date, Site, Algae, Treatment, PAR) %>%
  summarize(
    pseu.n= length(NPQ),
    mean.NPQ.site= mean(NPQ),
    sd.NPQ.site= sd(NPQ)
  ) %>%
  mutate(
    uniqueID= paste(Site, Algae, Treatment, sep= ".")
  )


# Summarize over replicates
npq.trt.figs <- npq.site.figs %>%
  group_by(Date, Algae, Treatment, PAR) %>%
  summarize(
    n= length(mean.NPQ.site),
    mean.NPQ.trt= mean(mean.NPQ.site),
    sd.NPQ.trt= sd(mean.NPQ.site),
    se.NPQ.trt= sd.NPQ.trt/sqrt(n)
  ) %>%
  mutate(
    uniqueID= paste(Algae, Treatment, sep= ".")
  )


#### SUMMARIZE REGRESSION PARAMETERS

## Average over psuedoreplicates
reg.figs.site <- pam.figs %>%
  filter(PAR == 1 & Algae != "Blank") %>%
  group_by(Date, Site, Algae, Treatment) %>%
  summarize(n= length(REG2.alpha),
            site.alpha.REG2 = mean(REG2.alpha, na.rm= TRUE),
            sd.alpha.REG2 = sd(REG2.alpha, na.rm= TRUE),
            site.ETRm.REG2= mean(REG2.ETRm, na.rm= TRUE),
            sd.ETRm.REG2= sd(REG2.ETRm, na.rm= TRUE),
            site.Ek.REG2 = mean(REG2.Ek, na.rm= TRUE),
            sd.Ek.REG2 = sd(REG2.Ek, na.rm= TRUE),
            site.Fv.Fm = mean(Fv.Fm, na.rm= TRUE),
            sd.Fv.Fm = sd(Fv.Fm, na.rm= TRUE),
            site.Fo = mean(Fo, na.rm= TRUE),
            sd.Fo = sd(Fo, na.rm= TRUE)) %>%
  ungroup() %>%
  mutate(Day= yday(Date) - yday(unique(Date)[1]),
         ID= str_c(Site, Algae, Treatment, sep= "."))

# Average over replicates
reg.figs.trt <- reg.figs.site %>%
  group_by(Date, Algae, Treatment) %>%
  summarize(n= length(site.alpha.REG2),
            mean.alpha.REG2.trt = mean(site.alpha.REG2, na.rm= TRUE),
            sd.alpha.REG2.trt = sd(site.alpha.REG2, na.rm= TRUE),
            se.alpha.REG2.trt = sd.alpha.REG2.trt / sqrt(n),
            mean.ETRm.REG2.trt= mean(site.ETRm.REG2, na.rm= TRUE),
            sd.ETRm.REG2.trt= sd(site.ETRm.REG2, na.rm= TRUE),
            se.ETRm.REG2.trt = sd.ETRm.REG2.trt / sqrt(n),
            mean.Ek.REG2.trt = mean(site.Ek.REG2, na.rm= TRUE),
            sd.Ek.REG2.trt = sd(site.Ek.REG2, na.rm= TRUE),
            se.Ek.REG2.trt = sd.Ek.REG2.trt / sqrt(n),
            mean.Fv.Fm.trt = mean(site.Fv.Fm, na.rm= TRUE),
            sd.Fv.Fm.trt = sd(site.Fv.Fm, na.rm= TRUE),
            se.Fv.Fm.trt = sd.Fv.Fm.trt / sqrt(n),
            mean.Fo.trt = mean(site.Fo, na.rm= TRUE),
            sd.Fo.trt = sd(site.Fo, na.rm= TRUE),
            se.Fo.trt = sd.Fo.trt / sqrt(n)) %>%
  ungroup() %>%
  mutate(Day= yday(Date) - yday(unique(Date)[1]),
         ID= str_c(Algae, Treatment, sep= "."))
         #mean.ETRm.adjusted= ifelse(Treatment == "Thal", mean.ETRm.REG2.trt + diff(reg.figs.site.blanks$mean.ETRm.REG2), mean.ETRm.REG2.trt))



#### SUMMARIZE DATA FROM BLANKS

lc.trt.figs.blanks <- pam.figs %>%
  filter(Algae == "Blank", Day == "6", Site != "2") %>%
  group_by(Algae, Treatment, PAR) %>%
  summarize(
    n= length(ETR),
    mean.trt.ETR= mean(ETR, na.rm = TRUE),
    sd.trt.ETR= sd(ETR, na.rm= TRUE),
    se.trt.ETR= sd.trt.ETR/sqrt(n)
  ) %>%
  ungroup()


reg.figs.site.blanks <- pam.figs %>%
  filter(Algae == "Blank", Day == "6", Site != "2") %>%
  group_by(Date, Algae, Treatment) %>%
  summarize(n= length(REG2.alpha),
            mean.alpha.REG2 = mean(REG2.alpha, na.rm= TRUE),
            sd.alpha.REG2 = sd(REG2.alpha, na.rm= TRUE),
            se.alpha.REG2 = sd.alpha.REG2 / sqrt(n),
            mean.ETRm.REG2= mean(REG2.ETRm, na.rm= TRUE),
            sd.ETRm.REG2= sd(REG2.ETRm, na.rm= TRUE),
            se.ETRm.REG2 = sd.ETRm.REG2 / sqrt(n),
            mean.Fv.Fm = mean(Fv.Fm, na.rm= TRUE),
            sd.Fv.Fm = sd(Fv.Fm, na.rm= TRUE),
            se.Fv.Fm = sd.Fv.Fm / sqrt(n),
            mean.Fo = mean(Fo, na.rm= TRUE),
            sd.Fo = sd(Fo, na.rm= TRUE)) %>%
  ungroup()




##### PLOTTING PARAMETERS ######################################################
plot_points <- geom_point(aes(fill= Treatment, shape= Treatment),
                          position= position_dodge(width= 0.4),
                          color= "black",
                          size= 2)
plot_errorbars <- geom_errorbar(position= position_dodge(width= 0.4), width= 0.4)
plot_lines <- geom_line(aes(group= ID, linetype= Treatment), position= position_dodge(width= 0.4), size= 0.5)
yintercept <- geom_hline(yintercept = 0, size= 0.25)
x_axis_format <- scale_x_date(breaks= seq.Date(as.Date("2015-06-09"), as.Date("2015-06-15"), by= 1),
                              labels=c("Jun-09", "Jun-10", "Jun-11", "", "", "", "Jun-15"))
treatment.order <- c("Thal", "Marg")
treatment.labels <- c("Thalweg", "Margin")
treatment.fill <- c("white", "black")
treatment.linetype <- c("dashed", "solid")
treatment.shapes <- c(21, 21)
date.facet.labels <- as_labeller(c(`2015-06-09` = "09-Jun", `2015-06-10` = "10-Jun", `2015-06-11` = "11-Jun", `2015-06-15` = "15-Jun"))
algae.facet.labels <- as_labeller(c(`Clad` = "Cladophora", `Oed` = "Oedogonium", `Peri` = "Periphyton"))


## ggplot themes
# theme_freshSci
source(file.path("/Users", "kbg", "Dropbox", "PAM_Angelo","PAM_Angelo_Analyses", "ggplot_themes.R"))

# theme_pam <- theme(panel.grid = element_blank(),
#                    plot.margin = unit(c(1, 1, 1, 1), "cm"),
#                    text = element_text(size= 14),
#                    plot.background = element_rect(fill = "transparent", color= "transparent"), # bg of the plot
#                    panel.background = element_rect(fill= "transparent", color= "transparent"),
#                    panel.border= element_rect(fill= NA, color= "black", linetype= "solid", size= 1),
#                    panel.ontop = TRUE,
#                    axis.text = element_text(colour="black"),
#                    axis.title.x = element_text(vjust = -0.75),
#                    axis.title.y = element_text(vjust = 1.5),
#                    legend.background = element_rect(size=0.25, color="black", fill= "transparent"),
#                    legend.key = element_blank(),
#                    strip.background = element_rect(fill="transparent", color= "transparent"),
#                    axis.text.x = element_text(angle= 45, hjust= 1),
#                    legend.position = "top")


#### MAKE PLOTS ################################################################


#### LIGHT CURVES

lc.trt.plot1 <- ggplot(data= lc.trt.figs, aes(x= PAR,
                                     y= mean.trt.ETR,
                                     ymin= mean.trt.ETR - se.trt.ETR,
                                     ymax= mean.trt.ETR + se.trt.ETR,
                                     group= uniqueID))

lc.fig.2015 <- lc.trt.plot1 +
  #yintercept +
  geom_line(aes(linetype= Treatment), size= 0.25) +
  plot_errorbars +
  geom_point(aes(fill=Treatment, shape= Treatment), size = 1.5, color= "black") +
  labs(x=expression(paste("PAR (",mu,"Mols ",m^{-2}," ", s^{-1}, ")")), y=" rETR (± SE)") +
  scale_x_continuous(limits= c(0, 4000)) +
  scale_y_continuous(expand= c(0.02, 0)) +
  scale_linetype_manual(values= treatment.linetype, breaks= treatment.order, labels= treatment.labels) +
  scale_fill_manual(values= treatment.fill, breaks= treatment.order, labels= treatment.labels) +
  scale_shape_manual(values= treatment.shapes, breaks= treatment.order, labels= treatment.labels) +
  #facet_grid(Algae~Date, labeller= labeller(Date= date.facet.labels, Algae= algae.facet.labels)) +
  facet_rep_grid(Algae~Date, labeller= labeller(Date= date.facet.labels, Algae= algae.facet.labels)) +
  theme_freshSci +
  theme(strip.text.y = element_blank(),
        strip.text.x = element_text(face= "bold", size= 10),
        legend.position = "top",
        legend.margin = margin(0, 0, 0, 0, unit= "cm"),
        legend.key.width = unit(1, "cm"),
        legend.box.spacing = unit(0, "cm"),
        axis.line = element_line(size= 0.25),
        #axis.text.x = element_text(angle= 45, vjust= 1, hjust= 1))
  )
lc.fig.2015

lc.fig.2015.anno <- plot_grid(lc.fig.2015, ncol= 1) +
  draw_label(label= expression(italic("Cladophora")), 
             x= 0.095, y= 0.83, size= 8, hjust= 0) +
  draw_label(label= expression(italic("Oedogonium")), 
             x= 0.095, y= 0.575, size= 8, hjust= 0) +
  draw_label(label= expression("Periphyton"), 
             x= 0.095, y= 0.32, size= 8, hjust= 0)
  
#lc.fig.2015.anno

ggsave(lc.fig.2015.anno, filename = file.path(dir_out_fig, "PAM2015_LightCurves.eps"), height= 12.7, width= 17.8, units= "cm")




#ggsave(last_plot(), filename = file.path(dir_out_fig, "light_curves_treatment_means.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)


lc.site.plot1 <- ggplot(data= lc.site.figs, aes(x= PAR,
                                              y= mean.site.ETR,
                                              ymin= mean.site.ETR - se.site.ETR,
                                              ymax= mean.site.ETR + se.site.ETR,
                                              group= uniqueID))

lc.site.plot1 +
  yintercept +
  geom_line(aes(linetype= Treatment), size= 0.25) +
  geom_point(aes(fill=Treatment, shape= Treatment), size = 1.5, color= "black") +
  labs(x=expression(paste("PAR ",mu,"Mols ",m^{-2}," ", s^{-1} )), y="rETR (± se)") +
  scale_x_continuous(limits= c(0, 4000)) +
  scale_linetype_manual(values= treatment.linetype, breaks= treatment.order, labels= treatment.labels) +
  scale_fill_manual(values= treatment.fill, breaks= treatment.order, labels= treatment.labels) +
  scale_shape_manual(values= treatment.shapes, breaks= treatment.order, labels= treatment.labels) +
  facet_grid(Algae~Date, labeller= labeller(Date= date.facet.labels, Algae= algae.facet.labels)) +
  theme_pam
ggsave(last_plot(), filename = file.path(dir_out_fig, "light_curves_site_means.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)


#### LIGHT CURVE FOR THE BLANKS
lc.blanks.plot1 <- ggplot(data= lc.trt.figs.blanks, aes(x= PAR,
                                              y= mean.trt.ETR,
                                              ymin= mean.trt.ETR - se.trt.ETR,
                                              ymax= mean.trt.ETR + se.trt.ETR))

lc.blanks.plot1 +
  yintercept +
  geom_line(aes(linetype= Treatment), size= 0.25) +
  plot_errorbars +
  geom_point(aes(fill=Treatment, shape= Treatment), size = 1.5, color= "black") +
  labs(x=expression(paste("PAR ",mu,"Mols ",m^{-2}," ", s^{-1} )), y="Mean relative electron transport rate ± se") +
  scale_x_continuous(limits= c(0, 3000)) +
  scale_linetype_manual(values= treatment.linetype) +
  scale_fill_manual(values= treatment.fill) +
  scale_shape_manual(values= treatment.shapes) +
 # facet_grid(Algae~Date, labeller= labeller(Date= date.facet.labels, Algae= algae.facet.labels)) +
  theme_pam
ggsave(last_plot(), filename = file.path(dir_out_fig, "light_curves_blanks.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)


#### NPQ CURVES
npq.trt.plot1 <- ggplot(data= npq.trt.figs, aes(x= PAR,
                                              y= mean.NPQ.trt,
                                              ymin= mean.NPQ.trt - se.NPQ.trt,
                                              ymax= mean.NPQ.trt + se.NPQ.trt,
                                              group= uniqueID))

npq.trt.plot1 +
  yintercept +
  geom_line(aes(linetype= Treatment), size= 0.25) +
  plot_errorbars +
  geom_point(aes(fill=Treatment, shape= Treatment), size = 1.5, color= "black") +
  labs(x=expression(paste("PAR ",mu,"Mols ",m^{-2}," ", s^{-1} )), y="Mean NPQ ± se") +
  scale_x_continuous(limits= c(0, 4000)) +
  scale_linetype_manual(values= treatment.linetype) +
  scale_fill_manual(values= treatment.fill) +
  scale_shape_manual(values= treatment.shapes) +
  facet_grid(Algae~Date, labeller= labeller(Date= date.facet.labels, Algae= algae.facet.labels)) +
  theme_pam
ggsave(last_plot(), filename = file.path(dir_out_fig, "npq_curves_treatment_means.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)







#### ALPHA

p.alpha.trt <- ggplot(data= reg.figs.trt, aes(x= Date,
                                      y= mean.alpha.REG2.trt,
                                      ymax= mean.alpha.REG2.trt + se.alpha.REG2.trt,
                                      ymin= mean.alpha.REG2.trt - se.alpha.REG2.trt,
                                      group= ID))

p.alpha.trt +
  yintercept +
#  plot_lines +
  plot_errorbars +
  plot_points +
  scale_y_continuous(limits= c(0, 0.23), breaks= seq(0, 0.2, by= 0.05), labels= c("0", "", "0.1", "", "0.2")) +
  labs(x= "", y= bquote("Alpha (± se)")) +
  scale_fill_manual(values= treatment.fill, breaks= treatment.order, labels= treatment.labels) +
  scale_shape_manual(values= treatment.shapes, breaks= treatment.order, labels= treatment.labels) +
  scale_linetype_manual(values= treatment.linetype) +
  x_axis_format +
  facet_grid(.~Algae, labeller= labeller(Algae= algae.facet.labels)) +
  theme_pam
ggsave(last_plot(), filename = file.path(dir_out_fig, "alphaREG2.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)


### ETR max

p.ETRm.trt <- ggplot(data= reg.figs.trt, aes(x= Date,
                                                      y= mean.ETRm.REG2.trt,
                                                      ymax= mean.ETRm.REG2.trt + se.ETRm.REG2.trt,
                                                      ymin= mean.ETRm.REG2.trt - se.ETRm.REG2.trt,
                                                      group= ID))

## With blank values subtracted off Thalweg
p.ETRm.trt.adj <- ggplot(data= reg.figs.trt, aes(x= Date,
                                             y= mean.ETRm.adjusted,
                                             ymax= mean.ETRm.adjusted + se.ETRm.REG2.trt,
                                             ymin= mean.ETRm.adjusted - se.ETRm.REG2.trt,
                                             group= ID))


#p.ETRm.trt.adj +
  p.ETRm.trt +
  yintercept +
  #plot_lines +
  plot_errorbars +
  plot_points +
  scale_y_continuous(limits= c(0, 175), breaks= seq(0, 175, by= 25), labels= c("0", "", "50", "", "100", "", "150", "")) +
  labs(x= "", y= bquote(rETR[max] ~ " (± se)")) +
  scale_fill_manual(values= treatment.fill, breaks= treatment.order, labels= treatment.labels) +
  scale_shape_manual(values= treatment.shapes, breaks= treatment.order, labels= treatment.labels) +
  scale_linetype_manual(values= treatment.linetype) +
  x_axis_format +
  facet_grid(.~Algae, labeller= labeller(Algae= algae.facet.labels)) +
  theme_pam
ggsave(last_plot(), filename = file.path(dir_out_fig, "ETRmREG2.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)


### Fv/Fm

p.FvFm.trt <- ggplot(data= reg.figs.trt, aes(x= Date,
                                               y= mean.Fv.Fm.trt,
                                               ymax= mean.Fv.Fm.trt +  se.Fv.Fm.trt,
                                               ymin= mean.Fv.Fm.trt -  se.Fv.Fm.trt,
                                               group= ID))

p.FvFm.trt +
  #plot_lines +
  plot_errorbars +
  plot_points +
  scale_y_continuous(limits= c(0.2, 0.62), breaks= seq(0, 0.6, by= 0.1), labels= c("0", "", "0.2", "", "0.4", "", "0.6")) +
  labs(x= "", y= bquote(F[v]/F[m] ~ " (± se)")) +
  scale_fill_manual(values= treatment.fill, breaks= treatment.order, labels= treatment.labels) +
  scale_shape_manual(values= treatment.shapes, breaks= treatment.order, labels= treatment.labels) +
  scale_linetype_manual(values= treatment.linetype) +
  x_axis_format +
  facet_grid(.~Algae, labeller= labeller(Algae= algae.facet.labels)) +
  theme_pam
ggsave(last_plot(), filename = file.path(dir_out_fig, "FvFm.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)

### Fo

p.Fo.trt <- ggplot(data= reg.figs.trt, aes(x= Date,
                                             y= mean.Fo.trt,
                                             ymax= mean.Fo.trt +  se.Fo.trt,
                                             ymin= mean.Fo.trt -  se.Fo.trt,
                                             group= ID))

p.Fo.trt +
  yintercept +
  plot_lines +
  plot_errorbars +
  plot_points +
#  scale_y_continuous(limits= c(0, 0.62), breaks= seq(0, 0.6, by= 0.1), labels= c("0", "", "0.2", "", "0.4", "", "0.6")) +
  labs(x= "", y= bquote("Mean Fo ± se")) +
  scale_fill_manual(values= treatment.fill) +
  scale_shape_manual(values= treatment.shapes) +
  scale_linetype_manual(values= treatment.linetype) +
  x_axis_format +
  facet_grid(.~Algae, labeller= labeller(Algae= algae.facet.labels)) +
  theme_pam
ggsave(last_plot(), filename = file.path(dir_out_fig, "Fo.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)



#### BLANKS

diff(reg.figs.site.blanks$mean.alpha.REG2)

p.alpha.trt.blank <- ggplot(data= reg.figs.site.blanks, aes(x= Treatment,
                                              y= mean.alpha.REG2,
                                              ymax= mean.alpha.REG2 + se.alpha.REG2,
                                              ymin= mean.alpha.REG2 - se.alpha.REG2))

p.alpha.trt.blank +
  yintercept +
  geom_errorbar(width= 0.2) +
  plot_points +
#  scale_y_continuous(limits= c(0, 0.23), breaks= seq(0, 0.2, by= 0.05), labels= c("0", "", "0.1", "", "0.2")) +
  labs(x= "", y= bquote("Mean alpha ± se")) +
  scale_fill_manual(values= treatment.fill) +
  scale_shape_manual(values= treatment.shapes) +
  scale_linetype_manual(values= treatment.linetype) +
  theme_pam
ggsave(last_plot(), filename = file.path(dir_out_fig, "alphaREG2_blank.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)


## ETRm
diff(reg.figs.site.blanks$mean.ETRm.REG2)

p.ETRm.trt.blank <- ggplot(data= reg.figs.site.blanks, aes(x= Treatment,
                                                            y= mean.ETRm.REG2,
                                                            ymax= mean.ETRm.REG2 + se.ETRm.REG2,
                                                            ymin= mean.ETRm.REG2 - se.ETRm.REG2))

p.ETRm.trt.blank +
  yintercept +
  geom_errorbar(width= 0.2) +
  plot_points +
  labs(x= "", y= bquote("Mean ETRm ± se")) +
  scale_fill_manual(values= treatment.fill) +
  scale_shape_manual(values= treatment.shapes) +
  scale_linetype_manual(values= treatment.linetype) +
  theme_pam
ggsave(last_plot(), filename = file.path(dir_out_fig, "ETRmREG2_blank.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)


## Fv/Fm
diff(reg.figs.site.blanks$mean.Fv.Fm)

p.FvFm.trt.blank <- ggplot(data= reg.figs.site.blanks, aes(x= Treatment,
                                                           y= mean.Fv.Fm,
                                                           ymax= mean.Fv.Fm + se.Fv.Fm,
                                                           ymin= mean.Fv.Fm - se.Fv.Fm))

p.FvFm.trt.blank +
  yintercept +
  geom_errorbar(width= 0.2) +
  plot_points +
  labs(x= "", y= bquote("Mean Fv/Fm ± se")) +
  scale_fill_manual(values= treatment.fill) +
  scale_shape_manual(values= treatment.shapes) +
  scale_linetype_manual(values= treatment.linetype) +
  theme_pam
ggsave(last_plot(), filename = file.path(dir_out_fig, "FvFm_blank.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)


