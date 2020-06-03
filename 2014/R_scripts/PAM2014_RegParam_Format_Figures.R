## 2014 PAM Angelo Experiment

## Read in and format and plot PE curve parameters 
## Alpha and rETRmax calculated from PAM data in script PAM2014_PEcurve_Format.R
## Fv/Fm from raw PAM data

## Created by KBG January-2016


#### LIBRARIES #################################################################
library(tidyverse)
library(ggplot2)
library(lemon) #facet_rep_wrap()
library(ggpubr) #ggarrange()
library(cowplot)
library(extrafont) #https://github.com/wch/extrafont
#loadfonts(device= "postscript")
################################################################################

#### FILE PATHS ################################################################
dir_input <- file.path("2014", "PAM_data")
dir_out_fig <- file.path("2014", "Figures")
dir_out_fig_manuscript <- file.path("..", "Manuscript_Drafts", "Manuscript_Figures")
dir_out_table <- file.path("2014", "PAM_data")
################################################################################



#### REGRESSION PARAMETERS CALCULATED BY KBG ###################################
#### in script PAM2014_PEcurve_Format.R
reg.data <- read_tsv(file.path(dir_input, "PAM2014_clean_REG_params.tsv")) %>%
             select(-contains("REG1"), -REG2.RSS) %>%
             rename(Alpha= REG2.alpha, ETRm= REG2.ETRm, Ek= REG2.Ek) %>% 
             mutate(Location= as.factor(ifelse(Location == "Benthic", "Submerged", "Floating")))
reg.data$Location <- factor(reg.data$Location, levels= levels(reg.data$Location)[c(2, 1)])

## Summarize parameters for the three replicates
reg.s <- reg.data %>%
           filter(Algae != "Blank") %>%
           group_by(Day, Location, Algae, TreatID) %>%
           summarize(
               N = length(Alpha),
               mean_Alpha = mean(Alpha),
               sd_Alpha = sd(Alpha),
               se_Alpha = sd_Alpha / sqrt(N),
               mean_ETRm = mean(ETRm),
               sd_ETRm = sd(ETRm),
               se_ETRm = sd_ETRm / sqrt(N),
               mean_FvFm = mean(FvFm),
               sd_FvFm = sd(FvFm),
               se_FvFm = sd_FvFm / sqrt(N)
           ) %>%
           ungroup() %>%
  mutate(facet_order= ifelse(Algae == "Clad_R", "2", 
                             ifelse(Algae == "Clad_Y", "1", 
                                    ifelse(Algae == "Cyano_Spires", "6", 
                                           ifelse(Algae == "Phorm", "5", 
                                                  ifelse(Algae == "Nostoc", "4",
                                                         ifelse(Algae == "Riv", "3", "7")))))))

reg.s$facet_order <- factor(reg.s$facet_order, 
                            labels= c(expression(italic("Cladophora")~"Yellow"),
                                      expression(italic("Cladophora")~"Red"),
                                      expression(italic("Rivularia")),
                                      expression(italic("Nostoc")),
                                      expression(italic("Microcoleus")),
                                      expression(italic("Anabaena")~"Spires")))


#### RESPONSE RATIOS ###########################################################

## Extract day0 information
reg.d0 <- reg.data %>%
           filter(Day == 0) %>%
           select(uniqueID, Alpha, ETRm, FvFm) %>%
           rename(Alpha.d0 = Alpha, ETRm.d0 = ETRm, FvFm.d0 = FvFm)

## Calculate response ratio
reg.rr <- reg.data %>%
  left_join(., reg.d0) %>%
  group_by(Day, Location, Algae, uniqueID) %>%
  mutate(Alpha.rr= log(Alpha / Alpha.d0),
         ETRm.rr= log(ETRm / ETRm.d0),
         FvFm.rr= log(FvFm / FvFm.d0)) %>%
  ungroup() %>%
  filter(Day == 1) %>%
  mutate(pair= ifelse(Rep == 1 | Rep == 2, "1:2",
                      ifelse(Rep == 3 | Rep == 4, "3:4", "5:6"))) %>% 
  mutate(facet_order= ifelse(Algae == "Clad_R", "2", 
                              ifelse(Algae == "Clad_Y", "1", 
                                     ifelse(Algae == "Cyano_Spires", "6", 
                                            ifelse(Algae == "Phorm", "5", 
                                                   ifelse(Algae == "Nostoc", "4",
                                                          ifelse(Algae == "Riv", "3", "7")))))))

write_tsv(reg.rr, path= file.path(dir_out_table, "PAM2014_clean_REG2_response_ratios.tsv") )


## Summarize response ratios
reg.rr.s <- reg.rr %>%
              filter(Algae != "Blank") %>%
              group_by(Algae, Location) %>%
              summarize(
                N = length(Alpha.rr),
                mean_Alpha.rr = mean(Alpha.rr),
                sd_Alpha.rr = sd(Alpha.rr),
                se_Alpha.rr = sd_Alpha.rr / sqrt(N),
                mean_ETRm.rr = mean(ETRm.rr),
                sd_ETRm.rr = sd(ETRm.rr),
                se_ETRm.rr = sd_ETRm.rr / sqrt(N),
                mean_FvFm.rr = mean(FvFm.rr),
                sd_FvFm.rr = sd(FvFm.rr),
                se_FvFm.rr = sd_FvFm.rr / sqrt(N)) %>%
              ungroup() %>% 
  mutate(facet_order= ifelse(Algae == "Clad_R", "2", 
                             ifelse(Algae == "Clad_Y", "1", 
                                    ifelse(Algae == "Cyano_Spires", "6", 
                                           ifelse(Algae == "Phorm", "5", 
                                                  ifelse(Algae == "Nostoc", "4",
                                                         ifelse(Algae == "Riv", "3", "7")))))))



## Submerged/Floating response ratios
   ## Calculate the response ratio for floating and Submerged parameter values for day 1 for each replicate

## Set up new data frame
reg.data.treatment.d1 <- reg.data %>%
                          select(-uniqueID, -Treatment, -TreatID) %>%
                          filter(Day == 1) %>%
                          mutate(Rep= ifelse(Rep == 2 | Rep == 4 | Rep == 6, Rep - 1, Rep)) %>%
                          gather(key= param, value= value, Alpha:FvFm) %>%
                          unite(treat_param, Location, param) %>%
                          spread(treat_param, value)

## Calculate response ratios
reg.treat.rr <- reg.data.treatment.d1 %>%
                  group_by(Rep, Algae) %>%
                  mutate(Alpha.rr= log(Floating_Alpha / Submerged_Alpha),
                         ETRm.rr= log(Floating_ETRm / Submerged_ETRm),
                         FvFm.rr= log(Floating_FvFm / Submerged_FvFm)) %>%
                  ungroup()

## Summarize response ratios over replicates
reg.treat.rr.s <- reg.treat.rr %>%
                   group_by(Algae) %>%
                   summarize(
                     N = length(Alpha.rr),
                     mean_Alpha.rr = mean(Alpha.rr),
                     sd_Alpha.rr = sd(Alpha.rr),
                     se_Alpha.rr = sd_Alpha.rr / sqrt(N),
                     mean_ETRm.rr = mean(ETRm.rr),
                     sd_ETRm.rr = sd(ETRm.rr),
                     se_ETRm.rr = sd_ETRm.rr / sqrt(N),
                     mean_FvFm.rr = mean(FvFm.rr),
                     sd_FvFm.rr = sd(FvFm.rr),
                     se_FvFm.rr = sd_FvFm.rr / sqrt(N)) %>%
                   ungroup()



##### PLOTTING PARAMETERS ######################################################
plot_points <- geom_point(aes(fill= Location, shape= Location),
                          position= position_dodge(width= 0.4),
                          color= "black",
                          size= 2)
plot_errorbars <- geom_errorbar(position= position_dodge(width= 0.4), width= 0.5,size= 0.3)
plot_lines <- geom_line(aes(group= Location, linetype= Location), position= position_dodge(width= 0.4), size= 0.3)
yintercept <- geom_hline(yintercept = 0, size= 0.25)
x_axis_day <-   scale_x_discrete(breaks= c("0", "1"), labels= c("23-Jul", "24-Jul"))
treatment.fill <- c("black", "white")
treatment.labels <- c("Submerged", "Floating")
treatment.linetype <- c("solid", "dashed")
treatment.shapes <- c(21, 21)
treatment.legend <- "Treatment"


ETR.rr.label <- expression(rETR[max]~" response ratio (± SE)")
ETR.label <- expression(rETR[max]~" (± SE)")
Alpha.label <- paste("Alpha (± SE)")
Alpha.rr.label <- paste("Alpha response ratio (± SE)")

FvFm.label <- expression(F[v]/F[m]~" (± SE)")
FvFm.rr.label <- expression(F[v]/F[m]~" response ratio (± SE)")


## ggplot themes
# theme_freshSci
source("ggplot_themes.R")


theme_pam_param <- theme(axis.title.x.bottom = element_blank(),
                         axis.line = element_line(size= 0.3),
                         axis.text.x = element_text(angle= 45, hjust= 1),
                         legend.position = "top",
                         legend.box.spacing =  unit(0, "cm"))

#### MAKE PLOTS ################################################################

#### COMBINE RESPONSE RATIO INTO A SINGLE FIGURE ####

reg.rr.s$facet_order <- factor(reg.rr.s$facet_order, 
                               labels= c(expression(italic("Cladophora")~"Yellow"),
                                         expression(italic("Cladophora")~"Red"),
                                         expression(italic("Rivularia")),
                                         expression(italic("Nostoc")),
                                         expression(italic("Microcoleus")),
                                         expression(italic("Anabaena")~"Spires")))

## FvFm Response ratio
FvFm.rr.p <- ggplot(data= reg.rr.s, aes(x= Location,
                                        y= mean_FvFm.rr,
                                        ymax= mean_FvFm.rr + se_FvFm.rr,
                                        ymin= mean_FvFm.rr - se_FvFm.rr))
fvfm.rr.fig <- FvFm.rr.p +
  yintercept +
  plot_errorbars +
  plot_points +
  scale_y_continuous(limits= c(-0.9, 0.3), breaks= seq(-0.8, 0.2, by= 0.2), labels= c("-0.8",  "", "0.04", "", "0.0", "")) +
  scale_fill_manual(values= treatment.fill, name= treatment.legend) +
  scale_shape_manual(values= treatment.shapes, name= treatment.legend) +
  scale_linetype_manual(values= treatment.linetype, name= treatment.legend) +
  labs(x= "Treatment", y= FvFm.rr.label) +
    facet_rep_wrap(~facet_order, nrow= 1, labeller= label_parsed) +
  theme_freshSci + 
  theme_pam_param +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_blank())
        #strip.text = element_text(margin= margin(0, 0, 10, 0, unit= ("pt"))))

fvfm.rr.fig

#ggsave(last_plot(), filename = file.path(dir_out_fig, "FvFm_rr.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)
ggsave(last_plot(), filename = file.path(dir_out_fig, "FvFm_rr.eps"), height= 17.8*0.66, width= 17.8, units= "cm")


## Alpha Response ratio
Alpha.rr.p <- ggplot(data= reg.rr.s, aes(x= Location,
                                         y= mean_Alpha.rr,
                                         ymax= mean_Alpha.rr + se_Alpha.rr,
                                         ymin= mean_Alpha.rr - se_Alpha.rr))

alpha.rr.fig <- Alpha.rr.p +
  yintercept +
  plot_errorbars +
  plot_points +
  scale_y_continuous(limits= c(-1.2, 0.3), breaks= seq(-1.25, 0.25, by= 0.25), labels= c("",  "-1.0", "", "-0.5", "", "0.0", "")) +
  scale_fill_manual(values= treatment.fill, labels= treatment.labels, name= treatment.legend) +
  scale_shape_manual(values= treatment.shapes, labels= treatment.labels, name= treatment.legend) +
  scale_linetype_manual(values= treatment.linetype, labels= treatment.labels, name= treatment.legend) +
  labs(x= "Treatment", y= Alpha.rr.label) +
  facet_rep_wrap(~facet_order, nrow= 1, labeller= label_parsed) +
  theme_freshSci + 
  theme_pam_param +
  theme(#strip.text = element_text(color= "transparent"),
    strip.text = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank())

alpha.rr.fig
#ggsave(last_plot(), filename = file.path(dir_out_fig, "AlphaREG2_rr.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)
ggsave(alpha.rr.fig, filename = file.path(dir_out_fig, "AlphaREG2_rr.eps"), height= 17.8*0.66, width= 17.8, units= "cm")




## ETRm response ratio
ETRm.rr.p <- ggplot(data= reg.rr.s, aes(x= Location,
                                        y= mean_ETRm.rr,
                                        ymax= mean_ETRm.rr + se_ETRm.rr,
                                        ymin= mean_ETRm.rr - se_ETRm.rr))

ETRm.rr.fig <- ETRm.rr.p +
  yintercept +
  plot_errorbars +
  plot_points +
  scale_y_continuous(limits= c(-1, 0.55), breaks= seq(-1, 0.76, by= 0.25), labels= c("-1.0", "", "0.5", "", "0.0", "", "0.5", "")) +
  scale_fill_manual(values= treatment.fill, labels= treatment.labels, name= treatment.legend) +
  scale_shape_manual(values= treatment.shapes, labels= treatment.labels, name= treatment.legend) +
  labs(x= "Treatment", y= ETR.rr.label) +
  facet_rep_wrap(~facet_order, nrow= 1, labeller= label_parsed) +
  theme_freshSci + 
  theme_pam_param +
  theme(#strip.text = element_text(color= "transparent"),
    strip.text = element_blank(),
    axis.title.y = element_blank())

ETRm.rr.fig
# ggsave(last_plot(), filename = file.path(dir_out_fig, "ETRmREG2_rr.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)
ggsave(ETRm.rr.fig, filename = file.path(dir_out_fig, "ETRmREG2_rr.eps"), height= 17.8*0.66, width= 17.8, units= "cm")


#### COMBINE WITH COWPLOT PACKAGE

rr.fig <- plot_grid(fvfm.rr.fig  + theme(legend.position="none"), 
                    alpha.rr.fig + theme(legend.position="none"), 
                    ETRm.rr.fig + theme(legend.position="none"), 
                    ncol= 1, nrow= 3) +
  draw_label(label= expression(bold(paste("F"[v]~"/"~"F"[m]))), x= 0.008, y= 0.95, size= 10, hjust= 0) +
  draw_label(label= expression(bold(paste("Alpha (\U03B1)"))), x= 0.008, y= 0.655, size= 10, hjust= 0) +
  draw_label(label= expression(bold(paste("rETR"[max]))), x= 0.008, y= 0.325, size= 10, hjust= 0)
rr.fig

rr.fig.anno <-  annotate_figure(rr.fig, 
                                left = text_grob(label= "Response ratio (± SE)", 
                                                 rot = 90),
                                
                                bottom= text_grob(label= "Treatment", vjust= -0.5))

ggsave(rr.fig.anno, filename = file.path(dir_out_fig_manuscript, "Fig_5.eps"), height= 17.8, width= 17.8, units= "cm", device= cairo_ps)


##### DATA POINTS FOR SUPPLEMENTAL FIGURES #####

## FvFm data
FvFm.p <- ggplot(data= reg.s, aes(x= as.character(Day),
                                  y= mean_FvFm,
                                  ymax= mean_FvFm + se_FvFm,
                                  ymin= mean_FvFm - se_FvFm,
                                  group= Location))

fvfm.fig <- FvFm.p +
  plot_lines +
  plot_errorbars +
  plot_points +
  scale_y_continuous(limits= c(0.12, 0.63), breaks= seq(0.1, 0.6, by= 0.1), labels= c("", "0.2", "", "0.4", "", "0.6")) +
  x_axis_day +
  scale_fill_manual(values= treatment.fill, name= treatment.legend) +
  scale_shape_manual(values= treatment.shapes, name= treatment.legend) +
  scale_linetype_manual(values= treatment.linetype, name= treatment.legend) +
  labs(x= "Treatment", y= FvFm.rr.label) +
  facet_rep_wrap(~facet_order, nrow= 1, labeller= label_parsed) +
  theme_freshSci + 
  theme_pam_param +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        legend.margin = margin(0.1, 0.1, 0.1, 0.1, unit= "cm"),
        legend.key.width = unit(1, "cm"),
        legend.box.spacing = unit(0, "cm"))


## Alpha data
Alpha.p <- ggplot(data= reg.s, aes(x= as.character(Day),
                                   y= mean_Alpha,
                                   ymax= mean_Alpha + se_Alpha,
                                   ymin= mean_Alpha - se_Alpha,
                                   group= Location))

alpha.fig <- Alpha.p +
  plot_lines +
  plot_errorbars +
  plot_points +
  scale_y_continuous(limits= c(0.04, 0.167), breaks= seq(0.04, 0.16, by= 0.02), labels= c("0.04", "", "0.08", "", "0.12", "", "0.16")) +
  x_axis_day +
  scale_fill_manual(values= treatment.fill, labels= treatment.labels, name= treatment.legend) +
  scale_shape_manual(values= treatment.shapes, labels= treatment.labels, name= treatment.legend) +
  scale_linetype_manual(values= treatment.linetype, labels= treatment.labels, name= treatment.legend) +
  labs(x= "Treatment", y= Alpha.rr.label) +
  facet_rep_wrap(~facet_order, nrow= 1, labeller= label_parsed) +
  theme_freshSci +
  theme_pam_param +
  theme(#strip.text = element_text(color= "transparent"),
    strip.text = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank())

## ETRm response ratio
ETRm.p <- ggplot(data= reg.s, aes(x= as.character(Day),
                                  y= mean_ETRm,
                                  ymax= mean_ETRm + se_ETRm,
                                  ymin= mean_ETRm - se_ETRm,
                                  group= Location))

ETRm.fig <- ETRm.p +
  plot_lines +
  plot_errorbars +
  plot_points +
  scale_y_continuous(limits= c(17, 80), breaks= seq(20, 80, by= 10), labels= c("20", "", "40", "", "60", "", "80")) +
  x_axis_day +
  scale_fill_manual(values= treatment.fill, labels= treatment.labels, name= treatment.legend) +
  scale_shape_manual(values= treatment.shapes, labels= treatment.labels, name= treatment.legend) +
  labs(x= "Treatment", y= ETR.rr.label) +
  facet_rep_wrap(~facet_order, nrow= 1, labeller= label_parsed) +
  theme_freshSci + 
  theme_pam_param +
  theme(#strip.text = element_text(color= "transparent"),
    strip.text = element_blank(),
    axis.title.y = element_blank())

#### COMBINE FIGURES #####
data_point.fig <- plot_grid(fvfm.fig + theme(legend.position="none"), 
                    alpha.fig + theme(legend.position="none"), 
                    ETRm.fig + theme(legend.position="none"), 
                    ncol= 1, nrow= 3,
                    axis= "l", align= "v") +
  draw_label(label= expression(bold(paste("F"[v]~"/"~"F"[m]))), x= 0.008, y= 0.95, size= 10, hjust= 0) +
  draw_label(label= expression(bold(paste("Alpha (\U03B1)"))), x= 0.008, y= 0.655, size= 10, hjust= 0) +
  draw_label(label= expression(bold(paste("rETR"[max]))), x= 0.008, y= 0.325, size= 10, hjust= 0)


data_point.fig.anno <-  annotate_figure(data_point.fig, 
                                left = text_grob(label= "Value (± SE)", 
                                                 rot = 90),
                                
                                bottom= text_grob(label= "Date (2014)", vjust= -0.5))

legend <- get_legend(fvfm.fig + theme(legend.margin = margin(20, 0, 0, 48, unit= "points"),
                                      legend.box.spacing = unit(0, "cm")))
data_point.fig.anno.legend <- plot_grid(legend, data_point.fig.anno,
          nrow=2,
          rel_heights = c(0.02, 1))

ggsave(data_point.fig.anno.legend, filename = file.path(dir_out_fig_manuscript, "Fig_S3.eps"), height= 17.8, width= 17.8, units= "cm", device= cairo_ps)

