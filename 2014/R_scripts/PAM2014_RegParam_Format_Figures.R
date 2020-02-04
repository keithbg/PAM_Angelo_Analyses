## Read in and format and plot PI curve data  from the PAM 2014 experiment
## with Yvonne, Paula, and Mary

## Created by KBG January-2016
## Revised by KBG August-2018

#### LIBRARIES #################################################################
library(tidyverse)
library(ggplot2)
library(stringr)
################################################################################

#### FILE PATHS ################################################################
dir_input <- file.path("/Users", "kbg", "Dropbox", "PAM_Angelo", "PAM_Angelo_Analyses", "2014", "PAM_data")
dir_out_fig <- file.path("/Users", "kbg", "Dropbox", "PAM_Angelo", "PAM_Angelo_Analyses", "2014", "Figures")
dir_out_table <- file.path("/Users", "kbg", "Dropbox", "PAM_Angelo", "PAM_Angelo_Analyses", "2014", "PAM_data")
################################################################################



#### REGRESSION PARAMETERS CALCULATED BY KBG ###################################
#### in script PAM2014_PI_curve_Format.R
reg.data <- read_tsv(file.path(dir_input, "PAM2014_clean_REG_params.tsv")) %>%
             select(-contains("REG1"), -REG2.RSS) %>%
             rename(Alpha= REG2.alpha, ETRm= REG2.ETRm, Ek= REG2.Ek) %>% 
             mutate(Location= as.factor(ifelse(Location == "Benthic", "Submerged", "Floating")))
reg.data$Location <- factor(reg.data$Location, levels= levels(reg.data$Location)[c(2, 1)])


##### PREVIOUS CALCULATIONS FROM YVONNE #######################################
#### Read in and format data ####
#   r0 <- read_csv(file.path(dir_input,"raw_data", "Algae_Type_Day0_REG2_KBG.csv"))
#   r1 <- read_csv(file.path(dir_input, "raw_data", "Algae_Type_Day1_REG2_KBG.csv"))
#   reg <- rbind(r0, r1)
#
#
# #### ALPHA AND ETRmax
# reg <- reg %>%
#          mutate(
#            Date= as.Date(.$Date, "%m/%d/%y"),
#            Location= ifelse((reg$Rep == 1 | reg$Rep ==3 | reg$Rep == 5), "Benthic", "Floating"),
#            Day= as.character(ifelse(Date == "2014-07-23", 0, 1)),
#            TreatID= as.character(paste(Day, Algae, Location, sep=".")),
#            uniqueID= as.factor(paste(Algae, Treatment, Rep, sep="."))
#          ) %>%
#         arrange(Date, Algae, Location)
#
# ## Format algae names
# reg <- reg %>%
#         mutate(Algae= ifelse(Algae == "Cyano Spires", "Spires",
#                              ifelse(Algae == "Nostoc brains", "Nostoc",
#                                     ifelse(Algae == "Phormidium", "Microcoleus",
#                                            ifelse(Algae == "Rusty Cladophora", "Clad_R",
#                                                   ifelse(Algae == "Yellow Cladophora", "Clad_Y", Algae))))))
#

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
  mutate(Day= as.character(Day),
         facet_order= ifelse(Algae == "Clad_R", "1", 
                             ifelse(Algae == "Clad_Y", "2", 
                                    ifelse(Algae == "Cyano_Spires", "3", 
                                           ifelse(Algae == "Phorm", "4", 
                                                  ifelse(Algae == "Nostoc", "5",
                                                         ifelse(Algae == "Riv", "6", "7")))))))
# levels(reg.s$facet_order) <- c("atop(italic(Cladophora), Red)", 
#                                "atop(italic(Cladophora), Yellow)", 
#                                "atop(italic(Anabaena), Spires)", 
#                                "italic(Microcoleus)", 
#                                "italic(Nostoc)", 
#                                "italic(Rivularia)", 
#                                "Blank") 
       


#### Fv/Fm, Fo ####
lc.df <- read_tsv(file.path(dir_input, "PAM2014_clean_light_curve.tsv"))

FvFM.Fo.df <- lc.df %>%
  filter(Type == "FO", F0_Menupoint == "17") %>%
  group_by(Day, Location, Algae, TreatID) %>%
  summarise(
    N = length(Fo),
    mean_Fo = mean(Fo),
    sd_Fo = sd(Fo),
    se_Fo = sd_Fo / sqrt(N),
    mean_FvFm = mean(FvFm),
    sd_FvFm = sd(FvFm),
    se_FvFm = sd_FvFm / sqrt(N)
  )



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
  mutate(facet_order= ifelse(Algae == "Clad_R", "1", 
                             ifelse(Algae == "Clad_Y", "2", 
                                    ifelse(Algae == "Cyano_Spires", "3", 
                                           ifelse(Algae == "Phorm", "4", 
                                                  ifelse(Algae == "Nostoc", "5",
                                                         ifelse(Algae == "Riv", "6", "7")))))))

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
  mutate(facet_order= ifelse(Algae == "Clad_R", "1", 
                             ifelse(Algae == "Clad_Y", "2", 
                                    ifelse(Algae == "Cyano_Spires", "3", 
                                           ifelse(Algae == "Phorm", "4", 
                                                  ifelse(Algae == "Nostoc", "5",
                                                         ifelse(Algae == "Riv", "6", "7")))))))

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
x_axis_day <- scale_x_discrete(breaks= c(0, 1), labels= c("23-Jul", "24-Jul"))
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


algae.facet.labels <- as_labeller(c(`3` = "Anabaena\nSpires",
                                    `5` = "Nostoc",
                                    `4`= "Microcoleus",
                                    `6` = "Rivularia",
                                    `1` = "Cladophora\nRed",
                                    `2` = "Cladophora\nYellow",
                                    `7` = "Blank"))




## ggplot themes
theme_pam <- theme(panel.grid = element_blank(),
                   plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
                   text = element_text(size= 14),
                   plot.background = element_rect(fill = "transparent", color= "transparent"), # bg of the plot
                   panel.background = element_rect(fill= "transparent", color= "transparent"),
                   panel.border= element_rect(fill= NA, color= "black", linetype= "solid", size= 1),
                   panel.ontop = TRUE,
                   axis.text = element_text(colour="black"),
                   axis.title.x = element_text(vjust = -0.75),
                   axis.title.y = element_text(vjust = 1.5),
                   legend.background = element_rect(size=0.25, color=NULL, fill= "transparent"),
                   legend.key = element_blank(),
                   strip.background = element_rect(fill="transparent", color= "transparent"),
                   axis.text.x = element_text(angle= 45, hjust= 1),
                   legend.position = "top",
                   legend.justification = "left")

theme_freshSci <- theme(panel.grid = element_blank(),
                   plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
                   text = element_text(size= 10),
                   plot.background = element_rect(fill = "transparent", color= "transparent"), # bg of the plot
                   panel.background = element_rect(fill= "transparent", color= "transparent"),
                   panel.border= element_rect(fill= NA, color= "black", linetype= "solid", size= 1),
                   panel.ontop = TRUE,
                   axis.text = element_text(colour="black"),
                   axis.title.x = element_text(vjust = -0.75),
                   axis.title.y = element_text(vjust = 1.5),
                   #strip.background = element_rect(fill="transparent", color= "transparent"),
                   strip.background = element_blank(),
                   axis.text.x = element_text(angle= 45, hjust= 1),
                   legend.background = element_rect(size=0.25, color=NULL, fill= "transparent"),
                   legend.key = element_blank(),
                   legend.position = "top",
                   #legend.position = c(0, 0.5),
                   legend.direction = "horizontal",
                   legend.justification = "left",
                   legend.box.margin = margin(0, 0, 0, 0, unit= "cm"),
                   legend.box.spacing =  unit(0, "cm"))


#### MAKE PLOTS ################################################################


## ETR max
  ETRm.p <- ggplot(data= reg.s, aes(x= Day,
                                    y= mean_ETRm,
                                    ymax= mean_ETRm + se_ETRm,
                                    ymin= mean_ETRm - se_ETRm,
                                    group= Location))

  ## ETRm actual values
  ETRm.p +
    plot_lines +
    plot_errorbars +
    plot_points +
    scale_y_continuous(limits= c(17, 80), breaks= seq(20, 80, by= 10), labels= c("20", "", "40", "", "60", "", "80")) +
    scale_fill_manual(values= treatment.fill, labels= treatment.labels, name= treatment.legend) +
    scale_shape_manual(values= treatment.shapes, labels= treatment.labels, name= treatment.legend) +
    scale_linetype_manual(values= treatment.linetype, labels= treatment.labels, name= treatment.legend) +
    labs(x= "", y= ETR.label) +
    x_axis_day +
    #facet_grid(.~Algae, labeller= labeller(Algae= algae.facet.labels)) +
    facet_grid(.~facet_order, labeller= labeller(facet_order= algae.facet.labels)) +
    #facet_grid(.~facet_order, labeller= label_parsed) +
    theme_freshSci + #theme_pam
    theme(axis.title.x.bottom = element_blank())
  #ggsave(last_plot(), filename = file.path(dir_out_fig, "ETRmREG2.pdf"), height= 8.4, width= 12.7, units= "cm", device = cairo_pdf)
  ggsave(last_plot(), filename = file.path(dir_out_fig, "ETRmREG2.eps"), height= 17.8*0.66, width= 17.8, units= "cm")
  

  ## ETRm response ratio
  ETRm.rr.p <- ggplot(data= reg.rr.s, aes(x= Location,
                                       y= mean_ETRm.rr,
                                       ymax= mean_ETRm.rr + se_ETRm.rr,
                                       ymin= mean_ETRm.rr - se_ETRm.rr))

  ETRm.rr.p +
    yintercept +
    plot_errorbars +
    plot_points +
    scale_y_continuous(limits= c(-1, 0.55), breaks= seq(-1, 0.76, by= 0.25), labels= c("-1.0", "", "0.5", "", "0.0", "", "0.5", "")) +
    scale_fill_manual(values= treatment.fill, labels= treatment.labels, name= treatment.legend, guide= FALSE) +
    scale_shape_manual(values= treatment.shapes, labels= treatment.labels, name= treatment.legend, guide= FALSE) +
    #scale_linetype_manual(values= treatment.linetype, labels= treatment.labels, name= treatment.legend, guide= FALSE) +
    labs(x= "Treatment", y= ETR.rr.label) +
   # facet_grid(.~Algae, labeller= labeller(Algae= algae.facet.labels)) +
    facet_grid(.~facet_order, labeller= labeller(facet_order= algae.facet.labels)) +
    theme_freshSci  + # theme_pam
 # ggsave(last_plot(), filename = file.path(dir_out_fig, "ETRmREG2_rr.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)
  ggsave(last_plot(), filename = file.path(dir_out_fig, "ETRmREG2_rr.eps"), height= 17.8*0.66, width= 17.8, units= "cm")
  

  ## ETRm response ratio, data points
  ETRm.rr.p.paired <- ggplot(data= reg.rr, aes(x= Location, y= ETRm.rr, group= pair))

  ETRm.rr.p.paired +
    yintercept +
    geom_line(aes(color= pair)) +
    geom_point(aes(color= pair), size= 3) +
    scale_y_continuous(limits= c(-1, 0.55), breaks= seq(-1, 0.76, by= 0.25), labels= c("-1.0", "", "0.5", "", "0.0", "", "0.5", "")) +
    scale_color_discrete(name= "Replicate pair") +
    #scale_shape_manual(values= treatment.shapes) +
    scale_linetype_manual(values= treatment.linetype, name= treatment.legend) +
    labs(x= "Treatment", y= ETR.label) +
    facet_grid(.~Algae, labeller= labeller(Algae= algae.facet.labels)) +
    theme_pam
  ggsave(last_plot(), filename = file.path(dir_out_fig, "ETRmREG2_rr_paired.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)


  ## ETRm response ratio day 1
  ETRm.rr.d1.p <- ggplot(data= reg.treat.rr, aes(x= Algae,
                                          y= ETRm.rr)) #,
                                          #ymax= mean_ETRm.rr + se_ETRm.rr,
                                          #ymin= mean_ETRm.rr - se_ETRm.rr))

  ETRm.rr.d1.p +
    yintercept +
    geom_point() +
   # plot_errorbars +
    #plot_points
    #cale_y_continuous(limits= c(-1, 0.55), breaks= seq(-1, 0.76, by= 0.25), labels= c("-1.0", "", "0.5", "", "0.0", "", "0.5", "")) +
    scale_x_discrete(labels= NULL) +
    labs(x= "Algae", y= "ETRmax response ratio", title= "Day 1 submerged & floating response ratio") +
    facet_grid(.~Algae, labeller= labeller(Algae= algae.facet.labels), scales= "free_x") +
    theme_pam
  ggsave(last_plot(), filename = file.path(dir_out_fig, "ETRmREG2_rr_d1_p1.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)

  ETRm.rr.d1.p2 <- ggplot(data= reg.treat.rr.s, aes(x= Algae,
                                                   y= mean_ETRm.rr,
                                                   ymax= mean_ETRm.rr + se_ETRm.rr,
                                                   ymin= mean_ETRm.rr - se_ETRm.rr))
  ETRm.rr.d1.p2 +
    yintercept +
    geom_point() +
    plot_errorbars +
    #plot_points
    #scale_y_continuous(limits= c(-1, 0.55), breaks= seq(-1, 0.76, by= 0.25), labels= c("-1.0", "", "0.5", "", "0.0", "", "0.5", "")) +
    scale_x_discrete(labels= NULL) +
    labs(x= "Algae", y= "ETRmax response ratio (± se)", title= "Day 1 submerged & floating response ratio") +
    facet_grid(.~Algae, labeller= labeller(Algae= algae.facet.labels), scales= "free_x") +
    theme_pam
  ggsave(last_plot(), filename = file.path(dir_out_fig, "ETRmREG2_rr_d1_p2.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)





## Alpha
  Alpha.p <- ggplot(data= reg.s, aes(x= Day,
                                     y= mean_Alpha,
                                     ymax= mean_Alpha + se_Alpha,
                                     ymin= mean_Alpha - se_Alpha,
                                     group= Location))

  Alpha.p +
    plot_lines +
    plot_errorbars +
    plot_points +
    scale_y_continuous(limits= c(0.04, 0.167), breaks= seq(0.04, 0.16, by= 0.02), labels= c("0.04", "", "0.08", "", "0.12", "", "0.16")) +
    scale_fill_manual(values= treatment.fill, labels= treatment.labels, name= treatment.legend) +
    scale_shape_manual(values= treatment.shapes, labels= treatment.labels, name= treatment.legend) +
    scale_linetype_manual(values= treatment.linetype, labels= treatment.labels, name= treatment.legend) +
    labs(x= "", y= Alpha.label) +
    x_axis_day +
    facet_grid(.~Algae, labeller= labeller(Algae= algae.facet.labels)) +
    theme_pam
  ggsave(last_plot(), filename = file.path(dir_out_fig, "AlphaREG2.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)

  ## Alpha Response ratio
  Alpha.rr.p <- ggplot(data= reg.rr.s, aes(x= Location,
                                          y= mean_Alpha.rr,
                                          ymax= mean_Alpha.rr + se_Alpha.rr,
                                          ymin= mean_Alpha.rr - se_Alpha.rr))

  Alpha.rr.p +
    yintercept +
    plot_lines +
    plot_errorbars +
    plot_points +
    scale_y_continuous(limits= c(-1.2, 0.3), breaks= seq(-1.25, 0.25, by= 0.25), labels= c("",  "-1.0", "", "-0.5", "", "0.0", "")) +
    scale_fill_manual(values= treatment.fill, labels= treatment.labels, name= treatment.legend) +
    scale_shape_manual(values= treatment.shapes, labels= treatment.labels, name= treatment.legend) +
    scale_linetype_manual(values= treatment.linetype, labels= treatment.labels, name= treatment.legend) +
    labs(x= "Treatment", y= Alpha.rr.label) +
    facet_grid(.~Algae, labeller= labeller(Algae= algae.facet.labels)) +
    theme_pam
  ggsave(last_plot(), filename = file.path(dir_out_fig, "AlphaREG2_rr.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)

  ## Alpha response ratio, data points
  Alpha.rr.p.paired <- ggplot(data= reg.rr, aes(x= Location, y= Alpha.rr, group= pair))

  Alpha.rr.p.paired +
    yintercept +
    geom_line(aes(color= pair)) +
    geom_point(aes(color= pair), size= 3) +
    scale_y_continuous(limits= c(-1.5, 0.3), breaks= seq(-1.55, 0.25, by= 0.25), labels= c("-1.5", "",  "-1.0", "", "-0.5", "", "0.0", "")) +
    scale_color_discrete(name= "Replicate pair") +
    #scale_shape_manual(values= treatment.shapes) +
    scale_linetype_manual(values= treatment.linetype) +
    labs(x= "Treatment", y= "Alpha response ratio") +
    facet_grid(.~Algae, labeller= labeller(Algae= algae.facet.labels)) +
    theme_pam
  ggsave(last_plot(), filename = file.path(dir_out_fig, "AlphaREG2_rr_paired.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)



  Alpha.rr.d1.p2 <- ggplot(data= reg.treat.rr.s, aes(x= Algae,
                                                     y= mean_Alpha.rr,
                                                     ymax= mean_Alpha.rr + se_Alpha.rr,
                                                     ymin= mean_Alpha.rr - se_Alpha.rr))

  Alpha.rr.d1.p2 +
    yintercept +
    geom_point() +
    plot_errorbars +
    #plot_points
    #scale_y_continuous(limits= c(-1, 0.55), breaks= seq(-1, 0.76, by= 0.25), labels= c("-1.0", "", "0.5", "", "0.0", "", "0.5", "")) +
    scale_x_discrete(labels= NULL) +
    labs(x= "Algae", y= "Alpha response ratio (± se)", title= "Day 1 submerged & floating response ratio") +
    facet_grid(.~Algae, labeller= labeller(Algae= algae.facet.labels), scales= "free_x") +
    theme_pam
  ggsave(last_plot(), filename = file.path(dir_out_fig, "AlphaREG2_rr_d1_p2.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)



  ## Fv/Fm
  FvFm.p <- ggplot(data= reg.s, aes(x= Day,
                                     y= mean_FvFm,
                                     ymax= mean_FvFm + se_FvFm,
                                     ymin= mean_FvFm - se_FvFm,
                                     group= Location))

  FvFm.p +
    plot_lines +
    plot_errorbars +
    plot_points +
    scale_y_continuous(limits= c(0.12, 0.63), breaks= seq(0.1, 0.6, by= 0.1), labels= c("", "0.2", "", "0.4", "", "0.6")) +
    scale_fill_manual(values= treatment.fill, name= treatment.legend) +
    scale_shape_manual(values= treatment.shapes, name= treatment.legend) +
    scale_linetype_manual(values= treatment.linetype, name= treatment.legend) +
    labs(x= "", y= FvFm.label) +
    x_axis_day +
    facet_grid(.~Algae, labeller= labeller(Algae= algae.facet.labels)) +
    theme_pam
  ggsave(last_plot(), filename = file.path(dir_out_fig, "FvFm.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)

  ## FvFm Response ratio
  FvFm.rr.p <- ggplot(data= reg.rr.s, aes(x= Location,
                                           y= mean_FvFm.rr,
                                           ymax= mean_FvFm.rr + se_FvFm.rr,
                                           ymin= mean_FvFm.rr - se_FvFm.rr))

  FvFm.rr.p +
    yintercept +
    plot_lines +
    plot_errorbars +
    plot_points +
    scale_y_continuous(limits= c(-0.9, 0.3), breaks= seq(-0.8, 0.2, by= 0.2), labels= c("-0.8",  "", "0.04", "", "0.0", "")) +
    scale_fill_manual(values= treatment.fill, name= treatment.legend) +
    scale_shape_manual(values= treatment.shapes, name= treatment.legend) +
    scale_linetype_manual(values= treatment.linetype, name= treatment.legend) +
    labs(x= "Treatment", y= FvFm.rr.label) +
    facet_grid(.~Algae, labeller= labeller(Algae= algae.facet.labels)) +
    theme_pam
  ggsave(last_plot(), filename = file.path(dir_out_fig, "FvFm_rr.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)

  ## FvFm response ratio, data points
  FvFm.rr.p.paired <- ggplot(data= reg.rr, aes(x= Location, y= FvFm.rr, group= pair))

  FvFm.rr.p.paired +
    yintercept +
    geom_line(aes(color= pair)) +
    geom_point(aes(color= pair), size= 3) +
    scale_y_continuous(limits= c(-0.95, 0.3), breaks= seq(-0.8, 0.2, by= 0.2), labels= c("-0.8",  "", "0.04", "", "0.0", "")) +
    scale_color_discrete(name= "Replicate pair") +
    scale_linetype_manual(values= treatment.linetype, name= treatment.legend) +
    labs(x= "Treatment", y= FvFm.rr.label) +
    facet_grid(.~Algae, labeller= labeller(Algae= algae.facet.labels)) +
    theme_pam
  ggsave(last_plot(), filename = file.path(dir_out_fig, "FvFm_rr_paired.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)



  FvFm.rr.d1.p2 <- ggplot(data= reg.treat.rr.s, aes(x= Algae,
                                                    y= mean_FvFm.rr,
                                                    ymax= mean_FvFm.rr + se_FvFm.rr,
                                                    ymin= mean_FvFm.rr - se_FvFm.rr))

  FvFm.rr.d1.p2 +
    yintercept +
    geom_point() +
    plot_errorbars +
    #plot_points
    #scale_y_continuous(limits= c(-1, 0.55), breaks= seq(-1, 0.76, by= 0.25), labels= c("-1.0", "", "0.5", "", "0.0", "", "0.5", "")) +
    scale_x_discrete(labels= NULL) +
    labs(x= "Algae", y= FvFm.rr.label, title= "Day 1 submerged & floating response ratio") +
    facet_grid(.~Algae, labeller= labeller(Algae= algae.facet.labels), scales= "free_x") +
    theme_pam
  ggsave(last_plot(), filename = file.path(dir_out_fig, "FvFm_rr_d1_p2.pdf"), height= 6.4, width= 8, units= "in", device = cairo_pdf)

