### GGplot themes
library(ggplot2)

theme_freshSci <- theme(panel.grid = element_blank(),
                        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
                        text = element_text(size= 10, family= "Arial"),
                        plot.background = element_rect(fill = "transparent", color= "transparent"), # bg of the plot
                        panel.background = element_rect(fill= "transparent", color= "transparent"),
                        panel.border= element_rect(fill= NA, color= NA, linetype= "solid", size= 1),
                        panel.ontop = TRUE,
                        axis.line = element_line(color= "black", size= 0.25),
                        axis.text = element_text(colour="black"),
                        axis.title.x = element_text(vjust = -0.75),
                        axis.title.y = element_text(vjust = 1.5),
                        #strip.background = element_rect(fill="transparent", color= "transparent"),
                        strip.background = element_blank(),
                        legend.background = element_rect(color=NULL, fill= "transparent"),
                        legend.key = element_blank(),
                        legend.justification = c(0, 1),
                        legend.box.margin = margin(0, 0, 0, 0, unit= "cm"),
                        legend.text = element_text(size= 8),
                        legend.title = element_text(size= 8)
                        )


# theme_pam <- theme(panel.grid = element_blank(),
#                    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
#                    text = element_text(size= 14),
#                    plot.background = element_rect(fill = "transparent", color= "transparent"), # bg of the plot
#                    panel.background = element_rect(fill= "transparent", color= "transparent"),
#                    panel.border= element_rect(fill= NA, color= "black", linetype= "solid", size= 1),
#                    panel.ontop = TRUE,
#                    axis.text = element_text(colour="black"),
#                    axis.title.x = element_text(vjust = -0.75),
#                    axis.title.y = element_text(vjust = 1.5),
#                    legend.background = element_rect(size=0.25, color=NULL, fill= "transparent"),
#                    legend.key = element_blank(),
#                    strip.background = element_rect(fill="transparent", color= "transparent"),
#                    axis.text.x = element_text(angle= 45, hjust= 1),
#                    legend.position = "top",
#                    legend.justification = "left")

