#' AQvis_plotTheme
#' 
#' Theming for AQvis_plotTheme
#' 
#' @export


AQvis_plotTheme = function(){
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 10),
        strip.placement = "outside",
        plot.title = element_text(size = 20,hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        aspect.ratio = 9/16,
        legend.position = "bottom",
        axis.text = element_text(size = 15,face = "bold",colour = "black"),
        axis.title = element_text(size = 15),
        plot.background = element_rect(fill = "white",
                                       colour = "white"),
        panel.background = element_rect(fill = "white",colour = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks = element_line(colour = "black"),
        #axis.line = element_line(colour = "black"), 
        panel.border = element_rect(colour = "black",fill = NA),
        legend.text = element_text(colour = "black", size = 12),
        legend.title = element_text(colour = "black", size = 15),
        legend.key = element_blank(),
        legend.background = element_rect(fill = "white", colour = "white"))
}