
library(vegan) # for PERMANOVA analysishttp://127.0.0.1:10019/graphics/plot_zoom_png?width=750&height=900
library(ggbiplot) #for PCA biplots
library(tidyverse) #for tidy processing and plotting
library(lubridate)
library(ggExtra)
library(ggbreak)
library(reshape2)
library(pracma)
library(ggpubr)
library(doBy)
library(agricolae)
library(cowplot)
#devtools::install_version("ggplot2", "3.4.1") V3.4.2 broke the markdown code with the descrete_range
# to install {ggbiplot}:
# library(devtools)
# install_github("vqv/ggbiplot")



# custom ggplot theme
options(ggplot2.discrete.color = PNWColors::pnw_palette("Bay", 3))
reorder_seasons = function(dat){
  dat %>% mutate(Season = factor(Season, levels = c("Spring", "LateSpring", "Fall")))
}

reorder_sites = function(dat){
  dat %>% mutate(Site = factor(Site, levels = c("Xeric", "Mesic", "Hydric")))
  
}
cbPalette2 <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black")
theme_kp <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.position = "top",
          legend.key=element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'lines'),
          legend.background = element_rect(colour = NA),
          panel.border = element_rect(color="black",size=1.5, fill = NA),
          
          plot.title = element_text(hjust = 0, size = 14),
          axis.text = element_text(size = 14, color = "black"),
          axis.title = element_text(size = 14, face = "bold", color = "black"),
          
          # formatting for facets
          panel.background = element_blank(),
          strip.background = element_rect(colour="white", fill="white"), #facet formatting
          panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
          panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=12, face="bold"), #facet labels
          strip.text.y = element_text(size=12, face="bold", angle = 270) #facet labels
    )
}

theme_set(theme_kp())


Combine_plots <- function(gg_NH4_Extract,gg_NO3_Extract,gg_PO4_Extract ) {  
  
  legend = cowplot::get_legend(gg_NH4_Extract+ guides(color = guide_legend(nrow = 1)) +
                                 theme(legend.position = "bottom"))
  gg_Resin_combine= cowplot::plot_grid(
    gg_NH4_Extract + theme(legend.position="none",axis.title.x=element_blank()),
    gg_NO3_Extract + theme(legend.position="none",axis.title.x=element_blank()),
    gg_PO4_Extract + theme(legend.position="none"),
    align = 'vh',
    labels = c("A", "B", "C"),
    label_y= 0.93,
    hjust = -1,
    ncol = 1
  )
  gg_Legend=cowplot::plot_grid(gg_Resin_combine,legend, ncol=1, rel_heights =c(1,0.05))
  
  
  
  list(gg_Legend=gg_Legend)
}


Combine_plots2 <- function(gg_NH4_Extract,gg_NO3_Extract) {  
  
  legend = cowplot::get_legend(gg_NH4_Extract+ guides(color = guide_legend(nrow = 1)) +
                                 theme(legend.position = "bottom"))
  gg_Resin_combine= cowplot::plot_grid(
    gg_NH4_Extract + theme(legend.position="none",axis.title.x=element_blank()),
    gg_NO3_Extract + theme(legend.position="none"),
    align = 'vh',
    labels = c("A", "B"),
    label_y= 0.93,
    hjust = -1,
    ncol = 1
  )
  gg_Legend=cowplot::plot_grid(gg_Resin_combine,legend, ncol=1, rel_heights =c(1,0.05))
  
  
  
  list(gg_Legend=gg_Legend)
}

length2= function (x, na.rm=FALSE) {
  if (na.rm) sum(!is.na(x))
  else       length(x)
}


fit_hsd = function(dat){
  a = aov(conc ~ Temp, data = dat)
  h = HSD.test(a, "Temp")
  h$groups %>% mutate(Temp = row.names(.)) %>%
    dplyr::rename(label = groups) %>%  
    dplyr::select(Temp, label)
}



