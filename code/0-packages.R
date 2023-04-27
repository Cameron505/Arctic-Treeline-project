library(tidyverse) #for tidy processing and plotting
library(vegan) # for PERMANOVA analysishttp://127.0.0.1:10019/graphics/plot_zoom_png?width=750&height=900
library(ggbiplot) #for PCA biplots
library(lubridate)
library(ggExtra)
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
