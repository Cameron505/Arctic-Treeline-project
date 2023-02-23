plot_Extract_Snowfence = function(Extract_processed){
  
  gg_NH4_Extract =
    Extract_processed %>%
    ggplot(aes(x=MONTH, y=NH4, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Month")+
    ggtitle("NH4")
  
  
  gg_NO3_Extract =
    Extract_processed %>%
    ggplot(aes(x=MONTH, y=NO3, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Month")+
    ggtitle("NO3")
  
  gg_PO4_Extract =
    Extract_processed %>%
    ggplot(aes(x=MONTH, y=PO4, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Month")+
    ggtitle("PO4")
  
  gg_TRS_Extract =
    Extract_processed %>%
    ggplot(aes(x=MONTH, y=TRS, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Month")+
    ggtitle("TRS")
  
  gg_phenolics_Extract =
    Extract_processed %>%
    ggplot(aes(x=MONTH, y=phenolics, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Month")+
    ggtitle("phenolics")
  
  gg_TFPA_Extract =
    Extract_processed %>%
    ggplot(aes(x=MONTH, y=TFPA, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Month")+
    ggtitle("TFPA")
  
  gg_MBC_Extract =
    Extract_processed %>%
    ggplot(aes(x=MONTH, y=MBC, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Month")+
    ggtitle("MBC")
  
  gg_MBN_Extract =
    Extract_processed %>%
    ggplot(aes(x=MONTH, y=MBN, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Month")+
    ggtitle("MBN")
  
  gg_MBP_Extract =
    Extract_processed %>%
    ggplot(aes(x=MONTH, y=Mic.PO4, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Month")+
    ggtitle("MBP")
  
  list("Snowfence Vs. Control NH4"= gg_NH4_Extract,
       "Snowfence Vs. Control NO3"= gg_NO3_Extract,
       "Snowfence Vs. Control PO4"= gg_PO4_Extract,
       "Snowfence Vs. Control TFPA"= gg_TFPA_Extract,
       "Snowfence Vs. Control Phenolics"= gg_phenolics_Extract,
       "Snowfence Vs. Control TRS"= gg_TRS_Extract,
       "Snowfence Vs. Control MBC"= gg_MBC_Extract,
       "Snowfence Vs. Control MBN"= gg_MBN_Extract,
       "Snowfence Vs. Control MBP"= gg_MBP_Extract
       )
  
}

plot_Extract_Snowfence_H2O = function(Extract_Processed_H2O){
  
  gg_NH4_Extract =
    Extract_Processed_H2O %>%
    ggplot(aes(x=MONTH, y=NH4.H2O, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Month")+
    ggtitle("Water Extracted NH4")
  
  
  gg_NO3_Extract =
    Extract_Processed_H2O %>%
    ggplot(aes(x=MONTH, y=NO3.H2O, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Month")+
    ggtitle("Water Extracted NO3")
  
  gg_PO4_Extract =
    Extract_Processed_H2O %>%
    ggplot(aes(x=MONTH, y=PO4.H2O, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Month")+
    ggtitle("Water Extracted PO4")
  
  gg_TRS_Extract =
    Extract_Processed_H2O %>%
    ggplot(aes(x=MONTH, y=TRS.H2O, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Month")+
    ggtitle("Water Extracted TRS")
  
  
  gg_TFPA_Extract =
    Extract_Processed_H2O %>%
    ggplot(aes(x=MONTH, y=TFPA.H2O, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Month")+
    ggtitle("Water Extracted TFPA")
  
  
  
  list("Snowfence Vs. Control H2O NH4"= gg_NH4_Extract,
       "Snowfence Vs. Control H2O NO3"= gg_NO3_Extract,
       "Snowfence Vs. Control H2O PO4"= gg_PO4_Extract,
       "Snowfence Vs. Control H2O TFPA"= gg_TFPA_Extract,
       "Snowfence Vs. Control H2O TRS"= gg_TRS_Extract
  )
  
}

plot_Extract_Seasonal = function(Extract_processed_Seasonal){
  
  gg_NH4_Extract =
    Extract_processed_Seasonal %>%
    ggplot(aes(x=DATE, y=NH4, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 2) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Month")+
    ggtitle("NH4")
  
  
  gg_NO3_Extract =
    Extract_processed_Seasonal %>%
    ggplot(aes(x=DATE, y=NO3, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 2) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Month")+
    ggtitle("NO3")
  
  gg_PO4_Extract =
    Extract_processed_Seasonal %>%
    ggplot(aes(x=DATE, y=PO4, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 2) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Month")+
    ggtitle("PO4")
  
  gg_TRS_Extract =
    Extract_processed_Seasonal %>%
    ggplot(aes(x=DATE, y=TRS, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 2) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Month")+
    ggtitle("TRS")
  
  gg_phenolics_Extract =
    Extract_processed_Seasonal %>%
    ggplot(aes(x=DATE, y=phenolics, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 2) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Month")+
    ggtitle("phenolics")
  
  gg_TFPA_Extract =
    Extract_processed_Seasonal %>%
    ggplot(aes(x=DATE, y=TFPA, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 2) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Month")+
    ggtitle("TFPA")
  
  gg_MBC_Extract =
    Extract_processed_Seasonal %>%
    ggplot(aes(x=DATE, y=MBC, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 2) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Month")+
    ggtitle("MBC")
  
  gg_MBN_Extract =
    Extract_processed_Seasonal %>%
    ggplot(aes(x=DATE, y=MBN, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 2) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Month")+
    ggtitle("MBN")
  
  gg_MBP_Extract =
    Extract_processed_Seasonal %>%
    ggplot(aes(x=DATE, y=Mic.PO4, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 2) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Month")+
    ggtitle("MBP")
  
  list("Seasonal NH4"= gg_NH4_Extract,
       "Seasonal NO3"= gg_NO3_Extract,
       "Seasonal PO4"= gg_PO4_Extract,
       "Seasonal TFPA"= gg_TFPA_Extract,
       "Seasonal Phenolics"= gg_phenolics_Extract,
       "Seasonal TRS"= gg_TRS_Extract,
       "Seasonal MBC"= gg_MBC_Extract,
       "Seasonal MBN"= gg_MBN_Extract,
       "Seasonal MBP"= gg_MBP_Extract
  )
  
}

plot_Extract_Seasonal_H2O = function(Extract_processed_Seasonal_H2O){
  
  gg_NH4_Extract =
    Extract_processed_Seasonal_H2O %>%
    ggplot(aes(x=DATE, y=NH4.H2O, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 2) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Month")+
    ggtitle("Water extracted NH4")
  
  
  gg_NO3_Extract =
    Extract_processed_Seasonal_H2O %>%
    ggplot(aes(x=DATE, y=NO3.H2O, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 2) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Month")+
    ggtitle("Water extracted NO3")
  
  gg_PO4_Extract =
    Extract_processed_Seasonal_H2O %>%
    ggplot(aes(x=DATE, y=PO4.H2O, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 2) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Month")+
    ggtitle("Water extracted PO4")
  
  gg_TRS_Extract =
    Extract_processed_Seasonal_H2O %>%
    ggplot(aes(x=DATE, y=TRS.H2O, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 2) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Month")+
    ggtitle("Water extracted TRS")
  
  gg_TFPA_Extract =
    Extract_processed_Seasonal_H2O %>%
    ggplot(aes(x=DATE, y=TFPA.H2O, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 2) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Month")+
    ggtitle("Water extracted TFPA")
  
  
  list("Seasonal_H2O NH4"= gg_NH4_Extract,
       "Seasonal_H2O NO3"= gg_NO3_Extract,
       "Seasonal_H2O PO4"= gg_PO4_Extract,
       "Seasonal_H2O TFPA"= gg_TFPA_Extract,
       "Seasonal_H2O TRS"= gg_TRS_Extract
  )
  
}

plot_PoreWater_Snowfence = function(PoreWater_processed){

  gg_NH4_PoreWater =
    PoreWater_processed %>%
    filter(Site!="")%>%
    ggplot(aes(x=DATE, y=NH4, color=treatment))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR + Site, scales="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Date")+
    ggtitle("NH4")
  gg_NO3_PoreWater =
    PoreWater_processed %>%
    filter(Site!="")%>%
    ggplot(aes(x=DATE, y=NO3, color=treatment))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR + Site, scales="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Date")+
    ggtitle("NO3")
  gg_PO4_PoreWater =
    PoreWater_processed %>%
    filter(Site!="")%>%
    ggplot(aes(x=DATE, y=PO4, color=treatment))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR + Site, scales="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Date")+
    ggtitle("PO4")
  gg_TRS_PoreWater =
    PoreWater_processed %>%
    filter(Site!="")%>%
    ggplot(aes(x=DATE, y=TRS, color=treatment))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR + Site, scales="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Date")+
    ggtitle("TRS")
  gg_TFPA_PoreWater =
    PoreWater_processed %>%
    filter(Site!="")%>%
    ggplot(aes(x=DATE, y=TFPA, color=treatment))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR + Site, scales="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Date")+
    ggtitle("TFPA")
  
  list("Snowfence Vs. Control PoreWater_NH4"= gg_NH4_PoreWater,
       "Snowfence Vs. Control PoreWater_NO3"= gg_NO3_PoreWater,
       "Snowfence Vs. Control PoreWater_PO4"= gg_PO4_PoreWater,
       "Snowfence Vs. Control PoreWater_TFPA"= gg_TFPA_PoreWater,
       #"Snowfence Vs. Control PoreWater_Phenolics"= gg_phenolics_PoreWater,
       "Snowfence Vs. Control PoreWater_TRS"= gg_TRS_PoreWater
  )
}

plot_PoreWater_Lysim = function(PoreWater_processed_Lysim){
  
  gg_N_PoreWater =
    PoreWater_processed_Lysim %>%
    filter(Site!="")%>%
    ggplot(aes(x=DATE, y=NC, color=treatment))+
    stat_summary(geom = "line",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Date")+
    ggtitle("NH4 + NO3 + TFPA")
  
  gg_PO4_PoreWater =
    PoreWater_processed_Lysim %>%
    filter(Site!="")%>%
    ggplot(aes(x=DATE, y=PO4, color=treatment))+
    stat_summary(geom = "line",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Date")+
    ggtitle("PO4")
  
  list("Lysim_N"= gg_N_PoreWater,
       "Lysim_PO4"= gg_PO4_PoreWater
       
  )
}

plot_PoreWater_Seasonal = function(PoreWater_processed_Seasonal){
  
  gg_NH4_PoreWater =
    PoreWater_processed_Seasonal %>%
    filter(Site!="")%>%
    ggplot(aes(x=DATE, y=NH4, color=treatment))+
    stat_summary(geom = "line",size = 1) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR + Site, scales="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Date")+
    ggtitle("NH4")
  gg_NO3_PoreWater =
    PoreWater_processed_Seasonal %>%
    filter(Site!="")%>%
    ggplot(aes(x=DATE, y=NO3, color=treatment))+
    stat_summary(geom = "line",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR + Site, scales="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Date")+
    ggtitle("NO3")
  gg_PO4_PoreWater =
    PoreWater_processed_Seasonal %>%
    filter(Site!="")%>%
    ggplot(aes(x=DATE, y=PO4, color=treatment))+
    stat_summary(geom = "line",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR + Site, scales="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Date")+
    ggtitle("PO4")
  gg_TRS_PoreWater =
    PoreWater_processed_Seasonal %>%
    filter(Site!="")%>%
    ggplot(aes(x=DATE, y=TRS, color=treatment))+
    stat_summary(geom = "line",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR + Site, scales="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Date")+
    ggtitle("TRS")
  gg_TFPA_PoreWater =
    PoreWater_processed_Seasonal %>%
    filter(Site!="")%>%
    ggplot(aes(x=DATE, y=TFPA, color=treatment))+
    stat_summary(geom = "line",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR + Site, scales="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    xlab("Date")+
    ggtitle("TFPA")
  
  list("Seasonal PoreWater_NH4"= gg_NH4_PoreWater,
       "Seasonal PoreWater_NO3"= gg_NO3_PoreWater,
       "Seasonal PoreWater_PO4"= gg_PO4_PoreWater,
       "Seasonal PoreWater_TFPA"= gg_TFPA_PoreWater,
       #"Seasonal PoreWater_Phenolics"= gg_phenolics_PoreWater,
       "Seasonal PoreWater_TRS"= gg_TRS_PoreWater
  )
}






