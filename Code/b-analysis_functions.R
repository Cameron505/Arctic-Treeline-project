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
    labs(x = "Month", 
         y = bquote('Ammonium ('*mu*'g '*NH[4]^"+"~-N~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Ammonium")
  
  
  gg_NO3_Extract =
    Extract_processed %>%
    ggplot(aes(x=MONTH, y=NO3, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Month", 
         y = bquote('Nitrate ('*mu*'g '*NO[3]^"-"~-N~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Nitrate")
  
  gg_PO4_Extract =
    Extract_processed %>%
    ggplot(aes(x=MONTH, y=PO4, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Month", 
         y = bquote('Phosphate ('*mu*'g '*PO[4]^"3-"~-P~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Phosphate")
  
  gg_TRS_Extract =
    Extract_processed %>%
    ggplot(aes(x=MONTH, y=TRS, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Month", 
         y = bquote('Total reducing sugars-glucose equiv. ('*mu*'g' ~g^-1 ~ dry ~ soil*')'))+
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
    labs(x = "Month", 
         y = bquote('Phenolics-gallic acid equiv. ('*mu*'g' ~g^-1 ~ dry ~ soil*')'))+
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
    labs(x = "Month", 
         y = bquote('Total free primary amines-Leucine equiv. (nMol' ~g^-1 ~ dry ~ soil*')'))+
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
    labs(x = "Month", 
         y = bquote('Microbial biomass ('*mu*'g C'~g^-1 ~ dry ~ soil*')'))+
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
    labs(x = "Month", 
         y = bquote('Microbial biomass ('*mu*'g N'~g^-1 ~ dry ~ soil*')'))+
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
    labs(x = "Month", 
         y = bquote('Microbial biomass ('*mu*'g P'~g^-1 ~ dry ~ soil*')'))+
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
    labs(x = "Month", 
         y = bquote('Ammonium ('*mu*'g '*NH[4]^"+"~-N~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Water Extracted Ammonium")
  
  
  gg_NO3_Extract =
    Extract_Processed_H2O %>%
    ggplot(aes(x=MONTH, y=NO3.H2O, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Month", 
         y = bquote('Nitrate ('*mu*'g '*NO[3]^"-"~-N~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Water Extracted Nitrate")
  
  gg_PO4_Extract =
    Extract_Processed_H2O %>%
    ggplot(aes(x=MONTH, y=PO4.H2O, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Month", 
         y = bquote('Phosphate ('*mu*'g '*PO[4]^"3-"~-P~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Water Extracted Phosphate")
  
  gg_TRS_Extract =
    Extract_Processed_H2O %>%
    ggplot(aes(x=MONTH, y=TRS.H2O, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Month", 
         y = bquote('Total reducing sugars-glucose equiv. ('*mu*'g' ~g^-1 ~ dry ~ soil*')'))+
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
    labs(x = "Month", 
         y = bquote('Total free primary amines-Leucine equiv. (nMol' ~g^-1 ~ dry ~ soil*')'))+
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
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Ammonium ('*mu*'g '*NH[4]^"+"~-N~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Ammonium")
  
  
  gg_NO3_Extract =
    Extract_processed_Seasonal %>%
    ggplot(aes(x=DATE, y=NO3, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Nitrate ('*mu*'g '*NO[3]^"-"~-N~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Nitrate")
  
  gg_PO4_Extract =
    Extract_processed_Seasonal %>%
    ggplot(aes(x=DATE, y=PO4, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Phosphate ('*mu*'g '*PO[4]^"3-"~-P~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Phosphate")
  
  gg_TRS_Extract =
    Extract_processed_Seasonal %>%
    ggplot(aes(x=DATE, y=TRS, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Total reducing sugars-glucose equiv. ('*mu*'g' ~g^-1 ~ dry ~ soil*')'))+
    ggtitle("TRS")
  
  gg_phenolics_Extract =
    Extract_processed_Seasonal %>%
    ggplot(aes(x=DATE, y=phenolics, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Phenolics-gallic acid equiv. ('*mu*'g' ~g^-1 ~ dry ~ soil*')'))+
    ggtitle("phenolics")
  
  gg_TFPA_Extract =
    Extract_processed_Seasonal %>%
    ggplot(aes(x=DATE, y=TFPA, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Total free primary amines-Leucine equiv. (nMol' ~g^-1 ~ dry ~ soil*')'))+
    ggtitle("TFPA")
  
  gg_MBC_Extract =
    Extract_processed_Seasonal %>%
    ggplot(aes(x=DATE, y=MBC, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Microbial biomass ('*mu*'g C'~g^-1 ~ dry ~ soil*')'))+
    ggtitle("MBC")
  
  gg_MBN_Extract =
    Extract_processed_Seasonal %>%
    ggplot(aes(x=DATE, y=MBN, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Microbial biomass ('*mu*'g N'~g^-1 ~ dry ~ soil*')'))+
    ggtitle("MBN")
  
  gg_MBP_Extract =
    Extract_processed_Seasonal %>%
    ggplot(aes(x=DATE, y=Mic.PO4, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Microbial biomass ('*mu*'g P'~g^-1 ~ dry ~ soil*')'))+
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
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Ammonium ('*mu*'g '*NH[4]^"+"~-N~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Water Extracted Ammonium")
  
  
  gg_NO3_Extract =
    Extract_processed_Seasonal_H2O %>%
    ggplot(aes(x=DATE, y=NO3.H2O, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Nitrate ('*mu*'g '*NO[3]^"-"~-N~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Water Extracted Nitrate")
  
  gg_PO4_Extract =
    Extract_processed_Seasonal_H2O %>%
    ggplot(aes(x=DATE, y=PO4.H2O, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Phosphate ('*mu*'g '*PO[4]^"3-"~-P~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Water Extracted Phosphate")
  
  gg_TRS_Extract =
    Extract_processed_Seasonal_H2O %>%
    ggplot(aes(x=DATE, y=TRS.H2O, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Total reducing sugars-glucose equiv. ('*mu*'g' ~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Water extracted TRS")
  
  gg_TFPA_Extract =
    Extract_processed_Seasonal_H2O %>%
    ggplot(aes(x=DATE, y=TFPA.H2O, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Total free primary amines-Leucine equiv. (nMol' ~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Water extracted TFPA")
  
  
  list("Seasonal_H2O NH4"= gg_NH4_Extract,
       "Seasonal_H2O NO3"= gg_NO3_Extract,
       "Seasonal_H2O PO4"= gg_PO4_Extract,
       "Seasonal_H2O TFPA"= gg_TFPA_Extract,
       "Seasonal_H2O TRS"= gg_TRS_Extract
  )
  
}

plot_Extract_all = function(Extract_processed_all){
  
  gg_NH4_Extract =
    Extract_processed_all %>%
    ggplot(aes(x=DATE, y=NH4, color=Site, shape=treatment))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Ammonium ('*mu*'g '*NH[4]^"+"~-N~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Ammonium")
  
  
  gg_NO3_Extract =
    Extract_processed_all %>%
    ggplot(aes(x=DATE, y=NO3, color=Site, shape=treatment))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Nitrate ('*mu*'g '*NO[3]^"-"~-N~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Nitrate")
  
  gg_PO4_Extract =
    Extract_processed_all %>%
    ggplot(aes(x=DATE, y=PO4, color=Site, shape=treatment))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Phosphate ('*mu*'g '*PO[4]^"3-"~-P~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Phosphate")
  
  gg_TRS_Extract =
    Extract_processed_all %>%
    ggplot(aes(x=DATE, y=TRS, color=Site, shape=treatment))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Total reducing sugars-glucose equiv. ('*mu*'g' ~g^-1 ~ dry ~ soil*')'))+
    ggtitle("TRS")
  
  gg_phenolics_Extract =
    Extract_processed_all %>%
    ggplot(aes(x=DATE, y=phenolics, color=Site, shape=treatment))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Phenolics-gallic acid equiv. ('*mu*'g' ~g^-1 ~ dry ~ soil*')'))+
    ggtitle("phenolics")
  
  gg_TFPA_Extract =
    Extract_processed_all %>%
    ggplot(aes(x=DATE, y=TFPA, color=Site, shape=treatment))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Total free primary amines-Leucine equiv. (nMol' ~g^-1 ~ dry ~ soil*')'))+
    ggtitle("TFPA")
  
  gg_MBC_Extract =
    Extract_processed_all %>%
    ggplot(aes(x=DATE, y=MBC, color=Site, shape=treatment))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Microbial biomass ('*mu*'g C'~g^-1 ~ dry ~ soil*')'))+
    ggtitle("MBC")
  
  gg_MBN_Extract =
    Extract_processed_all %>%
    ggplot(aes(x=DATE, y=MBN, color=Site, shape=treatment))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Microbial biomass ('*mu*'g N'~g^-1 ~ dry ~ soil*')'))+
    ggtitle("MBN")
  
  gg_MBP_Extract =
    Extract_processed_all %>%
    ggplot(aes(x=DATE, y=Mic.PO4, color=Site, shape=treatment))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Microbial biomass ('*mu*'g P'~g^-1 ~ dry ~ soil*')'))+
    ggtitle("MBP")
  
  list("all NH4"= gg_NH4_Extract,
       "all NO3"= gg_NO3_Extract,
       "all PO4"= gg_PO4_Extract,
       "all TFPA"= gg_TFPA_Extract,
       "all Phenolics"= gg_phenolics_Extract,
       "all TRS"= gg_TRS_Extract,
       "all MBC"= gg_MBC_Extract,
       "all MBN"= gg_MBN_Extract,
       "all MBP"= gg_MBP_Extract
  )
  
}

plot_Extract_all_H2O = function(Extract_processed_all_H2O){
  
  gg_NH4_Extract =
    Extract_processed_all_H2O %>%
    ggplot(aes(x=DATE, y=NH4.H2O, color=Site, shape=treatment))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Ammonium ('*mu*'g '*NH[4]^"+"~-N~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Water Extracted Ammonium")
  
  
  gg_NO3_Extract =
    Extract_processed_all_H2O %>%
    ggplot(aes(x=DATE, y=NO3.H2O, color=Site, shape=treatment))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Nitrate ('*mu*'g '*NO[3]^"-"~-N~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Water Extracted Nitrate")
  
  gg_PO4_Extract =
    Extract_processed_all_H2O %>%
    ggplot(aes(x=DATE, y=PO4.H2O, color=Site, shape=treatment))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Phosphate ('*mu*'g '*PO[4]^"3-"~-P~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Water Extracted Phosphate")
  
  gg_TRS_Extract =
    Extract_processed_all_H2O %>%
    ggplot(aes(x=DATE, y=TRS.H2O, color=Site, shape=treatment))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Total reducing sugars-glucose equiv. ('*mu*'g' ~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Water extracted TRS")
  
  gg_TFPA_Extract =
    Extract_processed_all_H2O %>%
    ggplot(aes(x=DATE, y=TFPA.H2O, color=Site, shape=treatment))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Total free primary amines-Leucine equiv. (nMol' ~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Water extracted TFPA")
  
  
  list("all_H2O NH4"= gg_NH4_Extract,
       "all_H2O NO3"= gg_NO3_Extract,
       "all_H2O PO4"= gg_PO4_Extract,
       "all_H2O TFPA"= gg_TFPA_Extract,
       "all_H2O TRS"= gg_TRS_Extract
  )
  
}

plot_Extract_Fert_H2O = function(Extract_data){
  
  gg_NH4_Extract =
    Extract_data %>%
    mutate_at(c('NH4.H2O', 'NO3.H2O','PO4.H2O','TFPA.H2O','TRS.H2O'), as.numeric) %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary","fertilized"))%>%
    mutate(treatment=factor(treatment, levels=c("Ancillary","Control","Snowfence","fertilized")),DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    ggplot(aes(x=DATE, y=NH4.H2O, color=treatment))+
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR+Site, scales="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Ammonium ('*mu*'g '*NH[4]^"+"~-N~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Water Extracted Ammonium")
  
  
  gg_NO3_Extract =
    Extract_data %>%
    mutate_at(c('NH4.H2O', 'NO3.H2O','PO4.H2O','TFPA.H2O','TRS.H2O'), as.numeric) %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary","fertilized"))%>%
    mutate(treatment=factor(treatment, levels=c("Ancillary","Control","Snowfence","fertilized")),DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    ggplot(aes(x=DATE, y=NO3.H2O, color=treatment))+
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR+Site, scales="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Nitrate ('*mu*'g '*NO[3]^"-"~-N~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Water Extracted Nitrate")
  
  gg_PO4_Extract =
    Extract_data %>%
    mutate_at(c('NH4.H2O', 'NO3.H2O','PO4.H2O','TFPA.H2O','TRS.H2O'), as.numeric) %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary","fertilized"))%>%
    mutate(treatment=factor(treatment, levels=c("Ancillary","Control","Snowfence","fertilized")),DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    ggplot(aes(x=DATE, y=PO4.H2O, color=treatment))+
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR+Site, scales="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Phosphate ('*mu*'g '*PO[4]^"3-"~-P~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Water Extracted Phosphate")
  
  gg_TRS_Extract =
    Extract_data %>%
    mutate_at(c('NH4.H2O', 'NO3.H2O','PO4.H2O','TFPA.H2O','TRS.H2O'), as.numeric) %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary","fertilized"))%>%
    mutate(treatment=factor(treatment, levels=c("Ancillary","Control","Snowfence","fertilized")),DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    ggplot(aes(x=DATE, y=TRS.H2O, color=treatment))+
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR+Site, scales="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Total reducing sugars-glucose equiv. ('*mu*'g' ~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Water Extracted TRS")
  
  
  gg_TFPA_Extract =
    Extract_data %>%
    mutate_at(c('NH4.H2O', 'NO3.H2O','PO4.H2O','TFPA.H2O','TRS.H2O'), as.numeric) %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary","fertilized"))%>%
    mutate(treatment=factor(treatment, levels=c("Ancillary","Control","Snowfence","fertilized")),DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    ggplot(aes(x=DATE, y=TFPA.H2O, color=treatment))+
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR+Site, scales="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Total free primary amines-Leucine equiv. (nMol' ~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Water Extracted TFPA")
  
  
  
  list("Fert H2O NH4"= gg_NH4_Extract,
       "Fert H2O NO3"= gg_NO3_Extract,
       "Fert H2O PO4"= gg_PO4_Extract,
       "Fert H2O TFPA"= gg_TFPA_Extract,
       "Fert H2O TRS"= gg_TRS_Extract
  )
  
}

plot_Extract_lysim = function(Extract_processed_lysim){
  
  gg_NH4_Extract =
    Extract_processed_lysim %>%
    ggplot(aes(x=DATE, y=NH4, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Ammonium ('*mu*'g '*NH[4]^"+"~-N~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Ammonium")
  
  
  gg_NO3_Extract =
    Extract_processed_lysim %>%
    ggplot(aes(x=DATE, y=NO3, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Nitrate ('*mu*'g '*NO[3]^"-"~-N~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Nitrate")
  
  gg_PO4_Extract =
    Extract_processed_lysim %>%
    ggplot(aes(x=DATE, y=PO4, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Phosphate ('*mu*'g '*PO[4]^"3-"~-P~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Phosphate")
  
  gg_TRS_Extract =
    Extract_processed_lysim %>%
    ggplot(aes(x=DATE, y=TRS, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Total reducing sugars-glucose equiv. ('*mu*'g' ~g^-1 ~ dry ~ soil*')'))+
    ggtitle("TRS")
  
  gg_phenolics_Extract =
    Extract_processed_lysim %>%
    ggplot(aes(x=DATE, y=phenolics, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Phenolics-gallic acid equiv. ('*mu*'g' ~g^-1 ~ dry ~ soil*')'))+
    ggtitle("phenolics")
  
  gg_TFPA_Extract =
    Extract_processed_lysim %>%
    ggplot(aes(x=DATE, y=TFPA, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Total free primary amines-Leucine equiv. (nMol' ~g^-1 ~ dry ~ soil*')'))+
    ggtitle("TFPA")
  
  gg_MBC_Extract =
    Extract_processed_lysim %>%
    ggplot(aes(x=DATE, y=MBC, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Microbial biomass ('*mu*'g C'~g^-1 ~ dry ~ soil*')'))+
    ggtitle("MBC")
  
  gg_MBN_Extract =
    Extract_processed_lysim %>%
    ggplot(aes(x=DATE, y=MBN, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Microbial biomass ('*mu*'g N'~g^-1 ~ dry ~ soil*')'))+
    ggtitle("MBN")
  
  gg_MBP_Extract =
    Extract_processed_lysim %>%
    ggplot(aes(x=DATE, y=Mic.PO4, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Microbial biomass ('*mu*'g P'~g^-1 ~ dry ~ soil*')'))+
    ggtitle("MBP")
  
  
  gg_NH4_Extract_H2O =
    Extract_processed_lysim %>%
    ggplot(aes(x=DATE, y=NH4.H2O, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Ammonium ('*mu*'g '*NH[4]^"+"~-N~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Water Extracted Ammonium")
  
  
  gg_NO3_Extract_H2O =
    Extract_processed_lysim %>%
    ggplot(aes(x=DATE, y=NO3.H2O, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Nitrate ('*mu*'g '*NO[3]^"-"~-N~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Water Extracted Nitrate")
  
  gg_PO4_Extract_H2O =
    Extract_processed_lysim %>%
    ggplot(aes(x=DATE, y=PO4.H2O, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Phosphate ('*mu*'g '*PO[4]^"3-"~-P~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Water Extracted Phosphate")
  
  gg_TRS_Extract_H2O =
    Extract_processed_lysim %>%
    ggplot(aes(x=DATE, y=TRS.H2O, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Total reducing sugars-glucose equiv. ('*mu*'g' ~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Water extracted TRS")
  
  gg_TFPA_Extract_H2O =
    Extract_processed_lysim %>%
    ggplot(aes(x=DATE, y=TFPA.H2O, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Total free primary amines-Leucine equiv. (nMol' ~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Water extracted TFPA")
  
  
  
  list("lysim NH4"= gg_NH4_Extract,
       "lysim NO3"= gg_NO3_Extract,
       "lysim PO4"= gg_PO4_Extract,
       "lysim TFPA"= gg_TFPA_Extract,
       "lysim Phenolics"= gg_phenolics_Extract,
       "lysim TRS"= gg_TRS_Extract,
       "lysim MBC"= gg_MBC_Extract,
       "lysim MBN"= gg_MBN_Extract,
       "lysim MBP"= gg_MBP_Extract,
       "lysim_H2O NH4"= gg_NH4_Extract_H2O,
       "lysim_H2O NO3"= gg_NO3_Extract_H2O,
       "lysim_H2O PO4"= gg_PO4_Extract_H2O,
       "lysim_H2O TFPA"= gg_TFPA_Extract_H2O,
       "lysim_H2O TRS"= gg_TRS_Extract_H2O
  )
  
}

plot_Extract_Fert = function(Extract_data){
  
  gg_NH4_Extract =
    Extract_data %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary","fertilized"))%>%
    mutate(treatment=factor(treatment, levels=c("Ancillary","Control","Snowfence","fertilized")),
           DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','phenolics','TRS','MBC','MBN','Mic.PO4'), as.numeric) %>%
    ggplot(aes(x=DATE, y=NH4, color=treatment))+
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR+Site, scales="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Ammonium ('*mu*'g '*NH[4]^"+"~-N~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Ammonium")
  
  
  gg_NO3_Extract =
    Extract_data %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary","fertilized"))%>%
    mutate(treatment=factor(treatment, levels=c("Ancillary","Control","Snowfence","fertilized")),DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','phenolics','TRS','MBC','MBN','Mic.PO4'), as.numeric) %>%
    ggplot(aes(x=DATE, y=NO3, color=treatment))+
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR+Site, scales="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Nitrate ('*mu*'g '*NO[3]^"-"~-N~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Nitrate")
  
  gg_PO4_Extract =
    Extract_data %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary","fertilized"))%>%
    mutate(treatment=factor(treatment, levels=c("Ancillary","Control","Snowfence","fertilized")),DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','phenolics','TRS','MBC','MBN','Mic.PO4'), as.numeric) %>%
    ggplot(aes(x=DATE, y=PO4, color=treatment))+
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR+Site, scales="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Phosphate ('*mu*'g '*PO[4]^"3-"~-P~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Phosphate")
  
  gg_TRS_Extract =
    Extract_data %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary","fertilized"))%>%
    mutate(treatment=factor(treatment, levels=c("Ancillary","Control","Snowfence","fertilized")),DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','phenolics','TRS','MBC','MBN','Mic.PO4'), as.numeric) %>%
    ggplot(aes(x=DATE, y=TRS, color=treatment))+
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR+Site, scales="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Total reducing sugars-glucose equiv. ('*mu*'g' ~g^-1 ~ dry ~ soil*')'))+
    ggtitle("TRS")
  
  gg_phenolics_Extract =
    Extract_data %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary","fertilized"))%>%
    mutate(treatment=factor(treatment, levels=c("Ancillary","Control","Snowfence","fertilized")),DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','phenolics','TRS','MBC','MBN','Mic.PO4'), as.numeric) %>%
    ggplot(aes(x=DATE, y=phenolics, color=treatment))+
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR+Site, scales="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Phenolics-gallic acid equiv. ('*mu*'g' ~g^-1 ~ dry ~ soil*')'))+
    ggtitle("phenolics")
  
  gg_TFPA_Extract =
    Extract_data %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary","fertilized"))%>%
    mutate(treatment=factor(treatment, levels=c("Ancillary","Control","Snowfence","fertilized")),DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','phenolics','TRS','MBC','MBN','Mic.PO4'), as.numeric) %>%
    ggplot(aes(x=DATE, y=TFPA, color=treatment))+
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR+Site, scales="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Total free primary amines-Leucine equiv. (nMol' ~g^-1 ~ dry ~ soil*')'))+
    ggtitle("TFPA")
  
  gg_MBC_Extract =
    Extract_data %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary","fertilized"))%>%
    mutate(treatment=factor(treatment, levels=c("Ancillary","Control","Snowfence","fertilized")),DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','phenolics','TRS','MBC','MBN','Mic.PO4'), as.numeric) %>%
    ggplot(aes(x=DATE, y=MBC, color=treatment))+
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR+Site, scales="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Microbial biomass ('*mu*'g C'~g^-1 ~ dry ~ soil*')'))+
    ggtitle("MBC")
  
  gg_MBN_Extract =
    Extract_data %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary","fertilized"))%>%
    mutate(treatment=factor(treatment, levels=c("Ancillary","Control","Snowfence","fertilized")),DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','phenolics','TRS','MBC','MBN','Mic.PO4'), as.numeric) %>%
    ggplot(aes(x=DATE, y=MBN, color=treatment))+
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR+Site, scales="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Microbial biomass ('*mu*'g N'~g^-1 ~ dry ~ soil*')'))+
    ggtitle("MBN")
  
  gg_MBP_Extract =
    Extract_data %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary","fertilized"))%>%
    mutate(treatment=factor(treatment, levels=c("Ancillary","Control","Snowfence","fertilized")),DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','phenolics','TRS','MBC','MBN','Mic.PO4'), as.numeric) %>%
    ggplot(aes(x=DATE, y=Mic.PO4, color=treatment))+
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR+Site, scales="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Microbial biomass ('*mu*'g P'~g^-1 ~ dry ~ soil*')'))+
    ggtitle("MBP")
  
  list("Fert NH4"= gg_NH4_Extract,
       "Fert NO3"= gg_NO3_Extract,
       "Fert PO4"= gg_PO4_Extract,
       "Fert TFPA"= gg_TFPA_Extract,
       "Fert Phenolics"= gg_phenolics_Extract,
       "Fert TRS"= gg_TRS_Extract,
       "Fert MBC"= gg_MBC_Extract,
       "Fert MBN"= gg_MBN_Extract,
       "Fert MBP"= gg_MBP_Extract
  )
  
}

plot_PoreWater_Fert = function(PoreWater_data){
  
  gg_NH4_PoreWater =
    PoreWater_data %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','TRS'), as.numeric) %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary","fertilized"))%>%
    mutate(treatment=factor(treatment, levels=c("Ancillary","Control","Snowfence","fertilized")),DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    ggplot(aes(x=DATE, y=NH4, color=treatment))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR + Site, scales="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Ammonium ('*mu*'g '*NH[4]^"+"~-N~mL^-1 ~ Soil ~ Solution*')'))+
    ggtitle("Ammonium")
  gg_NO3_PoreWater =
    PoreWater_data %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','TRS'), as.numeric) %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary","fertilized"))%>%
    mutate(treatment=factor(treatment, levels=c("Ancillary","Control","Snowfence","fertilized")),DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    ggplot(aes(x=DATE, y=NO3, color=treatment))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR + Site, scales="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Nitrate ('*mu*'g '*NO[3]^"-"~-N~mL^-1 ~ Soil ~ Solution*')'))+
    ggtitle("Nitrate")
  gg_PO4_PoreWater =
    PoreWater_data %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','TRS'), as.numeric) %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary","fertilized"))%>%
    mutate(treatment=factor(treatment, levels=c("Ancillary","Control","Snowfence","fertilized")),DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    ggplot(aes(x=DATE, y=PO4, color=treatment))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR + Site, scales="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Phosphate ('*mu*'g '*PO[4]^"3-"~-P~mL^-1 ~ Soil ~ Solution*')'))+
    ggtitle("Phosphate")
  gg_TRS_PoreWater =
    PoreWater_data %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','TRS'), as.numeric) %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary","fertilized"))%>%
    mutate(treatment=factor(treatment, levels=c("Ancillary","Control","Snowfence","fertilized")),DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    ggplot(aes(x=DATE, y=TRS, color=treatment))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR + Site, scales="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Total reducing sugars-glucose equiv. ('*mu*'g' ~mL^-1 ~ Soil ~ Solution*')'))+
    ggtitle("TRS")
  gg_TFPA_PoreWater =
    PoreWater_data %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','TRS'), as.numeric) %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary","fertilized"))%>%
    mutate(treatment=factor(treatment, levels=c("Ancillary","Control","Snowfence","fertilized")),DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    ggplot(aes(x=DATE, y=TFPA, color=treatment))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR + Site, scales="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Total free primary amines-Leucine equiv. (nMol' ~mL^-1 ~ Soil ~ Solution*')'))+
    ggtitle("TFPA")
  
  list("Fert PoreWater_NH4"= gg_NH4_PoreWater,
       "Fert PoreWater_NO3"= gg_NO3_PoreWater,
       "Fert PoreWater_PO4"= gg_PO4_PoreWater,
       "Fert PoreWater_TFPA"= gg_TFPA_PoreWater,
       #"Fert PoreWater_Phenolics"= gg_phenolics_PoreWater,
       "Fert PoreWater_TRS"= gg_TRS_PoreWater
  )
}

plot_PoreWater_Snowfence = function(PoreWater_processed){

  gg_NH4_PoreWater =
    PoreWater_processed %>%
    filter(Site!="")%>%
    ggplot(aes(x=DATE, y=NH4, color=treatment))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR + Site, scales="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Ammonium ('*mu*'g '*NH[4]^"+"~-N~mL^-1 ~ Soil ~ Solution*')'))+
    ggtitle("Ammonium")
  gg_NO3_PoreWater =
    PoreWater_processed %>%
    filter(Site!="")%>%
    ggplot(aes(x=DATE, y=NO3, color=treatment))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR + Site, scales="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Nitrate ('*mu*'g '*NO[3]^"-"~-N~mL^-1 ~ Soil ~ Solution*')'))+
    ggtitle("Nitrate")
  gg_PO4_PoreWater =
    PoreWater_processed %>%
    filter(Site!="")%>%
    ggplot(aes(x=DATE, y=PO4, color=treatment))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR + Site, scales="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Phosphate ('*mu*'g '*PO[4]^"3-"~-P~mL^-1 ~ Soil ~ Solution*')'))+
    ggtitle("Phosphate")
  gg_TRS_PoreWater =
    PoreWater_processed %>%
    filter(Site!="")%>%
    ggplot(aes(x=DATE, y=TRS, color=treatment))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR + Site, scales="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Total reducing sugars-glucose equiv. ('*mu*'g' ~mL^-1 ~ Soil ~ Solution*')'))+
    ggtitle("TRS")
  gg_TFPA_PoreWater =
    PoreWater_processed %>%
    filter(Site!="")%>%
    ggplot(aes(x=DATE, y=TFPA, color=treatment))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR + Site, scales="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Total free primary amines-Leucine equiv. (nMol' ~mL^-1 ~ Soil ~ Solution*')'))+
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
    ylab(expression(paste( "Ammonium + Nitrate + TFPA (",mu,"M Labile N)")))+
    xlab("Date")+
    ggtitle("Ammonium + Nitrate + TFPA")
  
  gg_PO4_PoreWater =
    PoreWater_processed_Lysim %>%
    filter(Site!="")%>%
    ggplot(aes(x=DATE, y=PO4_2, color=treatment))+
    stat_summary(geom = "line",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Phosphate ('*mu*'M '*PO[4]^"3-"~-P*')'))+
    xlab("Date")+
    ggtitle("Phosphate")
  
  list("Lysim_N"= gg_N_PoreWater,
       "Lysim_PO4"= gg_PO4_PoreWater
       
  )
}

plot_PoreWater_Seasonal = function(PoreWater_processed_Seasonal){
  
  gg_NH4_PoreWater =
    PoreWater_processed_Seasonal %>%
    filter(Site!="")%>%
    ggplot(aes(x=DATE, y=NH4, color=treatment))+
    stat_summary(geom = "point",size = 1) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR + Site, scales="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Ammonium ('*mu*'g '*NH[4]^"+"~-N~mL^-1 ~ Soil ~ Solution*')'))+
    ggtitle("Ammonium")
  gg_NO3_PoreWater =
    PoreWater_processed_Seasonal %>%
    filter(Site!="")%>%
    ggplot(aes(x=DATE, y=NO3, color=treatment))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR + Site, scales="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Nitrate ('*mu*'g '*NO[3]^"-"~-N~mL^-1 ~ Soil ~ Solution*')'))+
    ggtitle("Nitrate")
  gg_PO4_PoreWater =
    PoreWater_processed_Seasonal %>%
    filter(Site!="")%>%
    ggplot(aes(x=DATE, y=PO4, color=treatment))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR + Site, scales="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Phosphate ('*mu*'g '*PO[4]^"3-"~-P~mL^-1 ~ Soil ~ Solution*')'))+
    ggtitle("Phosphate")
  gg_TRS_PoreWater =
    PoreWater_processed_Seasonal %>%
    filter(Site!="")%>%
    ggplot(aes(x=DATE, y=TRS, color=treatment))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR + Site, scales="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Total reducing sugars-glucose equiv. ('*mu*'g' ~mL^-1 ~ Soil ~ Solution*')'))+
    ggtitle("TRS")
  gg_TFPA_PoreWater =
    PoreWater_processed_Seasonal %>%
    filter(Site!="")%>%
    ggplot(aes(x=DATE, y=TFPA, color=treatment))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR + Site, scales="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Total free primary amines-Leucine equiv. (nMol' ~mL^-1 ~ Soil ~ Solution*')'))+
    ggtitle("TFPA")
  
  list("Seasonal PoreWater_NH4"= gg_NH4_PoreWater,
       "Seasonal PoreWater_NO3"= gg_NO3_PoreWater,
       "Seasonal PoreWater_PO4"= gg_PO4_PoreWater,
       "Seasonal PoreWater_TFPA"= gg_TFPA_PoreWater,
       "Seasonal PoreWater_TRS"= gg_TRS_PoreWater
  )
}

plot_PoreWater_all = function(PoreWater_processed_all){
  
  gg_NH4_PoreWater =
    PoreWater_processed_all %>%
    filter(Site!="")%>%
    ggplot(aes(x=DATE, y=NH4, color=treatment))+
    stat_summary(geom = "point",size = 1) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR + Site, scales="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Ammonium ('*mu*'g '*NH[4]^"+"~-N~mL^-1 ~ Soil ~ Solution*')'))+
    ggtitle("Ammonium")
  gg_NO3_PoreWater =
    PoreWater_processed_all %>%
    filter(Site!="")%>%
    ggplot(aes(x=DATE, y=NO3, color=treatment))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR + Site, scales="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Nitrate ('*mu*'g '*NO[3]^"-"~-N~mL^-1 ~ Soil ~ Solution*')'))+
    ggtitle("Nitrate")
  gg_PO4_PoreWater =
    PoreWater_processed_all %>%
    filter(Site!="")%>%
    ggplot(aes(x=DATE, y=PO4, color=treatment))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR + Site, scales="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Phosphate ('*mu*'g '*PO[4]^"3-"~-P~mL^-1 ~ Soil ~ Solution*')'))+
    ggtitle("Phosphate")
  gg_TRS_PoreWater =
    PoreWater_processed_all %>%
    filter(Site!="")%>%
    ggplot(aes(x=DATE, y=TRS, color=treatment))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR + Site, scales="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Total reducing sugars-glucose equiv. ('*mu*'g' ~mL^-1 ~ Soil ~ Solution*')'))+
    ggtitle("TRS")
  gg_TFPA_PoreWater =
    PoreWater_processed_all %>%
    filter(Site!="")%>%
    ggplot(aes(x=DATE, y=TFPA, color=treatment))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR + Site, scales="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Total free primary amines-Leucine equiv. (nMol' ~mL^-1 ~ Soil ~ Solution*')'))+
    ggtitle("TFPA")
  
  list("all PoreWater_NH4"= gg_NH4_PoreWater,
       "all PoreWater_NO3"= gg_NO3_PoreWater,
       "all PoreWater_PO4"= gg_PO4_PoreWater,
       "all PoreWater_TFPA"= gg_TFPA_PoreWater,
       #"all PoreWater_Phenolics"= gg_phenolics_PoreWater,
       "all PoreWater_TRS"= gg_TRS_PoreWater
  )
}



plot_resin_Snowfence = function(Resin_processed){
  

  
  gg_NH4_Extract2 =
    Resin_processed %>%
    mutate(Purpose2=factor(Purpose2, levels= c("OW-GS","OW","GS")))%>%
    filter(Treatment %in% c("Snowfence","Control"))%>%
    ggplot(aes(x=Purpose2, y=Ammonium, fill=Site,alpha=Treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position= 'dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= 'dodge')+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_alpha_manual(values=c(0.75,1))+
    labs(x = "Season", 
         y = bquote('Ammonium ('*mu*'g '*NH[4]^"+"~-N~cm^-2 ~ Day^-1*')'))+
    ggtitle("Ammonium")
  
  
  gg_NO3_Extract2 =
    Resin_processed %>%
    mutate(Purpose2=factor(Purpose2, levels= c("OW-GS","OW","GS")))%>%
    filter(Treatment %in% c("Snowfence","Control"))%>%
    ggplot(aes(x=Purpose2, y=Nitrate, fill=Site,alpha=Treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position= 'dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= 'dodge')+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_alpha_manual(values=c(0.75,1))+
    labs(x = "Season", 
         y = bquote('Nitrate ('*mu*'g '*NO[3]^"-"~-N~cm^-2 ~ Day^-1*')'))+
    ggtitle("Nitrate")
  
  gg_PO4_Extract2 =
    Resin_processed %>%
    mutate(Purpose2=factor(Purpose2, levels= c("OW-GS","OW","GS")))%>%
    filter(Treatment %in% c("Snowfence","Control"))%>%
    ggplot(aes(x=Purpose2, y=Phosphate, fill=Site,alpha=Treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position= 'dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= 'dodge')+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_alpha_manual(values=c(0.75,1))+
    labs(x = "Season", 
         y = bquote('Phosphate ('*mu*'g '*PO[4]^"3-"~-P~cm^-2 ~ Day^-1*')'))+
    ggtitle("Phosphate")
  
  
  list("resin NH4-Treatment"= gg_NH4_Extract2,
       "resin NO3-Treatment"= gg_NO3_Extract2,
       "resin PO4-Treatment"= gg_PO4_Extract2
  )
  
}

plot_resin_Ancillary = function(Resin_processed){
  
  
  
  gg_NH4_Extract2 =
    Resin_processed %>%
    mutate(Purpose2=factor(Purpose2, levels= c("OW-GS","OW","GS")))%>%
    filter(Treatment %in% c("Ancillary"))%>%
    ggplot(aes(x=Purpose2, y=Ammonium, fill=Site))+
    stat_summary(fun="mean",geom = "bar",size = 2, position= 'dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= 'dodge')+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Season", 
         y = bquote('Ammonium ('*mu*'g '*NH[4]^"+"~-N~cm^-2 ~ Day^-1*')'))+
    ggtitle("Ammonium")
  
  
  gg_NO3_Extract2 =
    Resin_processed %>%
    mutate(Purpose2=factor(Purpose2, levels= c("OW-GS","OW","GS")))%>%
    filter(Treatment %in% c("Ancillary"))%>%
    ggplot(aes(x=Purpose2, y=Nitrate, fill=Site))+
    stat_summary(fun="mean",geom = "bar",size = 2, position= 'dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= 'dodge')+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Season", 
         y = bquote('Nitrate ('*mu*'g '*NO[3]^"-"~-N~cm^-2 ~ Day^-1*')'))+
    ggtitle("Nitrate")
  
  gg_PO4_Extract2 =
    Resin_processed %>%
    mutate(Purpose2=factor(Purpose2, levels= c("OW-GS","OW","GS")))%>%
    filter(Treatment %in% c("Ancillary"))%>%
    ggplot(aes(x=Purpose2, y=Phosphate, fill=Site))+
    stat_summary(fun="mean",geom = "bar",size = 2, position= 'dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= 'dodge')+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Season", 
         y = bquote('Phosphate ('*mu*'g '*PO[4]^"3-"~-P~cm^-2 ~ Day^-1*')'))+
    ggtitle("Phosphate")
  
  
  list("resin NH4-Treatment"= gg_NH4_Extract2,
       "resin NO3-Treatment"= gg_NO3_Extract2,
       "resin PO4-Treatment"= gg_PO4_Extract2
  )
  
}

plot_resin = function(Resin_processed){
  
  gg_NH4_Extract =
    Resin_processed %>%
    mutate(Purpose2=factor(Purpose2, levels= c("OW-GS","OW","GS")))%>%
    ggplot(aes(x=Purpose2, y=Ammonium, fill=Site))+
    stat_summary(fun="mean",geom = "bar",size = 2, position= 'dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= 'dodge')+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Season", 
         y = bquote('Ammonium ('*mu*'g '*NH[4]^"+"~-N~cm^-2 ~ Day^-1*')'))+
    ggtitle("Ammonium-By site")
  
  
  gg_NO3_Extract =
    Resin_processed %>%
    mutate(Purpose2=factor(Purpose2, levels= c("OW-GS","OW","GS")))%>%
    ggplot(aes(x=Purpose2, y=Nitrate, fill=Site))+
    stat_summary(fun="mean",geom = "bar",size = 2, position= 'dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= 'dodge')+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Season", 
         y = bquote('Nitrate ('*mu*'g '*NO[3]^"-"~-N~cm^-2 ~ Day^-1*')'))+
    ggtitle("Nitrate-By site")
  
  gg_PO4_Extract =
    Resin_processed %>%
    mutate(Purpose2=factor(Purpose2, levels= c("OW-GS","OW","GS")))%>%
    ggplot(aes(x=Purpose2, y=Phosphate, fill=Site))+
    stat_summary(fun="mean",geom = "bar",size = 2, position= 'dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= 'dodge')+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Season", 
         y = bquote('Phosphate ('*mu*'g '*PO[4]^"3-"~-P~cm^-2 ~ Day^-1*')'))+
    ggtitle("Phosphate-By site")
  
  gg_NH4_Extract2 =
    Resin_processed %>%
    mutate(Purpose2=factor(Purpose2, levels= c("OW-GS","OW","GS")))%>%
    filter(Treatment %in% c("Snowfence","Control","Ancillary"))%>%
    ggplot(aes(x=Purpose2, y=Ammonium, fill=Site,alpha=Treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position= 'dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= 'dodge')+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_alpha_manual(values=c(0.5,0.75,1))+
    labs(x = "Season", 
         y = bquote('Ammonium ('*mu*'g '*NH[4]^"+"~-N~cm^-2 ~ Day^-1*')'))+
    ggtitle("Ammonium- By Site and Treatment")
  
  
  gg_NO3_Extract2 =
    Resin_processed %>%
    mutate(Purpose2=factor(Purpose2, levels= c("OW-GS","OW","GS")))%>%
    filter(Treatment %in% c("Snowfence","Control","Ancillary"))%>%
    ggplot(aes(x=Purpose2, y=Nitrate, fill=Site,alpha=Treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position= 'dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= 'dodge')+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_alpha_manual(values=c(0.5,0.75,1))+
    labs(x = "Season", 
         y = bquote('Nitrate ('*mu*'g '*NO[3]^"-"~-N~cm^-2 ~ Day^-1*')'))+
    ggtitle("Nitrate- By Site and Treatment")
  
  gg_PO4_Extract2 =
    Resin_processed %>%
    mutate(Purpose2=factor(Purpose2, levels= c("OW-GS","OW","GS")))%>%
    filter(Treatment %in% c("Snowfence","Control","Ancillary"))%>%
    ggplot(aes(x=Purpose2, y=Phosphate, fill=Site,alpha=Treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position= 'dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= 'dodge')+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_alpha_manual(values=c(0.5,0.75,1))+
    labs(x = "Season", 
         y = bquote('Phosphate ('*mu*'g '*PO[4]^"3-"~-P~cm^-2 ~ Day^-1*')'))+
    ggtitle("Phosphate- By Site and Treatment")
  
  
  list("resin NH4"= gg_NH4_Extract,
       "resin NO3"= gg_NO3_Extract,
       "resin PO4"= gg_PO4_Extract,
       "resin NH4-Treatment"= gg_NH4_Extract2,
       "resin NO3-Treatment"= gg_NO3_Extract2,
       "resin PO4-Treatment"= gg_PO4_Extract2
  )
  
}





plot_resin_Fert= function(Resin_processed){
  
  
  
  gg_NH4_Extract2 =
    Resin_processed %>%
    mutate(Purpose2=factor(Purpose2, levels= c("OW-GS","OW","GS")))%>%
    filter(Treatment %in% c("Snowfence","Control","Ancillary","fertilized"))%>%
    mutate(Treatment=factor(Treatment, levels=c("Ancillary","Control","Snowfence","fertilized")))%>%
    ggplot(aes(x=Purpose2, y=Ammonium, fill=Site,alpha=Treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position= 'dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= 'dodge')+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_alpha_manual(values=c(0.25,0.5,0.75,1))+
    labs(x = "Season", 
         y = bquote('Ammonium ('*mu*'g '*NH[4]^"+"~-N~cm^-2 ~ Day^-1*')'))+
    ggtitle("Ammonium")
  
  
  gg_NO3_Extract2 =
    Resin_processed %>%
    mutate(Purpose2=factor(Purpose2, levels= c("OW-GS","OW","GS")))%>%
    filter(Treatment %in% c("Snowfence","Control","Ancillary","fertilized"))%>%
    mutate(Treatment=factor(Treatment, levels=c("Ancillary","Control","Snowfence","fertilized")))%>%
    ggplot(aes(x=Purpose2, y=Nitrate, fill=Site,alpha=Treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position= 'dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= 'dodge')+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_alpha_manual(values=c(0.25,0.5,0.75,1))+
    labs(x = "Season", 
         y = bquote('Nitrate ('*mu*'g '*NO[3]^"-"~-N~cm^-2 ~ Day^-1*')'))+
    ggtitle("Nitrate")
  
  gg_PO4_Extract2 =
    Resin_processed %>%
    mutate(Purpose2=factor(Purpose2, levels= c("OW-GS","OW","GS")))%>%
    filter(Treatment %in% c("Snowfence","Control","Ancillary","fertilized"))%>%
    mutate(Treatment=factor(Treatment, levels=c("Ancillary","Control","Snowfence","fertilized")))%>%
    ggplot(aes(x=Purpose2, y=Phosphate, fill=Site,alpha=Treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position= 'dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= 'dodge')+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_alpha_manual(values=c(0.25,0.5,0.75,1))+
    labs(x = "Season", 
         y = bquote('Phosphate ('*mu*'g '*PO[4]^"3-"~-P~cm^-2 ~ Day^-1*')'))+
    ggtitle("Phosphate")
  
  
  list("resin NH4-Treatment"= gg_NH4_Extract2,
       "resin NO3-Treatment"= gg_NO3_Extract2,
       "resin PO4-Treatment"= gg_PO4_Extract2
  )
  
}






