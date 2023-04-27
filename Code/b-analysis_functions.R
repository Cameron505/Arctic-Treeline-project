

#### The aggie projects primary goal is to assess snow pack and its impact on soil nutrients. 
####This section plots the comparisons between snow fence and control treatments.

plot_Extract_Snowfence = function(Extract_processed){
  
  fit_aov = function(Extract_processed){
    
    a = aov(conc ~ treatment, data = Extract_processed)
    broom::tidy(a) %>% 
      filter(term == "treatment") %>% 
      dplyr::select(`p.value`) %>% 
      mutate(asterisk = case_when(`p.value` <= 0.05 ~ "*"))
    
  }
  
  Extract_processed_long = Extract_processed %>%
    select(-c(Core.length:TRS.H2O)) %>%
    pivot_longer(cols= TOC.k2so4:Mic.PO4,
                 names_to= "analyte",
                 values_to= "conc") %>%
    filter(analyte!= "Mic.PO4")
  
  
  Extract_Snowfence_aov = 
    Extract_processed_long %>% 
    group_by(analyte,Site, YEAR, TIME) %>%
    na.omit()%>%
    do(fit_aov(.)) %>%
    mutate( treatment="Snowfence")
 
  
  
  
  gg_NH4_Extract =
    Extract_processed %>%
    ggplot(aes(x=TIME, y=NH4, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    geom_text(data = Extract_Snowfence_aov %>% filter(analyte == "NH4"), aes(y = 100, label = asterisk), size=10)+
    theme_light()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Time of year", 
         y = bquote('Ammonium ('*mu*'g '*NH[4]^"+"~-N~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Ammonium")
  
  
  gg_NO3_Extract =
    Extract_processed %>%
    ggplot(aes(x=TIME, y=NO3, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    geom_text(data = Extract_Snowfence_aov %>% filter(analyte == "NO3"), aes(y = 100, label = asterisk), size=10)+
    theme_light()+     theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Time of year", 
         y = bquote('Nitrate ('*mu*'g '*NO[3]^"-"~-N~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Nitrate")
  
  gg_PO4_Extract =
    Extract_processed %>%
    ggplot(aes(x=TIME, y=PO4, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    geom_text(data = Extract_Snowfence_aov %>% filter(analyte == "PO4"), aes(y = 25, label = asterisk), size=4)+
    theme_light()+     theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Time of year", 
         y = bquote('Phosphate ('*mu*'g '*PO[4]^"3-"~-P~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Phosphate")
  
  gg_TRS_Extract =
    Extract_processed %>%
    ggplot(aes(x=TIME, y=TRS, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    geom_text(data = Extract_Snowfence_aov %>% filter(analyte == "TRS"), aes(y = 9, label = asterisk), size=4)+
    theme_light()+     theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Time of year", 
         y = bquote('Total reducing sugars-glucose equiv. ('*mu*'g' ~g^-1 ~ dry ~ soil*')'))+
    ggtitle("TRS")
  
  gg_phenolics_Extract =
    Extract_processed %>%
    ggplot(aes(x=TIME, y=phenolics, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    geom_text(data = Extract_Snowfence_aov %>% filter(analyte == "phenolics"), aes(y = 9, label = asterisk), size=4)+
    theme_light()+     theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Time of year", 
         y = bquote('Phenolics-gallic acid equiv. ('*mu*'g' ~g^-1 ~ dry ~ soil*')'))+
    ggtitle("phenolics")
  
  gg_TFPA_Extract =
    Extract_processed %>%
    ggplot(aes(x=TIME, y=TFPA, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    geom_text(data = Extract_Snowfence_aov %>% filter(analyte == "TFPA"), aes(y = 4000, label = asterisk), size=4)+
    theme_light()+     theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Time of year", 
         y = bquote('Total free primary amines-Leucine equiv. (nMol' ~g^-1 ~ dry ~ soil*')'))+
    ggtitle("TFPA")
  
  gg_MBC_Extract =
    Extract_processed %>%
    ggplot(aes(x=TIME, y=MBC, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    geom_text(data = Extract_Snowfence_aov %>% filter(analyte == "MBC"), aes(y = 9, label = asterisk), size=4)+
    theme_light()+     theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Time of year", 
         y = bquote('Microbial biomass ('*mu*'g C'~g^-1 ~ dry ~ soil*')'))+
    ggtitle("MBC")
  
  gg_MBN_Extract =
    Extract_processed %>%
    ggplot(aes(x=TIME, y=MBN, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    geom_text(data = Extract_Snowfence_aov %>% filter(analyte == "MBN"), aes(y = 1000, label = asterisk), size=4)+
    theme_light()+     theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Time of year", 
         y = bquote('Microbial biomass ('*mu*'g N'~g^-1 ~ dry ~ soil*')'))+
    ggtitle("MBN")
  
  gg_MBP_Extract =
    Extract_processed %>%
    ggplot(aes(x=TIME, y=Mic.PO4, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    geom_text(data = Extract_Snowfence_aov %>% filter(analyte == "Mic.PO4"), aes(y = 9, label = asterisk), size=4)+
    theme_light()+     theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Time of year", 
         y = bquote('Microbial biomass ('*mu*'g P'~g^-1 ~ dry ~ soil*')'))+
    ggtitle("MBP")
  
  
  

  
  Extract_Snowfence_aov3 = 
    Extract_processed_long %>% 
    group_by(analyte,Site, YEAR, TIME) %>%
    na.omit()%>%
    do(fit_aov(.)) %>%
    knitr::kable()
  
  Extract_Snowfence_aov2 = 
    Extract_processed_long %>% 
    group_by(analyte,Site, YEAR, TIME) %>%
    na.omit()%>%
    do(fit_aov(.))%>%
    na.omit() %>%
    knitr::kable()
  
  Extract_Snowfence_aov2
  
  
  list("Snowfence Vs. Control NH4"= gg_NH4_Extract,
       "Snowfence Vs. Control NO3"= gg_NO3_Extract,
       "Snowfence Vs. Control PO4"= gg_PO4_Extract,
       "Snowfence Vs. Control TFPA"= gg_TFPA_Extract,
       "Snowfence Vs. Control Phenolics"= gg_phenolics_Extract,
       "Snowfence Vs. Control TRS"= gg_TRS_Extract,
       "Snowfence Vs. Control MBC"= gg_MBC_Extract,
       "Snowfence Vs. Control MBN"= gg_MBN_Extract,
       "Snowfence Vs. Control MBP"= gg_MBP_Extract,
       Extract_Snowfence_aov2=Extract_Snowfence_aov2,
       Extract_Snowfence_aov3=Extract_Snowfence_aov3
       )
  
 
  
}

plot_Extract_Snowfence_H2O = function(Extract_processed_H2O){
  
  fit_aov = function(Extract_processed_H2O){
    
    a = aov(conc ~ treatment, data = Extract_processed_H2O)
    broom::tidy(a) %>% 
      filter(term == "treatment") %>% 
      dplyr::select(`p.value`) %>% 
      mutate(asterisk = case_when(`p.value` <= 0.05 ~ "*"))
    
  }
  
  Extract_processed_long_H2O = Extract_processed_H2O %>%
    select(-c(TOC.k2so4:Dry.weight)) %>%
    mutate_at('TRS.H2O',as.numeric)%>%
    pivot_longer(cols= NH4.H2O:TRS.H2O,
                 names_to= "analyte",
                 values_to= "conc") %>%
    filter(analyte!= "Mic.PO4")
  
  
  Extract_Snowfence_aov_H2O = 
    Extract_processed_long_H2O %>% 
    group_by(analyte,Site, YEAR, TIME) %>%
    na.omit()%>%
    do(fit_aov(.)) %>%
    mutate( treatment="Snowfence")
  
  
  
  
  
  gg_NH4_Extract =
    Extract_processed_H2O %>%
    ggplot(aes(x=TIME, y=NH4.H2O, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    geom_text(data = Extract_Snowfence_aov_H2O %>% filter(analyte == "NH4.H2O"), aes(y = 50, label = asterisk), size=4)+
    theme_light()+     theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Month", 
         y = bquote('Ammonium ('*mu*'g '*NH[4]^"+"~-N~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Water Extracted Ammonium")
  
  
  gg_NO3_Extract =
    Extract_processed_H2O %>%
    ggplot(aes(x=TIME, y=NO3.H2O, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    geom_text(data = Extract_Snowfence_aov_H2O %>% filter(analyte == "NO3.H2O"), aes(y = 60, label = asterisk), size=10)+
    theme_light()+     theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Month", 
         y = bquote('Nitrate ('*mu*'g '*NO[3]^"-"~-N~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Water Extracted Nitrate")
  
  gg_PO4_Extract =
    Extract_processed_H2O %>%
    ggplot(aes(x=TIME, y=PO4.H2O, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    geom_text(data = Extract_Snowfence_aov_H2O %>% filter(analyte == "PO4.H2O"), aes(y = 60, label = asterisk), size=10)+
    theme_light()+     theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Month", 
         y = bquote('Phosphate ('*mu*'g '*PO[4]^"3-"~-P~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Water Extracted Phosphate")
  
  gg_TRS_Extract =
    Extract_processed_H2O %>%
    ggplot(aes(x=TIME, y=TRS.H2O, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    geom_text(data = Extract_Snowfence_aov_H2O %>% filter(analyte == "TRS.H2O"), aes(y = 2, label = asterisk), size=4)+
    theme_light()+     theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Month", 
         y = bquote('Total reducing sugars-glucose equiv. ('*mu*'g' ~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Water Extracted TRS")
  
  
  gg_TFPA_Extract =
    Extract_processed_H2O %>%
    ggplot(aes(x=TIME, y=TFPA.H2O, fill=treatment))+
    stat_summary(fun="mean",geom = "bar",size = 2, position='dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position='dodge')+
    facet_wrap(~YEAR+Site)+
    geom_text(data = Extract_Snowfence_aov_H2O %>% filter(analyte == "TFPA.H2O"), aes(y = 60, label = asterisk), size=10)+
    theme_light()+     theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Month", 
         y = bquote('Total free primary amines-Leucine equiv. (nMol' ~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Water Extracted TFPA")
  
  Extract_Snowfence_aov3_H2O = 
    Extract_processed_long_H2O %>% 
    group_by(analyte,Site, YEAR, TIME) %>%
    na.omit()%>%
    do(fit_aov(.)) %>%
    knitr::kable()
  
  Extract_Snowfence_aov2_H2O = 
    Extract_processed_long_H2O %>% 
    group_by(analyte,Site, YEAR, TIME) %>%
    na.omit()%>%
    do(fit_aov(.))%>%
    na.omit() %>%
    knitr::kable()
  
  Extract_Snowfence_aov2_H2O
  
  
  list("Snowfence Vs. Control H2O NH4"= gg_NH4_Extract,
       "Snowfence Vs. Control H2O NO3"= gg_NO3_Extract,
       "Snowfence Vs. Control H2O PO4"= gg_PO4_Extract,
       "Snowfence Vs. Control H2O TFPA"= gg_TFPA_Extract,
       "Snowfence Vs. Control H2O TRS"= gg_TRS_Extract,
       Extract_Snowfence_aov2_H2O= Extract_Snowfence_aov2_H2O,
       Extract_Snowfence_aov3_H2O=Extract_Snowfence_aov3_H2O
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
  
  
  
  PoreWater_processed_long = PoreWater_processed %>%
    pivot_longer(cols= Mass:TRS,
                 names_to= "analyte",
                 values_to= "conc") 
  
  
  Fit.LME=function(PoreWater_processed_long){
  a = nlme::lme(conc ~ MONTH + YEAR + Site + treatment,
                random = ~1|Plot,
                data = PoreWater_processed_long,na.action=na.exclude)%>%
  anova()
  
  a %>% 
    as.data.frame() %>% 
    rownames_to_column("variable") %>% 
    filter(variable %in% c("MONTH","YEAR","Site","treatment")) %>%
    dplyr::rename( p_value = 'p-value')%>%
    mutate(p_value = round(p_value, 3),asterisk = case_when(`p_value` <= 0.05 ~ "*"))
  }
  
  
  Porewater_Snowfence_LME = 
    PoreWater_processed_long %>% 
    group_by(analyte)%>%
    do(Fit.LME(.))%>%
    knitr::kable()
  
  Porewater_Snowfence_LME2 = 
    PoreWater_processed_long %>% 
    group_by(analyte)%>%
    do(Fit.LME(.)) %>%
    na.omit()%>%
    knitr::kable()
  
  
  
 
  
  
  
  
  list("Snowfence Vs. Control PoreWater_NH4"= gg_NH4_PoreWater,
       "Snowfence Vs. Control PoreWater_NO3"= gg_NO3_PoreWater,
       "Snowfence Vs. Control PoreWater_PO4"= gg_PO4_PoreWater,
       "Snowfence Vs. Control PoreWater_TFPA"= gg_TFPA_PoreWater,
       #"Snowfence Vs. Control PoreWater_Phenolics"= gg_phenolics_PoreWater,
       "Snowfence Vs. Control PoreWater_TRS"= gg_TRS_PoreWater,
       Porewater_Snowfence_LME=Porewater_Snowfence_LME,
       Porewater_Snowfence_LME2=Porewater_Snowfence_LME2
  )
}

plot_resin_Snowfence = function(Resin_processed){
  
  fit_aov = function(Resin_processed){
    
    a = aov(conc ~ Treatment, data = Resin_processed)
    broom::tidy(a) %>% 
      filter(term == "Treatment") %>% 
      dplyr::select(`p.value`) %>% 
      mutate(asterisk = case_when(`p.value` <= 0.05 ~ "*"))
    
  }
  
  fit_aov2 = function(Resin_processed){
    
    a = aov(conc ~ Site, data = Resin_processed)
    broom::tidy(a) %>% 
      filter(term == "Site") %>% 
      dplyr::select(`p.value`) %>% 
      mutate(asterisk = case_when(`p.value` <= 0.05 ~ "*"))
    
  }
  
  Extract_processed_long = Resin_processed %>%
    mutate(Purpose2=factor(Purpose2, levels= c("OW-GS","OW","GS")))%>%
    filter(Treatment %in% c("Snowfence","Control"))%>%
    select(-c(Matrix:Anion.area)) %>%
    pivot_longer(cols= Nitrate:Phosphate,
                 names_to= "analyte",
                 values_to= "conc") %>%
    filter(analyte!= "Mic.PO4")
  
  
  Extract_resin_aov = 
    Extract_processed_long %>% 
    group_by(analyte,YEAR,Site, Purpose2) %>%
    do(fit_aov(.)) %>%
    mutate( Treatment="Snowfence")
  
  Extract_resin_aov2 = 
    Extract_processed_long %>% 
    group_by(analyte,YEAR, Purpose2) %>%
    do(fit_aov2(.)) %>%
    mutate( Site="Hydric")
  
  gg_NH4_Extract2 =
    Resin_processed %>%
    mutate(Purpose2=factor(Purpose2, levels= c("OW-GS","OW","GS")))%>%
    filter(Treatment %in% c("Snowfence","Control"))%>%
    ggplot(aes(x=Purpose2, y=Ammonium, fill=Site))+
    stat_summary(fun="mean",geom = "bar",size = 2, position= 'dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= 'dodge')+
    facet_wrap(~YEAR, scale="free_x")+
    geom_text(data = Extract_resin_aov2 %>% filter(analyte == "Ammonium"), aes(y = 0.04, label = asterisk), size=10)+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_alpha_manual(values=c(0.25,1))+
    labs(x = "Season", 
         y = bquote('Ammonium ('*mu*'g '*NH[4]^"+"~-N~cm^-2 ~ Day^-1*')'))+
    ggtitle("Ammonium")
  
  
  gg_NO3_Extract2 =
    Resin_processed %>%
    mutate(Purpose2=factor(Purpose2, levels= c("OW-GS","OW","GS")))%>%
    filter(Treatment %in% c("Snowfence","Control"))%>%
    ggplot(aes(x=Purpose2, y=Nitrate, fill=Site))+
    stat_summary(fun="mean",geom = "bar",size = 2, position= 'dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= 'dodge')+
    geom_text(data = Extract_resin_aov2 %>% filter(analyte == "Nitrate"), aes(y = 0.01, label = asterisk), size=10)+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_alpha_manual(values=c(0.25,1))+
    labs(x = "Season", 
         y = bquote('Nitrate ('*mu*'g '*NO[3]^"-"~-N~cm^-2 ~ Day^-1*')'))+
    ggtitle("Nitrate")
  
  gg_PO4_Extract2 =
    Resin_processed %>%
    mutate(Purpose2=factor(Purpose2, levels= c("OW-GS","OW","GS")))%>%
    filter(Treatment %in% c("Snowfence","Control"))%>%
    ggplot(aes(x=Purpose2, y=Phosphate, fill=Site))+
    stat_summary(fun="mean",geom = "bar",size = 2, position= 'dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= 'dodge')+
    geom_text(data = Extract_resin_aov2 %>% filter(analyte == "Phosphate"), aes(y = 0.03, label = asterisk), size=10)+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_alpha_manual(values=c(0.25,1))+
    labs(x = "Season", 
         y = bquote('Phosphate ('*mu*'g '*PO[4]^"3-"~-P~cm^-2 ~ Day^-1*')'))+
    ggtitle("Phosphate")
  
  
  Extract_resin_aov3 = 
    Extract_processed_long %>% 
    group_by(analyte,YEAR,Treatment, Purpose2) %>%
    do(fit_aov2(.))%>%
    knitr::kable(caption="Comparing Sites (Significant only)")
  Extract_resin_aov4 = 
    Extract_processed_long %>% 
    group_by(analyte,YEAR,Treatment, Purpose2) %>%
    do(fit_aov2(.))%>%
    na.omit()%>%
    knitr::kable(caption="Comparing Sites")
  
  list("resin NH4-Treatment"= gg_NH4_Extract2,
       "resin NO3-Treatment"= gg_NO3_Extract2,
       "resin PO4-Treatment"= gg_PO4_Extract2,
       Extract_resin_aov4=Extract_resin_aov4,
       Extract_resin_aov3=Extract_resin_aov3
  )
  
}


#### due to higher frequency these plots are for ancillary only

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
  
  PoreWater_processed_long = PoreWater_processed_Seasonal %>%
    pivot_longer(cols= Mass:TRS,
                 names_to= "analyte",
                 values_to= "conc") 
  
  Fit.LME=function(PoreWater_processed_long){
    a = nlme::lme(conc ~ MONTH + YEAR + Site + treatment,
                  random = ~1|Plot,
                  data = PoreWater_processed_long,na.action=na.exclude)%>%
      anova()
    
    a %>% 
      as.data.frame() %>% 
      rownames_to_column("variable") %>% 
      filter(variable %in% c("MONTH","YEAR","Site","treatment")) %>%
      dplyr::rename( p_value = 'p-value')%>%
      mutate(p_value = round(p_value, 3),asterisk = case_when(`p_value` <= 0.05 ~ "*"))
  }
  
  
  Porewater_Seasonal_LME = 
    PoreWater_processed_long %>% 
    group_by(analyte)%>%
    do(Fit.LME(.))%>%
    na.omit()%>%
    knitr::kable()
  
  Porewater_Seasonal_LME2 = 
    PoreWater_processed_long %>% 
    group_by(analyte)%>%
    do(Fit.LME(.))%>%
    knitr::kable()
  
  list("Seasonal PoreWater_NH4"= gg_NH4_PoreWater,
       "Seasonal PoreWater_NO3"= gg_NO3_PoreWater,
       "Seasonal PoreWater_PO4"= gg_PO4_PoreWater,
       "Seasonal PoreWater_TFPA"= gg_TFPA_PoreWater,
       "Seasonal PoreWater_TRS"= gg_TRS_PoreWater,
       Porewater_Seasonal_LME=Porewater_Seasonal_LME,
       Porewater_Seasonal_LME2=Porewater_Seasonal_LME2
  )
}


#### Snowfence control and ancillary plots

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
  
  
  
  
  
  
  
  
  gg_MBC_3year =
    Extract_processed_all %>%
    filter(YEAR %in% c("2017","2018","2019") )%>%
    ggplot(aes(x=DATE, y=MBC, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 1.3) +
    stat_summary(fun="mean",geom = "point",size = 5) +
    stat_summary(fun.data = mean_se, geom = "errorbar", size=0.8, color="black")+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    theme(title = element_blank(),strip.text.x = element_blank(),
          axis.text.x = element_text(angle=90),
          axis.text=element_text(size=20),
          axis.title=element_text(size=25,face="bold"),
          legend.text = element_text(size=20))+
    scale_x_date(date_labels = "%b/%d")+
    scale_colour_manual(values= c( "#33FFFF", "#990099","#66CC00"))+
    labs(x = "Date", 
         y = bquote('Microbial biomass ('*mu*'g C'~g^-1 ~ dry ~ soil*')'))+
    ggtitle("MBC")
  ggsave(gg_MBC_3year,filename="MBC_3year.png","Graphs/", device= "png" ,width = 18, height = 12 , units = "in")
  
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
  
  
  
  
  
  PoreWater_processed_long = PoreWater_processed_all %>%
    pivot_longer(cols= Mass:TRS,
                 names_to= "analyte",
                 values_to= "conc") 
  
  
  Fit.LME=function(PoreWater_processed_long){
    a = nlme::lme(conc ~ MONTH + YEAR + Site + treatment,
                  random = ~1|Plot,
                  data = PoreWater_processed_long,na.action=na.exclude)%>%
      anova()
    
    a %>% 
      as.data.frame() %>% 
      rownames_to_column("variable") %>% 
      filter(variable %in% c("MONTH","YEAR","Site","treatment")) %>%
      dplyr::rename(p_value = 'p-value') %>%
      mutate(p_value = round(p_value, 3),asterisk = case_when(`p_value` <= 0.05 ~ "*"))
  }
  
  
  Porewater_all_LME = 
    PoreWater_processed_long %>% 
    group_by(analyte)%>%
    do(Fit.LME(.))%>%
    na.omit()%>%
    knitr::kable()
  
  Porewater_all_LME2 = 
    PoreWater_processed_long %>% 
    group_by(analyte)%>%
    do(Fit.LME(.))%>%
    knitr::kable()
  
  list("all PoreWater_NH4"= gg_NH4_PoreWater,
       "all PoreWater_NO3"= gg_NO3_PoreWater,
       "all PoreWater_PO4"= gg_PO4_PoreWater,
       "all PoreWater_TFPA"= gg_TFPA_PoreWater,
       #"all PoreWater_Phenolics"= gg_phenolics_PoreWater,
       "all PoreWater_TRS"= gg_TRS_PoreWater,
       Porewater_all_LME=Porewater_all_LME,
       Porewater_all_LME2=Porewater_all_LME2
  )
}


#### Plotting data from fert plots.

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
  
  
  
  PoreWater_data = PoreWater_data %>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','TRS'), as.numeric)
  
  PoreWater_processed_long = PoreWater_data %>%
    pivot_longer(cols= Mass:TRS,
                 names_to= "analyte",
                 values_to= "conc") 
  
  
  Fit.LME=function(PoreWater_processed_long){
    a = nlme::lme(conc ~ MONTH + YEAR + Site + treatment,
                  random = ~1|Plot,
                  data = PoreWater_processed_long,na.action=na.exclude)%>%
      anova()
    
    a %>% 
      as.data.frame() %>% 
      rownames_to_column("variable") %>% 
      filter(variable %in% c("MONTH","YEAR","Site","treatment")) %>%
      dplyr::rename(p_value = 'p-value') %>%
      mutate(p_value == round(p_value, 3),asterisk = case_when(`p_value` <= 0.05 ~ "*"))
  }
  
  
  Porewater_LME = 
    PoreWater_processed_long %>% 
    group_by(analyte)%>%
    do(Fit.LME(.))%>%
    na.omit()%>%
    knitr::kable()
  
  Porewater_LME2 = 
    PoreWater_processed_long %>% 
    group_by(analyte)%>%
    do(Fit.LME(.))%>%
    knitr::kable()
  
  
  
  
  
  list("Fert PoreWater_NH4"= gg_NH4_PoreWater,
       "Fert PoreWater_NO3"= gg_NO3_PoreWater,
       "Fert PoreWater_PO4"= gg_PO4_PoreWater,
       "Fert PoreWater_TFPA"= gg_TFPA_PoreWater,
       #"Fert PoreWater_Phenolics"= gg_phenolics_PoreWater,
       "Fert PoreWater_TRS"= gg_TRS_PoreWater,
       Porewater_LME=Porewater_LME,
       Porewater_LME2=Porewater_LME2
  )
}



#### Plotting destuctive lysimeter experiment

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







#### Resin strip data plots

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
    ggtitle("Ammonium-Ancillary")
  
  
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
    ggtitle("Nitrate-Ancillary")
  
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
    ggtitle("Phosphate-Ancillary")
  
  
  list("resin NH4-Treatment"= gg_NH4_Extract2,
       "resin NO3-Treatment"= gg_NO3_Extract2,
       "resin PO4-Treatment"= gg_PO4_Extract2
  )
  
}

plot_resin = function(Resin_processed){
  
  fit_aov = function(Resin_processed){
    
    a = aov(conc ~ Treatment, data = Resin_processed)
    broom::tidy(a) %>% 
      filter(term == "Treatment") %>% 
      dplyr::select(`p.value`) %>% 
      mutate(asterisk = case_when(`p.value` <= 0.05 ~ "*"))
    
  }
  
  fit_aov2 = function(Resin_processed){
    
    a = aov(conc ~ Site, data = Resin_processed)
    broom::tidy(a) %>% 
      filter(term == "Site") %>% 
      dplyr::select(`p.value`) %>% 
      mutate(asterisk = case_when(`p.value` <= 0.05 ~ "*"))
    
  }
  
  Extract_processed_long = Resin_processed %>%
    mutate(Purpose2=factor(Purpose2, levels= c("OW-GS","OW","GS")))%>%
    select(-c(Matrix:Anion.area)) %>%
    pivot_longer(cols= Nitrate:Phosphate,
                 names_to= "analyte",
                 values_to= "conc") %>%
    filter(analyte!= "Mic.PO4")
  
  
  Extract_resin_aov = 
    Extract_processed_long %>% 
    group_by(analyte,YEAR,Site,Purpose2) %>%
    do(fit_aov(.)) %>%
    mutate( Treatment="Snowfence")
  
  Extract_resin_aov2 = 
    Extract_processed_long %>% 
    group_by(analyte,YEAR, Purpose2) %>%
    do(fit_aov2(.)) %>%
  mutate( Site="Hydric")
  
  
  gg_NH4_Extract =
    Resin_processed %>%
    mutate(Purpose2=factor(Purpose2, levels= c("OW-GS","OW","GS")))%>%
    ggplot(aes(x=Purpose2, y=Ammonium, fill=Site))+
    stat_summary(fun="mean",geom = "bar",size = 2, position= 'dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= 'dodge')+
    facet_wrap(~YEAR, scale="free_x")+
    geom_text(data = Extract_resin_aov2 %>% filter(analyte == "Ammonium"), aes(y = 0.04, label = asterisk), size=6)+
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
    geom_text(data = Extract_resin_aov2 %>% filter(analyte == "Nitrate"), aes(y = 0.015, label = asterisk), size=6)+
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
    geom_text(data = Extract_resin_aov2 %>% filter(analyte == "Phosphate"), aes(y = 0.04, label = asterisk), size=6)+
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
  Extract_resin_aov3 = 
    Extract_processed_long %>% 
    group_by(analyte,YEAR, Purpose2) %>%
    do(fit_aov2(.)) %>%
    na.omit()%>%
    knitr::kable(caption="Significant differences between sites")
  Extract_resin_aov4 = 
    Extract_processed_long %>% 
    group_by(analyte,YEAR, Purpose2) %>%
    do(fit_aov2(.)) %>%
    knitr::kable(caption="differences between sites all")
  
  list("resin NH4"= gg_NH4_Extract,
       "resin NO3"= gg_NO3_Extract,
       "resin PO4"= gg_PO4_Extract,
       "resin NH4-Treatment"= gg_NH4_Extract2,
       "resin NO3-Treatment"= gg_NO3_Extract2,
       "resin PO4-Treatment"= gg_PO4_Extract2,
       Extract_resin_aov3=Extract_resin_aov3,
       Extract_resin_aov4=Extract_resin_aov4
  )
  
}

plot_resin_Fert= function(Resin_processed){
  
  fit_aov = function(Resin_processed){
    
    a = aov(conc ~ Treatment, data = Resin_processed)
    broom::tidy(a) %>% 
      filter(term == "Treatment") %>% 
      dplyr::select(`p.value`) %>% 
      mutate(asterisk = case_when(`p.value` <= 0.05 ~ "*"))
    
  }
  
  fit_aov2 = function(Resin_processed){
    
    a = aov(conc ~ Site, data = Resin_processed)
    broom::tidy(a) %>% 
      filter(term == "Site") %>% 
      dplyr::select(`p.value`) %>% 
      mutate(asterisk = case_when(`p.value` <= 0.05 ~ "*"))
    
  }
  
  Extract_processed_long = Resin_processed %>%
    mutate(Purpose2=factor(Purpose2, levels= c("OW-GS","OW","GS")))%>%
    select(-c(Matrix:Anion.area)) %>%
    pivot_longer(cols= Nitrate:Phosphate,
                 names_to= "analyte",
                 values_to= "conc") %>%
    filter(analyte!= "Mic.PO4")
  
  
  Extract_resin_aov = 
    Extract_processed_long %>% 
    group_by(analyte,YEAR,Site, Purpose2) %>%
    do(fit_aov(.)) %>%
    mutate( Treatment="Snowfence")
  
  Extract_resin_aov2 = 
    Extract_processed_long %>% 
    group_by(analyte,YEAR, Purpose2) %>%
    do(fit_aov2(.)) 
    #mutate( Site="Hydric")
  
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




#### PLots and tables for FTICR

plot_fticr_Domains= function(fticr_meta,fticr_data_longform,fticr_data_trt){
  
  TREATMENTS = dplyr::quos(Site, Year, Season, Polar)
  
  gg_vk_domains = 
    gg_vankrev(fticr_meta, aes(x = OC, y = HC, color = Class))+
    scale_color_manual(values = PNWColors::pnw_palette("Sunset2"))+
    theme_kp()
  
  gg_vk_domains_nosc = 
  gg_vankrev(fticr_meta, aes(x = OC, y = HC, color = as.numeric(NOSC)))+
    scale_color_gradientn(colors = PNWColors::pnw_palette("Bay"))+
    theme_kp()
  
  
  list("vk domains"= gg_vk_domains,
       "vk domains nosc"= gg_vk_domains_nosc
  )
  
}

plot_polarVnonPolar_vk = function(fticr_hcoc){
  
    fticr_hcoc %>%
       distinct(formula, HC, OC, Polar) %>% 
       gg_vankrev(aes(x = OC, y = HC, color = Polar))+
       stat_ellipse(level = 0.90, show.legend = FALSE)+
       theme(legend.position = c(0.8, 0.8)) +
       NULL
    # include marginal density plots in the RMarkdown because of rendering issues
    # see https://github.com/daattali/ggExtra/issues/147
  
}

plot_polarVnonPolar_pca= function(relabund_cores){
  pca_all = fit_pca_function(relabund_cores)
  
  #gg_pca_polar_nonpolar = 
    ggbiplot(pca_all$pca_int, obs.scale = 1, var.scale = 1,
             groups = as.character(pca_all$grp$Polar), 
             ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
    geom_point(size=3,stroke=1, alpha = 0.5,
               aes(#shape = groups,
                 color = groups))+
    #scale_shape_manual(values = c(21, 22, 19), name = "", guide = "none")+
    xlim(-4,4)+
    ylim(-3.5,3.5)+
    labs(shape="",
         title = "all samples",
         subtitle = "polar vs. nonpolar")+
    theme_kp()+
    NULL
}

plot_pca_by_site= function(pca_polar){
  
  gg_pca_by_site = 
    ggbiplot(pca_polar$pca_int, obs.scale = 1, var.scale = 1,
             groups = as.character(pca_polar$grp$Site), 
             ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
    geom_point(size=3,stroke=1, alpha = 1,
               aes(#shape = groups,
                 color = groups))+
    #scale_shape_manual(values = c(21, 22, 19), name = "", guide = "none")+
    xlim(-4,4)+
    ylim(-3.5,3.5)+
    labs(shape="",
         title = "all samples",
         subtitle = "separation by Site")+
    theme_kp()+
    NULL
  
  
  list("pca_by_site"= gg_pca_by_site
  )
  
}

plot_pca_polar= function(pca_polar){
  
  gg_pca_by_year = 
    ggbiplot(pca_polar$pca_int, obs.scale = 1, var.scale = 1,
             groups = as.character(pca_polar$grp$Year), 
             ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
    geom_point(size=3,stroke=1, alpha = 1,
               aes(
                 #shape = as.character(pca_polar$grp$Site),
                 color = groups))+
    #scale_shape_manual(values = c(21, 22, 19), name = "", guide = "none")+
    xlim(-4,4)+
    ylim(-3.5,3.5)+
    labs(shape="",
         title = "all samples",
         subtitle = "separation by Site")+
    theme_kp()+
    NULL
 
  gg_pca_by_season = 
    ggbiplot(pca_polar$pca_int, obs.scale = 1, var.scale = 1,
             groups = as.character(pca_polar$grp$Season), 
             ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
    geom_point(size=3,stroke=1, alpha = 1,
               aes(#shape = groups,
                 color = groups))+
    #scale_shape_manual(values = c(21, 22, 19), name = "", guide = "none")+
    xlim(-4,4)+
    ylim(-3.5,3.5)+
    labs(shape="",
         title = "all samples",
         subtitle = "separation by Site")+
    theme_kp()+
    NULL
  
  
  list("pca_by_year"= gg_pca_by_year,
       "pca_by_season"= gg_pca_by_season
  )
  
}

plot_pca_hydric= function(pca_hydric){
  
  gg_pca_hydric = 
    ggbiplot(pca_hydric$pca_int, obs.scale = 1, var.scale = 1,
             groups = as.character(pca_hydric$grp$Season), 
             ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
    geom_point(size=3,stroke=1, alpha = 1,
               aes(shape = as.character(pca_hydric$grp$Year),
                   color = groups))+
    #scale_shape_manual(values = c(21, 22, 19), name = "", guide = "none")+
    xlim(-4,4)+
    ylim(-3.5,3.5)+
    labs(shape="",
         title = "Hydric samples",
         subtitle = "separation by Season")+
    theme_kp()+
    NULL
  
  
  list("pca_hydric"= gg_pca_hydric
  )
  
}

plot_vk_polar= function(fticr_hcoc_polar){
  
  gg_vk_all = 
    gg_vankrev(fticr_hcoc_polar, aes(x = OC, y = HC, color = Site))+
    stat_ellipse(level = 0.90, show.legend = FALSE)+
    facet_grid(Season ~ Polar + Year)
  
  gg_vk_all_site = 
    gg_vankrev(fticr_hcoc_polar, aes(x = OC, y = HC, color = as.character(Year)))+
    stat_ellipse(level = 0.90, show.legend = FALSE)+
    facet_grid(. ~ Site)
  
  list("gg_vk_all_polar"= gg_vk_all,
       "gg_vk_site_polar"= gg_vk_all_site
  )
  
}

plot_unique= function(fticr_hcoc, fticr_meta){
  
  fticr_unique_site = 
    fticr_hcoc %>% 
    distinct(formula, Site, HC, OC) %>% 
    group_by(formula) %>% 
    dplyr::mutate(n = n())
  
  # overlay unique peaks onto common peaks
  gg_site_common_unique = 
  fticr_unique_site %>% filter(n == 3) %>% 
    gg_vankrev(aes(x = OC, y = HC))+
    geom_point(data = fticr_unique_site %>% filter(n == 1),
               aes(color = Site), alpha = 0.7)+
    facet_wrap(~Site)+
    labs(title = "Unique peaks at each Site",
         subtitle = "black/grey = peaks common to all")
  
    UniqueT = fticr_unique_site %>% 
    filter(n == 1) %>% 
    left_join(fticr_meta %>% dplyr::select(formula, Class)) %>% 
    group_by(Site, Class) %>% 
    dplyr::summarise(counts = n()) %>% 
    pivot_wider(names_from = "Site", values_from = "counts") %>%
    knitr::kable()

    
  list("site_common_unique"= gg_site_common_unique,
       "PERMANOVA_table"= UniqueT
  )
  
}

plot_unique2= function(fticr_hcoc, fticr_meta){
  
  fticr_unique_site = 
    fticr_hcoc %>% 
    distinct(formula, Site, HC, OC) %>% 
    group_by(formula) %>% 
    dplyr::mutate(n = n())
  
  
  fticr_unique_site %>% 
    filter(n == 1) %>% 
    left_join(fticr_meta %>% dplyr::select(formula, Class)) %>% 
    group_by(Site, Class) %>% 
    dplyr::summarise(counts = n()) %>% 
    pivot_wider(names_from = "Site", values_from = "counts") %>%
    knitr::kable()
  
  
}

plot_seasonal_Mesic_Hydric_polar= function(fticr_hcoc_polar){
  
  fticr_hcoc_polar_mesic_hydric = 
    fticr_hcoc_polar %>% 
    filter(Site %in% c("Mesic", "Hydric","Xeric"))
  
  gg_seasonal= fticr_hcoc_polar_mesic_hydric %>% 
    gg_vankrev(aes(x = OC, y = HC, color = Season))+
    facet_grid(Site ~ Year)
  
  list("seasonal"= gg_seasonal
  )
  
}

plot_permanova_polar= function( relabund_cores_polar){
  relabund_wide = 
    relabund_cores_polar %>% 
    ungroup() %>% 
    mutate(Class = factor(Class, 
                          levels = c("aliphatic", "unsaturated/lignin", 
                                     "aromatic", "condensed aromatic"))) %>% 
    dplyr::select(-c(abund, total)) %>% 
    spread(Class, relabund) %>% 
    replace(is.na(.), 0)
  
  # adonis function for PERMANOVA
  permanova_fticr_all = 
    adonis(relabund_wide %>% dplyr::select(aliphatic:`condensed aromatic`) ~ (Site + Year + Season)^2, 
           data = relabund_wide) 
  
 knitr::kable(permanova_fticr_all$aov.tab)
  
  
}




