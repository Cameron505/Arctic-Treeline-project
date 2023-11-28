

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
    ggplot(aes(x=TIME, y=NH4, color=treatment))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2,
                 position = position_dodge(width = 0.5),
                 aes(group = interaction(Site,TIME, treatment, YEAR)),
                 width=0.4)+
    geom_point(position = position_dodge(width = 0.5), size=1.6)+
    facet_wrap(~YEAR+Site)+
    geom_text(data = Extract_Snowfence_aov %>% filter(analyte == "NH4"), aes(y = 100, label = asterisk), size=10, show.legend = FALSE, color="black")+
    theme_light()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_x_discrete(breaks=c("Late Winter", "Early Spring", "Late Summer"), labels=c("March", "June", "August"))+
    labs(x = "Month", 
         y = bquote('Ammonium ('*mu*'g '*NH[4]^"+"~-N~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Ammonium")+
    theme_CKM()+
    guides(color=guide_legend(title="Treatment"))
  
  
  gg_NO3_Extract =
    Extract_processed %>%
    ggplot(aes(x=TIME, y=NO3, color=treatment))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2,
                 position = position_dodge(width = 0.5),
                 aes(group = interaction(Site,TIME, treatment, YEAR)),
                 width = 0.4)+
    geom_point(position = position_dodge(width = 0.5), size=1.6)+
    facet_wrap(~YEAR+Site)+
    geom_text(data = Extract_Snowfence_aov %>% filter(analyte == "NO3"), aes(y = 100, label = asterisk), size=10, show.legend = FALSE, color="black")+
    theme_light()+     theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_x_discrete(breaks=c("Late Winter", "Early Spring", "Late Summer"), labels=c("March", "June", "August"))+
    labs(x = "Month", 
         y = bquote('Nitrate ('*mu*'g '*NO[3]^"-"~-N~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Nitrate")+
    theme_CKM()+
    guides(color=guide_legend(title="Treatment"))
  
  gg_PO4_Extract =
    Extract_processed %>%
    ggplot(aes(x=TIME, y=PO4, color=treatment))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2,
                 position = position_dodge(width = 0.5),
                 aes(group = interaction(Site,TIME, treatment, YEAR)),
                 width = 0.4)+
    geom_point(position = position_dodge(width = 0.5), size=1.6)+
    facet_wrap(~YEAR+Site)+
    geom_text(data = Extract_Snowfence_aov %>% filter(analyte == "PO4"), aes(y = 25, label = asterisk), size=4, show.legend = FALSE, color="black")+
    theme_light()+     theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_x_discrete(breaks=c("Late Winter", "Early Spring", "Late Summer"), labels=c("March", "June", "August"))+
    labs(x = "Month",  
         y = bquote('Phosphate ('*mu*'g '*PO[4]^"3-"~-P~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Phosphate")+
    theme_CKM()+
    guides(color=guide_legend(title="Treatment"))
  
  gg_TRS_Extract =
    Extract_processed %>%
    ggplot(aes(x=TIME, y=TRS, color=treatment))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2,
                 position = position_dodge(width = 0.5),
                 aes(group = interaction(Site,TIME, treatment, YEAR)),
                 width = 0.4)+
    geom_point(position = position_dodge(width = 0.5), size=1.6)+
    facet_wrap(~YEAR+Site)+
    geom_text(data = Extract_Snowfence_aov %>% filter(analyte == "TRS"), aes(y = 9, label = asterisk), size=4, show.legend = FALSE, color="black")+
    theme_light()+     theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_x_discrete(breaks=c("Late Winter", "Early Spring", "Late Summer"), labels=c("March", "June", "August"))+
    labs(x = "Month",  
         y = bquote('Total reducing sugars-glucose equiv. ('*mu*'g' ~g^-1 ~ dry ~ soil*')'))+
    ggtitle("TRS")+
    theme_CKM()+
    guides(color=guide_legend(title="Treatment"))
  
  gg_phenolics_Extract =
    Extract_processed %>%
    ggplot(aes(x=TIME, y=phenolics, color=treatment))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2,
                 position = position_dodge(width = 0.5),
                 aes(group = interaction(Site,TIME, treatment, YEAR)),
                 width = 0.4)+
    geom_point(position = position_dodge(width = 0.5), size=1.6)+
    facet_wrap(~YEAR+Site)+
    geom_text(data = Extract_Snowfence_aov %>% filter(analyte == "phenolics"), aes(y = 9, label = asterisk), size=4, show.legend = FALSE, color="black")+
    theme_light()+     theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_x_discrete(breaks=c("Late Winter", "Early Spring", "Late Summer"), labels=c("March", "June", "August"))+
    labs(x = "Month", 
         y = bquote('Phenolics-gallic acid equiv. ('*mu*'g' ~g^-1 ~ dry ~ soil*')'))+
    ggtitle("phenolics")+
    theme_CKM()+
    guides(color=guide_legend(title="Treatment"))
  
  gg_TFPA_Extract =
    Extract_processed %>%
    ggplot(aes(x=TIME, y=TFPA, color=treatment))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2,
                 position = position_dodge(width = 0.5),
                 aes(group = interaction(Site,TIME, treatment, YEAR)),
                 width = 0.4)+
    geom_point(position = position_dodge(width = 0.5), size=1.6)+
    facet_wrap(~YEAR+Site)+
    geom_text(data = Extract_Snowfence_aov %>% filter(analyte == "TFPA"), aes(y = 4000, label = asterisk), size=4, show.legend = FALSE, color="black")+
    theme_light()+     theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_x_discrete(breaks=c("Late Winter", "Early Spring", "Late Summer"), labels=c("March", "June", "August"))+
    labs(x = "Month",  
         y = bquote('Total free primary amines-Leucine equiv. (nMol' ~g^-1 ~ dry ~ soil*')'))+
    ggtitle("TFPA")+
    theme_CKM()+
    guides(color=guide_legend(title="Treatment"))
  
  gg_MBC_Extract =
    Extract_processed %>%
    ggplot(aes(x=TIME, y=MBC, color=treatment))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2,
                 position = position_dodge(width = 0.5),
                 aes(group = interaction(Site,TIME, treatment, YEAR)),
                 width = 0.4)+
    geom_point(position = position_dodge(width = 0.5), size=1.6)+
    facet_wrap(~YEAR+Site)+
    geom_text(data = Extract_Snowfence_aov %>% filter(analyte == "MBC"), aes(y = 9, label = asterisk), size=4, show.legend = FALSE, color="black")+
    theme_light()+     theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_x_discrete(breaks=c("Late Winter", "Early Spring", "Late Summer"), labels=c("March", "June", "August"))+
    labs(x = "Month", 
         y = bquote('Microbial biomass ('*mu*'g C'~g^-1 ~ dry ~ soil*')'))+
    ggtitle("MBC")+
    theme_CKM()+
    guides(color=guide_legend(title="Treatment"))
  
  gg_MBN_Extract =
    Extract_processed %>%
    ggplot(aes(x=TIME, y=MBN, color=treatment))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2,
                 position = position_dodge(width = 0.5),
                 aes(group = interaction(Site,TIME, treatment, YEAR)),
                 width = 0.4)+
    geom_point(position = position_dodge(width = 0.5), size=1.6)+
    facet_wrap(~YEAR+Site)+
    geom_text(data = Extract_Snowfence_aov %>% filter(analyte == "MBN"), aes(y = 1000, label = asterisk), size=4, show.legend = FALSE, color="black")+
    theme_light()+     theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_x_discrete(breaks=c("Late Winter", "Early Spring", "Late Summer"), labels=c("March", "June", "August"))+
    labs(x = "Month", 
         y = bquote('Microbial biomass ('*mu*'g N'~g^-1 ~ dry ~ soil*')'))+
    ggtitle("MBN")+
    theme_CKM()+
    guides(color=guide_legend(title="Treatment"))
  
  gg_MBP_Extract =
    Extract_processed %>%
    ggplot(aes(x=TIME, y=Mic.PO4, color=treatment))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2,
                 position = position_dodge(width = 0.5),
                 aes(group = interaction(Site,TIME, treatment, YEAR)),
                 width = 0.4)+
    geom_point(position = position_dodge(width = 0.5), size=1.6)+
    facet_wrap(~YEAR+Site)+
    geom_text(data = Extract_Snowfence_aov %>% filter(analyte == "Mic.PO4"), aes(y = 9, label = asterisk), size=4, show.legend = FALSE, color="black")+
    theme_light()+     theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_x_discrete(breaks=c("Late Winter", "Early Spring", "Late Summer"), labels=c("March", "June", "August"))+
    labs(x = "Month",  
         y = bquote('Microbial biomass ('*mu*'g P'~g^-1 ~ dry ~ soil*')'))+
    ggtitle("MBP")+
    theme_CKM()+
    guides(color=guide_legend(title="Treatment"))
  
  
  
  
  
  Extract_Snowfence_aov3 = 
    Extract_processed_long %>% 
    group_by(analyte,Site, YEAR, TIME) %>%
    na.omit()%>%
    do(fit_aov(.)) %>%
    knitr::kable(caption = "Significant comparisons")
  
  Extract_Snowfence_aov2 = 
    Extract_processed_long %>% 
    group_by(analyte,Site, YEAR, TIME) %>%
    na.omit()%>%
    do(fit_aov(.))%>%
    na.omit() %>%
    knitr::kable(caption= "All comparisons")
  
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
    mutate(Purpose3=paste(Extract_processed_long$Purpose2,Extract_processed_long$YEAR))%>%
    group_by(analyte,YEAR, Purpose3) %>%
    do(fit_aov2(.)) %>%
    mutate( Site="East wet")
  
  Resin_processed$Purpose3<- paste(Resin_processed$Purpose2,Resin_processed$YEAR)
  
  means<-Resin_processed%>%
    pivot_longer(Nitrate:Phosphate)%>%
    group_by(Site,name)%>%
    summarise(M=mean(value, na.rm=T))
  sd<-Resin_processed%>%
    pivot_longer(Nitrate:Phosphate)%>%
    group_by(Site,name)%>%
    summarise(SD=sd(value, na.rm=T))%>%
    left_join(means)
  
  
  gg_NH4_Extract2 =
    Resin_processed %>%
    mutate(Purpose3=factor(Purpose3, levels=c("OW 2017","OW-GS 2018","GS 2018","OW 2019","GS 2019","OW-GS 2020","OW 2021")))%>%
    filter(Treatment %in% c("Snowfence","Control"))%>%
    ggplot(aes(x=Purpose3, y=Ammonium*100, fill=Site))+
    stat_summary(fun="mean",geom = "bar",size = 2, position= 'dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= 'dodge')+
    facet_wrap(~YEAR, scale="free_x",nrow=1)+
    scale_y_continuous(expand=c(0,0),limits=c(0,5),oob=rescale_none)+
    geom_text(data = Extract_resin_aov2 %>% filter(analyte == "Ammonium"), aes(y = 3.5, label = asterisk), size=10)+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_alpha_manual(values=c(0.25,1))+
    scale_x_discrete(breaks=c("OW 2017","OW-GS 2018","GS 2018","OW 2019","GS 2019","OW-GS 2020","OW 2021"),
                     labels=c(" W '16-'17", "GW '17- '18",  "G '18","W '18-'19","G '19","WG '19-'20","W '20-'21"))+
    labs(x = "W- Winter G- Growing season")+
    ylab(expression(atop('Ammonium', paste('('*mu*'g '*NH[4]^"+"~-N~cm^-2 ~ 100-Days^-1*')'))))+
    ggtitle("Ammonium")+
    theme_CKM()
  
  
  gg_NO3_Extract2 =
    Resin_processed %>%
    mutate(Purpose3=factor(Purpose3, levels=c("OW 2017","OW-GS 2018","GS 2018","OW 2019","GS 2019","OW-GS 2020","OW 2021")))%>%
    filter(Treatment %in% c("Snowfence","Control"))%>%
    ggplot(aes(x=Purpose3, y=Nitrate*100, fill=Site))+
    stat_summary(fun="mean",geom = "bar",size = 2, position= 'dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= 'dodge')+
    geom_text(data = Extract_resin_aov2 %>% filter(analyte == "Nitrate"), aes(y = 1, label = asterisk), size=10)+
    facet_wrap(~YEAR, scale="free_x",nrow=1)+
    scale_y_continuous(expand=c(0,0),limits=c(0,1.4),oob=rescale_none)+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_alpha_manual(values=c(0.25,1))+
    scale_x_discrete(breaks=c("OW 2017","OW-GS 2018","GS 2018","OW 2019","GS 2019","OW-GS 2020","OW 2021"),
                     labels=c(" W '16-'17", "GW '17- '18",  "G '18","W '18-'19","G '19","WG '19-'20","W '20-'21"))+
    labs(x = "W- Winter G- Growing season")+
    ylab(expression(atop('Nitrate', paste('('*mu*'g '*NO[3]^"-"~-N~cm^-2 ~ 100-Days^-1*')'))))+
    ggtitle("Nitrate")+
    theme_CKM()
  
  gg_PO4_Extract2 =
    Resin_processed %>%
    mutate(Purpose3=factor(Purpose3, levels=c("OW 2017","OW-GS 2018","GS 2018","OW 2019","GS 2019","OW-GS 2020","OW 2021")))%>%
    filter(Treatment %in% c("Snowfence","Control"))%>%
    ggplot(aes(x=Purpose3, y=Phosphate*100, fill=Site))+
    stat_summary(fun="mean",geom = "bar",size = 2, position= 'dodge') +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= 'dodge')+
    geom_text(data = Extract_resin_aov2 %>% filter(analyte == "Phosphate"), aes(y = 2.5, label = asterisk), size=10)+
    facet_wrap(~YEAR, scale="free_x",nrow=1)+
    scale_y_continuous(expand=c(0,0),limits=c(0,3.5),oob=rescale_none)+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_alpha_manual(values=c(0.25,1))+
    scale_x_discrete(breaks=c("OW 2017","OW-GS 2018","GS 2018","OW 2019","GS 2019","OW-GS 2020","OW 2021"),
                     labels=c(" W '16-'17", "GW '17- '18",  "G '18","W '18-'19","G '19","WG '19-'20","W '20-'21"))+
    labs(x = "W- Winter G- Growing season")+
    ylab(expression(atop('Phosphate', paste('('*mu*'g '*PO[4]^"3-"~-P~cm^-2 ~ 100-Days^-1*')'))))+
    ggtitle("Phosphate")+
    theme_CKM()
  
  
  
  
  
  
  COMB<-Combine_plots(gg_NH4_Extract2,gg_NO3_Extract2,gg_PO4_Extract2)
  
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
       Extract_resin_aov3=Extract_resin_aov3,
       COMB=COMB
  )
  
}

plot_inaccess =function(Extract_processed_Seasonal,PoreWater_processed_Seasonal){
  
  
  InaccessA<-Extract_processed_Seasonal %>%
    mutate(NH4I=((((NH4*(Dry.weight*5)/25)/14.0067)*1000)),NO3I=((((NO3*(Dry.weight*5)/25)/14.0067)*1000)),TFPAI=((TFPA*(Dry.weight*5)/(25+(5-(Dry.weight*5))))),
           NH4IW=((((NH4.H2O*(Dry.weight*5)/25)/14.0067)*1000)),NO3IW=((((NO3.H2O*(Dry.weight*5)/25)/14.0067)*1000)),TFPAIW=((TFPA.H2O*(Dry.weight*5)/(25+(5-(Dry.weight*5))))),
           NTE= NH4I+NO3I+TFPAI,NTW=NH4IW+NO3IW+TFPAIW, inacN=NTE-NTW)
  Inaccess<-Extract_processed_Seasonal %>%
    mutate(NH4I=((((NH4*(Dry.weight*5)/25)/14.0067)*1000)),NO3I=((((NO3*(Dry.weight*5)/25)/14.0067)*1000)),TFPAI=((TFPA*(Dry.weight*5)/(25+(5-(Dry.weight*5))))),
           NH4IW=((((NH4.H2O*(Dry.weight*5)/25)/14.0067)*1000)),NO3IW=((((NO3.H2O*(Dry.weight*5)/25)/14.0067)*1000)),TFPAIW=((TFPA.H2O*(Dry.weight*5)/(25+(5-(Dry.weight*5))))),
           NTE= NH4I+NO3I+TFPAI,NTW=NH4IW+NO3IW+TFPAIW, adsorb=NTE-NTW)%>%
    select(Date:Plot,NTE,NTW,adsorb)%>%
    pivot_longer(NTE:adsorb)%>%
    select(Date:Plot,name,value)
  
  Pore<-PoreWater_processed_Seasonal %>%
    mutate(NH4IP=(NH4/14.0067)*1000,NO3IP=(NO3/14.0067)*1000, TFPAIP=TFPA,
           NTP=NH4IP+NO3IP+TFPAIP)%>%
    pivot_longer(NTP)%>%
    select(Date:Plot,name,value)
  
  comb<- data.frame(rbind(Inaccess,Pore))%>%
    mutate(Date=as.Date(Date, "%m/%d/%Y"), name=as.factor(as.character(name)), value=as.numeric(value),Site=as.factor(as.character(Site)), DATE=as.Date(Date),
           YEAR=year(Date),
           MONTH=month(Date))
  
  
  
  Inaccess2<-Extract_processed_Seasonal %>%
    mutate(NH4I=((((NH4*(Dry.weight*5)/25)/14.0067)*1000)),NO3I=((((NO3*(Dry.weight*5)/25)/14.0067)*1000)),TFPAI=((TFPA*(Dry.weight*5)/(25+(5-(Dry.weight*5))))),
           NH4IW=((((NH4.H2O*(Dry.weight*5)/25)/14.0067)*1000)),NO3IW=((((NO3.H2O*(Dry.weight*5)/25)/14.0067)*1000)),TFPAIW=((TFPA.H2O*(Dry.weight*5)/(25+(5-(Dry.weight*5))))),
           NTE= NH4I+NO3I+TFPAI,NTW=NH4IW+NO3IW+TFPAIW, adsorb=NTE-NTW)%>%
    pivot_longer(NH4I:adsorb)%>%
    select(Date:Plot,name,value)
  
  
  
  Pore2<-PoreWater_processed_Seasonal %>%
    mutate(NH4IP=(NH4/14.0067)*1000,NO3IP=(NO3/14.0067)*1000, TFPAIP=TFPA,
           NTP=NH4IP+NO3IP+TFPAIP)%>%
    pivot_longer(NH4IP:NTP)%>%
    select(Date:Plot,name,value)
  
  
  
  comb2<- data.frame(rbind(Inaccess2,Pore2))%>%
    mutate(Date=as.Date(Date, "%m/%d/%Y"), name=as.factor(as.character(name)), value=as.numeric(value),Site=as.factor(as.character(Site)), DATE=as.Date(Date),
           YEAR=year(Date),
           MONTH=month(Date))%>%
    pivot_wider(names_from = name, values_from = value)
  
  N_counts<-comb2%>%
    filter(Site!="",!is.na(NTP))%>%
    select(Date,Site,NTP)%>%
    filter(Site!="")%>%
    group_by(Date,Site)%>%
    summarize(N_counts=n())
  
  
  
  
  gg_Pore<- comb2%>%
    filter(Date < as.Date("2019-08-16"))%>%
    filter(Site!="")%>%
    select(DATE,Site,NH4IP:TFPAIP)%>%
    pivot_longer(NH4IP:TFPAIP)%>%
    group_by(DATE, Site,name) %>%
    summarize(mean_value = mean(value, na.rm = TRUE),
              se= sqrt(var(value, na.rm = TRUE) / sum(!is.na(value)))) %>%
    ggplot(aes(x = DATE, y = mean_value, fill = name))+
    geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se, color = name), lwd = 1, width = 1) +
    geom_point(shape = 16,aes(color=name),lwd=6, show.legend =FALSE)+
    facet_wrap(~Site, ncol = 1, scales = "free_y")+
    scale_y_continuous(oob=rescale_none,expand = c(0, 0))+
    guides(color=guide_legend(title= "Legend"))+
    scale_color_manual(values = cbPalette4, labels= c("Ammonium", "Nitrate", "TFPA"))+
    scale_x_break(breaks=as.Date(c("2017-08-26","2018-06-03","2018-08-24","2019-06-06")))+
    scale_x_date(labels = date_format("%m-%y"))+
    theme_CKM()+
    theme(legend.position = "bottom")+
    labs(x = "Date", 
         y = bquote(''*mu*'Mol N (Ammonium, Nitrate, or TFPA)'))+
    ggtitle("Pore water")+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  Poreavg<- comb2%>%
    filter(Date < as.Date("2019-08-16"))%>%
    filter(Site!="")%>%
    select(DATE,Site,NH4IP:NTP)%>%
    pivot_longer(NH4IP:NTP)%>%
    group_by(name) %>%
    summarize(mean_value = mean(value, na.rm = TRUE),
              se= sqrt(var(value, na.rm = TRUE) / sum(!is.na(value))))
  
  17.4/23.7
  5.42/23.4
  0.747/23.7
  
  
  
  gg_water<- comb2%>%
    filter(Date < as.Date("2019-08-16"))%>%
    filter(Site!="")%>%
    select(DATE,Site,NH4IW:TFPAIW)%>%
    pivot_longer(NH4IW:TFPAIW)%>%
    group_by(DATE, Site,name) %>%
    summarize(mean_value = mean(value, na.rm = TRUE),
              se= sqrt(var(value, na.rm = TRUE) / sum(!is.na(value)))) %>%
    ggplot(aes(x = DATE, y = mean_value, fill = name))+
    geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se, color = name), lwd = 1, width = 1) +
    geom_point(shape = 16,aes(color=name),lwd=6, show.legend =FALSE)+
    facet_wrap(~Site, ncol = 1, scales="free_x")+
    scale_y_continuous(oob=rescale_none,expand = c(0, 0))+
    guides(color=guide_legend(title= "Legend"))+
    scale_color_manual(values = cbPalette4, labels= c("Ammonium", "Nitrate", "TFPA"))+
    scale_x_break(breaks=as.Date(c("2017-08-26","2018-06-03","2018-08-24","2019-06-06")))+
    scale_x_date(labels = date_format("%m-%y"))+
    theme_CKM()+
    theme(legend.position = "bottom")+
    labs(x = "Date", 
         y = bquote(''*mu*'Mol N (Ammonium, Nitrate, or TFPA)'))+
    ggtitle("Water extraction")+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  water2<- comb2%>%
    filter(Date < as.Date("2019-08-16"))%>%
    filter(Site!="")%>%
    select(DATE,Site,NH4IW:TFPAIW,NTW)%>%
    pivot_longer(NH4IW:NTW)%>%
    group_by(name) %>%
    summarize(mean_value = mean(value, na.rm = TRUE),
              se= sqrt(var(value, na.rm = TRUE) / sum(!is.na(value))))
  
  22.5/68.7
  44.2/68.7
  1.95/68.7
  
  gg_salt<-comb2%>%
    filter(Date < as.Date("2019-08-16"))%>%
    filter(Site!="")%>%
    select(DATE,Site,NH4I:TFPAI)%>%
    pivot_longer(NH4I:TFPAI)%>%
    group_by(DATE, Site,name) %>%
    summarize(mean_value = mean(value, na.rm = TRUE),
              se= sqrt(var(value, na.rm = TRUE) / sum(!is.na(value)))) %>%
    ggplot(aes(x = DATE, y = mean_value, fill = name))+
    geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se, color = name), lwd = 1, width = 1) +
    geom_point(shape = 16,aes(color=name),lwd=6, show.legend =FALSE)+
    facet_wrap(~Site, ncol = 1, scales="free_x")+
    scale_y_continuous(oob=rescale_none,expand = c(0, 0))+
    guides(color=guide_legend(title= "Legend"))+
    scale_color_manual(values = cbPalette4, labels= c("Ammonium", "Nitrate", "TFPA"))+
    scale_x_break(breaks=as.Date(c("2017-08-26","2018-06-03","2018-08-24","2019-06-06")))+
    scale_x_date(labels = date_format("%m-%y"))+
    theme_CKM()+
    theme(legend.position = "bottom")+
    labs(x = "Date", 
         y = bquote(''*mu*'Mol N (Ammonium, Nitrate, or TFPA)'))+
    ggtitle("Potassium sulfate extraction")+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  salt2<-comb2%>%
    filter(Date < as.Date("2019-08-16"))%>%
    filter(Site!="")%>%
    select(DATE,Site,NH4I:TFPAI,NTE)%>%
    pivot_longer(NH4I:NTE)%>%
    group_by(name) %>%
    summarize(mean_value = mean(value, na.rm = TRUE),
              se= sqrt(var(value, na.rm = TRUE) / sum(!is.na(value))))
  
  103/161
  39.7/161
  21.0/161
 
  
  
  Inaccess2<- comb %>%
    group_by(Date, Site,treatment, name) %>%
    summarize(mean_value = mean(value, na.rm = TRUE),
              se= sqrt(var(value, na.rm = TRUE) / sum(!is.na(value)))) %>%
    filter(Site != "")
  
  
  InaccessA2<- InaccessA %>%
    mutate(Date=as.Date(Date, "%m/%d/%Y")) %>%
    group_by(Date, Site, treatment) %>%
    summarize(mean_value = mean(inacN, na.rm = TRUE),
              se= sqrt(var(inacN, na.rm = TRUE) / sum(!is.na(inacN)))) %>%
    filter(Site != "")
  
  Table<- Inaccess2 %>%
    pivot_wider(names_from = Site,
                values_from = c(mean_value,se))
  Table_E<-Table%>%
    filter(name=="NTE")
  Table_W<-Table%>%
    filter(name=="NTW")
  Table_P<-Table%>%
    filter(name=="NTP")
  
  min(Table_E$mean_value_Xeric, na.rm = T)
  max(Table_E$mean_value_Xeric, na.rm = T)
  min(Table_E$mean_value_Mesic, na.rm = T)
  max(Table_E$mean_value_Mesic, na.rm = T)
  min(Table_E$mean_value_Hydric, na.rm = T)
  max(Table_E$mean_value_Hydric, na.rm = T)
  
  min(Table_W$mean_value_Xeric, na.rm = T)
  max(Table_W$mean_value_Xeric, na.rm = T)
  min(Table_W$mean_value_Mesic, na.rm = T)
  max(Table_W$mean_value_Mesic, na.rm = T)
  min(Table_W$mean_value_Hydric, na.rm = T)
  max(Table_W$mean_value_Hydric, na.rm = T)
  
  min(Table_P$mean_value_Xeric, na.rm = T)
  max(Table_P$mean_value_Xeric, na.rm = T)
  min(Table_P$mean_value_Mesic, na.rm = T)
  max(Table_P$mean_value_Mesic, na.rm = T)
  min(Table_P$mean_value_Hydric, na.rm = T)
  max(Table_P$mean_value_Hydric, na.rm = T)
  
  
  
  
  fit_aov = function(Extract_processed){
    
    a = aov(value ~ MONTH * YEAR * Site,  data = Extract_processed)
    broom::tidy(a) %>% 
      mutate(asterisk = case_when(`p.value` <= 0.05 ~ "*"))
    
  }
  
  Extraction_Porewater_ANOVA = 
    comb %>% 
    group_by(name)%>%
    do(fit_aov(.))%>%
    na.omit()%>%
    knitr::kable()
  
  
  
  
  
  
  
  
  Count<-Inaccess2%>%
    group_by(Date,Site,treatment,name)%>%
    summarise(C=n())
  
  
  
  
  inaccessible_N<-ggplot(Inaccess2%>% filter(name != "adsorb"), aes(x = Date, y = mean_value, fill = name)) +
    geom_area(aes(fill=name),position = 'identity', alpha = 0.4) +
    geom_line(aes(color = name), lwd = 2) +
    geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se, color = name), lwd = 1, width = 1) +
    geom_point(shape = 16,aes(color=name),lwd=6)+
    facet_wrap(~Site, nrow = 3)+
    scale_y_continuous(limits=c(0,350),oob=rescale_none,expand = c(0, 0))+
    guides(fill=guide_legend(title= "Legend"))+
    scale_x_break(breaks=as.Date(c("2017-08-26","2018-06-03","2018-08-24","2019-06-06","2019-08-16","2020-08-30","2020-09-06","2021-06-04")))+
    scale_x_date(labels = date_format("%m-%Y"))+
    scale_color_manual(values = cbPalette3, labels= c("Salt extract", "pore water", "Water extract"))+
    scale_fill_manual(name="name",
                      breaks=c("NTE", "NTP", "NTW"),
                      labels=c("Adsorbed", "Mobile", "Inaccessible"),values = cbPalette3)+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank())
  
  
  
  
  inaccessible_N_only<-ggplot(InaccessA2, aes(x = Date, y = mean_value)) +
    geom_line( lwd = 2) +
    geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), lwd = 1, width = 1) +
    geom_point(shape = 16)+
    facet_wrap(~Site, nrow = 3)+
    scale_y_continuous(limits=c(0,350),oob=rescale_none,expand = c(0, 0))+
    guides(fill=guide_legend(title= "Legend"))+
    scale_x_break(breaks=as.Date(c("2017-08-26","2018-06-03","2018-08-24","2019-06-06","2019-08-16","2020-08-30","2020-09-06","2021-06-04")))+
    scale_x_date(labels = date_format("%m-%Y"))+
    scale_color_manual(values = cbPalette3)+
    scale_fill_manual(values = cbPalette3)+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank())
  
  Inaccess2S<-Inaccess2%>%
    filter(Date < as.Date("2019-08-16"))
  
  inaccessible_N2<-ggplot(Inaccess2S%>% filter(name != "adsorb"), aes(x = Date, y = mean_value, fill = name)) +
    geom_area(aes(fill=name),position = 'identity', alpha = 0.4) +
    geom_line(aes(color = name), lwd = 2) +
    geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se, color = name), lwd = 1, width = 1) +
    geom_point(shape = 16,aes(color=name),lwd=6)+
    facet_wrap(~Site, nrow = 3)+
    scale_y_continuous(limits=c(0,350),oob=rescale_none,expand = c(0, 0))+
    guides(fill=guide_legend(title= "Legend"))+
    scale_x_break(breaks=as.Date(c("2017-08-26","2018-06-03","2018-08-24","2019-06-06")))+
    scale_x_date(date_breaks = "months" ,labels = date_format("%m-'%y"))+
    scale_color_manual(values = cbPalette3, labels= c("Salt extract", "pore water", "Water extract"))+
    scale_fill_manual(name="name",
                      breaks=c("NTE", "NTP", "NTW"),
                      labels=c("Adsorbed", "Mobile", "Inaccessible"),values = cbPalette3)+
    ylab(bquote(''*NH[4]^'+' *+ NO[3]^'-'~+ TFPA ~(μMol ~N) ))+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank())
  
  
  
  list(inaccessible_N=inaccessible_N,
       inaccessible_N_only=inaccessible_N_only,
       inaccessible_N2=inaccessible_N2,
       gg_Pore=gg_Pore,
       gg_water=gg_water,
       gg_salt=gg_salt
       
  )
  
  
  
  
}

plot_inaccess_p =function(Extract_processed_Seasonal,PoreWater_processed_Seasonal){
  
  
  InaccessA<-Extract_processed_Seasonal %>%
    mutate(PO4I=((((PO4*(Dry.weight*5)/25)/30.973762)*1000)),
           PO4IW=((((PO4.H2O*(Dry.weight*5)/25)/30.973762)*1000)))%>%
    select(Date:Plot,PO4I,PO4IW)%>%
    pivot_longer(PO4I:PO4IW)%>%
    select(Date:Plot,name,value)
  
  Pore<-PoreWater_processed_Seasonal %>%
    mutate(PO4IP=(PO4/30.973762)*1000)%>%
    pivot_longer(PO4IP)%>%
    select(Date:Plot,name,value)
  
  comb<- data.frame(rbind(InaccessA,Pore))%>%
    mutate(Date=as.Date(Date, "%m/%d/%Y"), name=as.factor(as.character(name)), value=as.numeric(value),Site=as.factor(as.character(Site)))
  
  
  
  P_counts<-comb%>%
    filter(Site!="", name=="PO4IP",!is.na(value))%>%
    select(Date,Site,value)%>%
    group_by(Date,Site)%>%
    summarize(P_counts=n())
  
 # Total_counts<- N_counts%>%
  #  full_join(P_counts)
  
  #write.csv(Total_counts, "Graphs/Total_lysim_counts.csv")
  
  Inaccess2<- comb %>%
    group_by(Date, Site,treatment, name) %>%
    summarize(mean_value = mean(value, na.rm = TRUE),
              se= sqrt(var(value, na.rm = TRUE) / sum(!is.na(value)))) %>%
    filter(Site != "")
  
  Overmean<-comb%>%
    group_by(name,Site)%>%
    summarize(mean_value = mean(value, na.rm = TRUE),
              se= sqrt(var(value, na.rm = TRUE) / sum(!is.na(value)))) %>%
    filter(Site != "")
  
  Overmean2<-comb%>%
    group_by(name)%>%
    summarize(mean_value = mean(value, na.rm = TRUE),
              se= sqrt(var(value, na.rm = TRUE) / sum(!is.na(value))))
  
  
  inaccessible_N<-ggplot(Inaccess2, aes(x = Date, y = mean_value, fill = name)) +
    geom_area(aes(fill=name),position = 'identity', alpha = 0.4) +
    geom_line(aes(color = name), lwd = 2) +
    geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se, color = name), lwd = 1, width = 1) +
    geom_point(shape = 16,aes(color=name),lwd=6)+
    facet_wrap(~Site, nrow = 3)+
    scale_y_continuous(limits=c(0,50),oob=rescale_none,expand = c(0, 0))+
    guides(fill=guide_legend(title= "Legend"))+
    scale_x_break(breaks=as.Date(c("2017-08-26","2018-06-03","2018-08-24","2019-06-06","2019-08-16","2020-08-30","2020-09-06","2021-06-04")))+
    scale_x_date(labels = date_format("%m-%Y"))+
    scale_color_manual(values = cbPalette3)+
    scale_fill_manual(name="name",
                      breaks=c("NTE", "NTP", "NTW"),
                      labels=c("Adsorbed", "Mobile", "Inaccessible"),values = cbPalette3)+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank())
  
  

  Inaccess2S<-Inaccess2%>%
    filter(Date < as.Date("2019-08-16"))
  
  inaccessible_N2<-ggplot(Inaccess2S, aes(x = Date, y = mean_value, fill = name)) +
    geom_area(aes(fill=name),position = 'identity', alpha = 0.4) +
    geom_line(aes(color = name), lwd = 2) +
    geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se, color = name), lwd = 1, width = 1) +
    geom_point(shape = 16,aes(color=name),lwd=6)+
    facet_wrap(~Site, nrow = 3)+
    scale_y_continuous(limits=c(0,30),oob=rescale_none,expand = c(0, 0))+
    guides(fill=guide_legend(title= "Legend"))+
    scale_x_break(breaks=as.Date(c("2017-08-26","2018-06-03","2018-08-24","2019-06-06")))+
    scale_x_date(labels = date_format("%m"))+
    scale_color_manual(values = cbPalette3, labels= c("Salt extract", "pore water", "Water extract"))+
    scale_fill_manual(name="name",
                      breaks=c("NTE", "NTP", "NTW"),
                      labels=c("Adsorbed", "Mobile", "Inaccessible"),values = cbPalette3)+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank())+
    ylab(bquote('Phosphate ('*mu*'Mol '*PO[4]^"3-"~-P*')'))+
    xlab("Month")
  
  
  list(inaccessible_N=inaccessible_N,
       inaccessible_N2=inaccessible_N2
       
  )
  
  
  
  
}

plot_inaccess_TRS =function(Extract_processed_Seasonal,PoreWater_processed_Seasonal){
  
  
  InaccessA<-Extract_processed_Seasonal %>%
    mutate(TRS.H2O=as.numeric(TRS.H2O)) %>%
    mutate(TRSI=((((TRS*(Dry.weight*5)/25)))),
           TRSIW=((((TRS.H2O*(Dry.weight*5)/25)))))%>%
    select(Date:Plot,TRSI,TRSIW)%>%
    pivot_longer(TRSI:TRSIW)%>%
    select(Date:Plot,name,value)
  
  Pore<-PoreWater_processed_Seasonal %>%
    mutate(TRSIP=(TRS))%>%
    pivot_longer(TRSIP)%>%
    select(Date:Plot,name,value)
  
  comb<- data.frame(rbind(InaccessA,Pore))%>%
    mutate(Date=as.Date(Date, "%m/%d/%Y"), name=as.factor(as.character(name)), value=as.numeric(value),Site=as.factor(as.character(Site)))
  
  
  Inaccess2<- comb %>%
    group_by(Date, Site,treatment, name) %>%
    summarize(mean_value = mean(value, na.rm = TRUE),
              se= sqrt(var(value, na.rm = TRUE) / sum(!is.na(value)))) %>%
    filter(Site != "")
  
  
  
  
  
  
  inaccessible_N<-ggplot(Inaccess2, aes(x = Date, y = mean_value, fill = name)) +
    geom_area(aes(fill=name),position = 'identity', alpha = 0.4) +
    geom_line(aes(color = name), lwd = 2) +
    geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se, color = name), lwd = 1, width = 1) +
    geom_point(shape = 16,aes(color=name),lwd=6)+
    facet_wrap(~Site, nrow = 3)+
    scale_y_continuous(limits=c(0,.5),oob=rescale_none,expand = c(0, 0))+
    guides(fill=guide_legend(title= "Legend"))+
    scale_x_break(breaks=as.Date(c("2017-08-26","2018-06-03","2018-08-24","2019-06-06","2019-08-16","2020-08-30","2020-09-06","2021-06-04")))+
    scale_x_date(labels = date_format("%m-%Y"))+
    scale_color_manual(values = cbPalette3)+
    scale_fill_manual(name="name",
                      breaks=c("TRSI", "TRSIP", "TRSIW"),
                      labels=c("Adsorbed", "Mobile", "Inaccessible"),values = cbPalette3)+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank())
  
  
  
  Inaccess2S<-Inaccess2%>%
    filter(Date < as.Date("2019-08-16"))
  
  inaccessible_TRS2_point5<-ggplot(Inaccess2S, aes(x = as.Date(Date), y = mean_value, fill = name)) +
    geom_area(aes(fill=name),position = 'identity', alpha = 0.4) +
    geom_line(aes(color = name), lwd = 2) +
    geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se, color = name), lwd = 1, width = 1) +
    geom_point(shape = 16,aes(color=name),lwd=6)+
    facet_wrap(~Site, nrow = 3)+
    scale_y_continuous(limits=c(0,.5),oob=rescale_none,expand = c(0, 0))+
    guides(fill=guide_legend(title= "Legend"))+
    scale_x_break(breaks=as.Date(c("2017-08-26","2018-06-03","2018-08-24","2019-06-06")))+
    scale_x_date(labels = date_format("%m-%Y"))+
    scale_color_manual(values = cbPalette3)+
    scale_fill_manual(name="name",
                      breaks=c("TRSI", "TRSIP", "TRSIW"),
                      labels=c("Adsorbed", "Mobile", "Inaccessible"),values = cbPalette3)+
    theme_CKM()
  inaccessible_TRS2_30<-ggplot(Inaccess2S, aes(x = as.Date(Date), y = mean_value, fill = name)) +
    geom_area(aes(fill=name),position = 'identity', alpha = 0.4) +
    geom_line(aes(color = name), lwd = 2) +
    geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se, color = name), lwd = 1, width = 1) +
    geom_point(shape = 16,aes(color=name),lwd=6)+
    facet_wrap(~Site, nrow = 3)+
    scale_y_continuous(limits=c(0,30),oob=rescale_none,expand = c(0, 0))+
    guides(fill=guide_legend(title= "Legend"))+
    scale_x_break(breaks=as.Date(c("2017-08-26","2018-06-03","2018-08-24","2019-06-06")))+
    scale_x_date(labels = date_format("%m-%Y"))+
    scale_color_manual(values = cbPalette3)+
    scale_fill_manual(name="name",
                      breaks=c("TRSI", "TRSIP", "TRSIW"),
                      labels=c("Adsorbed", "Mobile", "Inaccessible"),values = cbPalette3)+
    theme_CKM()
  
  list(inaccessible_TRS2_point5=inaccessible_TRS2_point5,
       inaccessible_TRS2_30=inaccessible_TRS2_30
       
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
  
  
  
  gg_NH4_Extract2 =
    Extract_processed_Seasonal %>%
    ggplot(aes(x=DATE, y=NH4, color=Site))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2,
                 aes(group = interaction(Site,DATE)),
                 width=0.8)+
    geom_point( size = 1.4)+
    #facet_wrap(~Site, scale="free_x",nrow=3)+
    theme_light()+
    scale_y_continuous(limits=c(0,200),oob=rescale_none,expand = c(0, 0))+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank())+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_x_break(breaks=as.Date(c("2017-08-26","2018-06-03","2018-08-24","2019-06-06","2019-08-16","2020-08-30","2020-09-06","2021-06-04")))+
    scale_x_date(labels = date_format("%m-%Y"))+
    labs(x = "Date", 
         y = bquote('Ammonium ('*mu*'g '*NH[4]^"+"~-N~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Ammonium")
  
  
  gg_NO3_Extract2 =
    Extract_processed_Seasonal %>%
    ggplot(aes(x=DATE, y=NO3, color=Site))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2,
                 aes(group = interaction(Site,DATE)),
                 width=0.8)+
    geom_point( size = 1.4)+
    #facet_wrap(~Site, scale="free_x",nrow=3)+
    theme_light()+
    scale_y_continuous(limits=c(0,100),oob=rescale_none,expand = c(0, 0))+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank())+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_x_break(breaks=as.Date(c("2017-08-26","2018-06-03","2018-08-24","2019-06-06","2019-08-16","2020-08-30","2020-09-06","2021-06-04")))+
    scale_x_date(labels = date_format("%m-%Y"))+
    labs(x = "Date", 
         y = bquote('Nitrate ('*mu*'g '*NO[3]^"-"~-N~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Nitrate")
  
  gg_PO4_Extract2 =
    Extract_processed_Seasonal %>%
    ggplot(aes(x=DATE, y=PO4, color=Site))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2,
                 aes(group = interaction(Site,DATE)),
                 width=0.8)+
    geom_point( size = 1.4)+
    #facet_wrap(~Site, scale="free_x",nrow=3)+
    theme_light()+
    scale_y_continuous(limits=c(0,75),oob=rescale_none,expand = c(0, 0))+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank())+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_x_break(breaks=as.Date(c("2017-08-26","2018-06-03","2018-08-24","2019-06-06","2019-08-16","2020-08-30","2020-09-06","2021-06-04")))+
    scale_x_date(labels = date_format("%m-%Y"))+
    labs(x = "Date", 
         y = bquote('Phosphate ('*mu*'g '*PO[4]^"3-"~-P~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Phosphate")
  
  gg_TRS_Extract2 =
    Extract_processed_Seasonal %>%
    ggplot(aes(x=DATE, y=TRS, color=Site))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2,
                 aes(group = interaction(Site,DATE)),
                 width=0.8)+
    geom_point( size = 1.4)+
    #facet_wrap(~Site, scale="free_x",nrow=3)+
    theme_light()+
    scale_y_continuous(limits=c(0,50),oob=rescale_none,expand = c(0, 0))+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank())+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_x_break(breaks=as.Date(c("2017-08-26","2018-06-03","2018-08-24","2019-06-06","2019-08-16","2020-08-30","2020-09-06","2021-06-04")))+
    scale_x_date(labels = date_format("%m-%Y"))+
    labs(x = "Date", 
         y = bquote('Total reducing sugars-glucose equiv. ('*mu*'g' ~g^-1 ~ dry ~ soil*')'))+
    ggtitle("TRS")
  
  gg_phenolics_Extract2 =
    Extract_processed_Seasonal %>%
    ggplot(aes(x=DATE, y=phenolics, color=Site))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2,
                 aes(group = interaction(Site,DATE)),
                 width=0.8)+
    geom_point( size = 1.4)+
    #facet_wrap(~Site, scale="free_x",nrow=3)+
    theme_light()+
    scale_y_continuous(limits=c(0,200),oob=rescale_none,expand = c(0, 0))+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank())+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_x_break(breaks=as.Date(c("2017-08-26","2018-06-03","2018-08-24","2019-06-06","2019-08-16","2020-08-30","2020-09-06","2021-06-04")))+
    scale_x_date(labels = date_format("%m-%Y"))+
    labs(x = "Date", 
         y = bquote('Phenolics-gallic acid equiv. ('*mu*'g' ~g^-1 ~ dry ~ soil*')'))+
    ggtitle("phenolics")
  
  gg_TFPA_Extract2 =
    Extract_processed_Seasonal %>%
    ggplot(aes(x=DATE, y=TFPA, color=Site))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2,
                 aes(group = interaction(Site,DATE)),
                 width=0.8)+
    geom_point( size = 1.4)+
    #facet_wrap(~Site, scale="free_x",nrow=3)+
    theme_light()+
    scale_y_continuous(limits=c(0,2000),oob=rescale_none,expand = c(0, 0))+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank())+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_x_break(breaks=as.Date(c("2017-08-26","2018-06-03","2018-08-24","2019-06-06","2019-08-16","2020-08-30","2020-09-06","2021-06-04")))+
    scale_x_date(labels = date_format("%m-%Y"))+
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
  
  
  
  gg_MBC_Extract2 =
    Extract_processed_Seasonal %>%
    #filter(YEAR %in% c(2017,2018,2019))%>%
    ggplot(aes(x=DATE, y=MBC, color=Site))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2,
                 aes(group = interaction(Site,DATE)),
                 width=3)+
    geom_point( size = 2)+
    #facet_wrap(~Site, scale="free",ncol=1)+
    theme_light()+
    scale_y_continuous(limits=c(0,25000),oob=rescale_none,expand = c(0, 0))+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank())+
    scale_color_manual(name="Site",
                       values = c("#009E73", "#56B4E9", "#E69F00" ),
                      breaks=c("#009E73", "#56B4E9", "#E69F00" ),
                      labels=c("East dry", "East wet", "West wet"))+
    scale_fill_manual(values=cbPalette2)+
    scale_x_break(breaks=as.Date(c("2017-08-26","2018-06-03","2018-08-24","2019-06-06","2019-08-16","2020-08-30","2020-09-06","2021-06-04")))+
    scale_x_date(labels = date_format("%m-%Y"))+
    labs(x = "Date", 
         y = bquote('Microbial biomass ('*mu*'g C'~g^-1 ~ dry ~ soil*')'))+
    ggtitle("MBC")
  
  
  
  gg_MBN_Extract2 =
    Extract_processed_Seasonal %>%
    #filter(YEAR %in% c(2017,2018,2019))%>%
    ggplot(aes(x=DATE, y=MBN, color=Site))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2,
                 aes(group = interaction(Site,DATE)),
                 width=3)+
    geom_point( size = 2)+
    #facet_wrap(~Site, scale="free",ncol=1)+
    theme_light()+
    scale_y_continuous(limits=c(0,2500),oob=rescale_none,expand = c(0, 0))+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank())+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_x_break(breaks=as.Date(c("2017-08-26","2018-06-03","2018-08-24","2019-06-06","2019-08-16","2020-08-30","2020-09-06","2021-06-04")))+
    scale_x_date(labels = date_format("%m-%Y"))+
    labs(x = "Date", 
         y = bquote('Microbial biomass ('*mu*'g N'~g^-1 ~ dry ~ soil*')'))+
    ggtitle("MBN")
  
  
  
  gg_MBP_Extract2 =
    Extract_processed_Seasonal %>%
    #filter(YEAR %in% c(2017,2018,2019))%>%
    ggplot(aes(x=DATE, y=Mic.PO4, color=Site))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2,
                 aes(group = interaction(Site,DATE)),
                 width=3)+
    geom_point( size = 2)+
    #facet_wrap(~Site, scale="free",ncol=1)+
    theme_light()+
    scale_y_continuous(limits=c(0,300),oob=rescale_none,expand = c(0, 0))+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank())+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_x_break(breaks=as.Date(c("2017-08-26","2018-06-03","2018-08-24","2019-06-06","2019-08-16","2020-08-30","2020-09-06","2021-06-04")))+
    scale_x_date(labels = date_format("%m-%Y"))+
    labs(x = "Date", 
         y = bquote('Microbial biomass ('*mu*'g P'~g^-1 ~ dry ~ soil*')'))+
    ggtitle("MBP")
  
  Extract_processed_S = Extract_processed_Seasonal %>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE))%>%
    filter(Site %in% c("East wet","West wet","East dry")) %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','TRS','MBC','MBN','Mic.PO4'), as.numeric)
  
  Extract_processed_S_long = Extract_processed_S %>%
    pivot_longer(cols= NH4:TRS,
                 names_to= "analyte",
                 values_to= "conc") 
  
  
  Extract_processed_S_long_biomass = Extract_processed_S %>%
    pivot_longer(cols= MBC:Mic.PO4,
                 names_to= "analyte",
                 values_to= "conc") %>%
    filter(analyte %in% c('MBC','MBN','Mic.PO4'))
  
  
  Fit.LME=function(Extract_processed_S_long){
    a = nlme::lme(conc ~ MONTH * YEAR * Site ,
                  random = ~1|Plot,
                  data = Extract_processed_S_long,na.action=na.exclude)%>%
      anova()
    
    a %>% 
      as.data.frame() %>% 
      rownames_to_column("variable") %>% 
      filter(variable %in% c("MONTH","YEAR","Site","MONTH:YEAR","MONTH:Site","YEAR:Site","MONTH:YEAR:Site")) %>%
      dplyr::rename(p_value = 'p-value') %>%
      mutate(p_value == round(p_value, 3),asterisk = case_when(`p_value` <= 0.05 ~ "*"))
  }
  
  
  Ancillary_LME = 
    Extract_processed_S_long %>% 
    group_by(analyte)%>%
    do(Fit.LME(.))%>%
    na.omit()%>%
    knitr::kable()
  
  Ancillary_LME2 = 
    Extract_processed_S_long %>% 
    group_by(analyte)%>%
    do(Fit.LME(.))%>%
    knitr::kable()
  
  
  
  fit_aov = function(Extract_processed){
    
    a = aov(conc ~ MONTH * YEAR * Site,  data = Extract_processed)
    broom::tidy(a) %>% 
      mutate(asterisk = case_when(`p.value` <= 0.05 ~ "*"))
    
  }
  
  Ancillary_ANOVA2 = 
    Extract_processed_S_long %>% 
    group_by(analyte)%>%
    do(fit_aov(.))%>%
    na.omit()%>%
    knitr::kable()
  
  Ancillary_ANOVA = 
    Extract_processed_S_long %>% 
    group_by(analyte)%>%
    do(fit_aov(.))%>%
    knitr::kable()
  
  Ancillary_ANOVA_biomass = 
    Extract_processed_S_long_biomass %>% 
    group_by(analyte)%>%
    do(fit_aov(.))%>%
    knitr::kable(caption = "Biomass ANOVA significant comparisons")
  
  Ancillary_LME_biomass = 
    Extract_processed_S_long_biomass %>% 
    group_by(analyte)%>%
    do(Fit.LME(.))%>%
    na.omit()%>%
    knitr::kable(caption = "Biomass LME significant comparisons")
  
  Ancillary_LME2_biomass = 
    Extract_processed_S_long_biomass %>% 
    group_by(analyte)%>%
    do(Fit.LME(.))%>%
    knitr::kable(caption = "Biomass LME all comparisons")
  
  
  
  list("Seasonal NH4"= gg_NH4_Extract,
       "Seasonal NO3"= gg_NO3_Extract,
       "Seasonal PO4"= gg_PO4_Extract,
       "Seasonal TFPA"= gg_TFPA_Extract,
       "Seasonal Phenolics"= gg_phenolics_Extract,
       "Seasonal TRS"= gg_TRS_Extract,
       "Seasonal MBC"= gg_MBC_Extract,
       "Seasonal MBN"= gg_MBN_Extract,
       "Seasonal MBP"= gg_MBP_Extract,
       gg_NH4_Extract2=gg_NH4_Extract2,
       gg_NO3_Extract2=gg_NO3_Extract2,
       gg_PO4_Extract2=gg_PO4_Extract2,
       gg_TFPA_Extract2=gg_TFPA_Extract2,
       gg_phenolics_Extract2=gg_phenolics_Extract2,
       gg_TRS_Extract2=gg_TRS_Extract2,
       gg_MBC_Extract2=gg_MBC_Extract2,
       gg_MBN_Extract2=gg_MBN_Extract2,
       gg_MBP_Extract2=gg_MBP_Extract2,
       Ancillary_LME=Ancillary_LME,
       Ancillary_LME2=Ancillary_LME2,
       Ancillary_LME_biomass=Ancillary_LME_biomass,
       Ancillary_LME2_biomass=Ancillary_LME2_biomass,
       Ancillary_ANOVA= Ancillary_ANOVA,
       Ancillary_ANOVA_biomass=Ancillary_ANOVA_biomass
       
       
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
  
  
  Extract_processed_H2O_long = Extract_processed_Seasonal_H2O %>%
    pivot_longer(cols= NH4.H2O:TRS.H2O,
                 names_to= "analyte",
                 values_to= "conc") 
  
  fit_aov = function(Extract_processed){
    
    a = aov(conc ~ MONTH * YEAR * Site,  data = Extract_processed)
    broom::tidy(a) %>% 
      mutate(asterisk = case_when(`p.value` <= 0.05 ~ "*"))
    
  }
  
  Fit.LME=function(Extract_processed_S_long){
    a = nlme::lme(conc ~ MONTH * YEAR * Site ,
                  random = ~1|Plot,
                  data = Extract_processed_S_long,na.action=na.exclude)%>%
      anova()
    
    a %>% 
      as.data.frame() %>% 
      rownames_to_column("variable") %>% 
      filter(variable %in% c("MONTH","YEAR","Site","MONTH:YEAR","MONTH:Site","YEAR:Site","MONTH:YEAR:Site")) %>%
      dplyr::rename(p_value = 'p-value') %>%
      mutate(p_value == round(p_value, 3),asterisk = case_when(`p_value` <= 0.05 ~ "*"))
  }
  
  
  Ancillary_H2O_ANOVA = 
    Extract_processed_H2O_long %>% 
    group_by(analyte)%>%
    do(fit_aov(.))%>%
    na.omit()%>%
    knitr::kable()
  
  Ancillary_H2O_ANOVA2 = 
    Extract_processed_H2O_long %>% 
    group_by(analyte)%>%
    do(fit_aov(.))%>%
    knitr::kable()
  
  
  
  
  
  
  
  
  
  
  
  
  list("Seasonal_H2O NH4"= gg_NH4_Extract,
       "Seasonal_H2O NO3"= gg_NO3_Extract,
       "Seasonal_H2O PO4"= gg_PO4_Extract,
       "Seasonal_H2O TFPA"= gg_TFPA_Extract,
       "Seasonal_H2O TRS"= gg_TRS_Extract,
       Ancillary_H2O_ANOVA=Ancillary_H2O_ANOVA,
       Ancillary_H2O_ANOVA2=Ancillary_H2O_ANOVA2
  )
  
}

plot_PoreWater_Seasonal = function(PoreWater_processed_Seasonal){
  
  
 
  
  
  
  
  gg_NH4_PoreWater =
    PoreWater_processed_Seasonal %>%
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
  
  
  
  gg_NH4_PoreWater2 =
    PoreWater_processed_Seasonal %>%
    #filter(Site!="",YEAR %in% c(2017,2018,2019),treatment=="Ancillary")%>%
    ggplot(aes(x=DATE, y=NH4, color=Site))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    #facet_wrap(~Site,nrow=3)+
    theme_light()+
    scale_y_continuous(limits=c(0,4),oob=rescale_none,expand = c(0, 0))+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank())+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_x_break(breaks=as.Date(c("2017-08-26","2018-06-03","2018-08-24","2019-06-06","2019-08-16","2020-09-05","2020-09-09","2021-06-04")))+
    scale_x_date(labels = date_format("%m-%y"))+
    labs(x = "Date", 
         y = bquote('Ammonium ('*mu*'g '*NH[4]^"+"~-N~mL^-1 ~ Soil ~ Solution*')'))+
    ggtitle("Ammonium")
  
  gg_NO3_PoreWater2 =
    PoreWater_processed_Seasonal %>%
    #filter(Site!="",YEAR %in% c(2017,2018,2019),treatment=="Ancillary")%>%
    ggplot(aes(x=DATE, y=NO3, color=Site))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    #facet_wrap(~Site,nrow=3)+
    theme_light()+
    scale_y_continuous(limits=c(0,1.5),oob=rescale_none,expand = c(0, 0))+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank())+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_x_break(breaks=as.Date(c("2017-08-26","2018-06-03","2018-08-24","2019-06-06","2019-08-16","2020-09-05","2020-09-09","2021-06-04")))+
    scale_x_date(labels = date_format("%m-%y"))+
    labs(x = "Date", 
         y = bquote('Nitrate ('*mu*'g '*NO[3]^"-"~-N~mL^-1 ~ Soil ~ Solution*')'))+
    ggtitle("Nitrate")
  gg_PO4_PoreWater2 =
    PoreWater_processed_Seasonal %>%
    #filter(Site!="",YEAR %in% c(2017,2018,2019),treatment=="Ancillary")%>%
    ggplot(aes(x=DATE, y=PO4, color=Site))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    #facet_wrap(~Site,nrow=3)+
    theme_light()+
    scale_y_continuous(limits=c(0,0.9),oob=rescale_none,expand = c(0, 0))+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank())+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_x_break(breaks=as.Date(c("2017-08-26","2018-06-03","2018-08-24","2019-06-06","2019-08-16","2020-09-05","2020-09-09","2021-06-04")))+
    scale_x_date(labels = date_format("%m-%y"))+
    labs(x = "Date", 
         y = bquote('Phosphate ('*mu*'g '*PO[4]^"3-"~-P~mL^-1 ~ Soil ~ Solution*')'))+
    ggtitle("Phosphate")
  gg_TRS_PoreWater2 =
    PoreWater_processed_Seasonal %>%
    #filter(Site!="",YEAR %in% c(2017,2018,2019),treatment=="Ancillary")%>%
    ggplot(aes(x=DATE, y=TRS, color=Site))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    #facet_wrap(~Site,nrow=3)+
    theme_light()+
    scale_y_continuous(limits=c(0,0.7),oob=rescale_none,expand = c(0, 0))+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank())+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_x_break(breaks=as.Date(c("2017-08-26","2018-06-03","2018-08-24","2019-06-06","2019-08-16","2020-09-05","2020-09-09","2021-06-04")))+
    scale_x_date(labels = date_format("%m-%y"))+
    labs(x = "Date", 
         y = bquote('Total reducing sugars-glucose equiv. ('*mu*'g' ~mL^-1 ~ Soil ~ Solution*')'))+
    ggtitle("TRS")
  gg_TFPA_PoreWater2 =
    PoreWater_processed_Seasonal %>%
    #filter(Site!="",YEAR %in% c(2017,2018,2019),treatment=="Ancillary")%>%
    ggplot(aes(x=DATE, y=TFPA, color=Site))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    #facet_wrap(~Site,nrow=3)+
    theme_light()+
    scale_y_continuous(limits=c(0,10),oob=rescale_none,expand = c(0, 0))+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank())+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_x_break(breaks=as.Date(c("2017-08-26","2018-06-03","2018-08-24","2019-06-06","2019-08-16","2020-09-05","2020-09-09","2021-06-04")))+
    scale_x_date(labels = date_format("%m-%y"))+
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
       gg_NH4_PoreWater2=gg_NH4_PoreWater2,
       gg_NO3_PoreWater2=gg_NO3_PoreWater2,
       gg_PO4_PoreWater2=gg_PO4_PoreWater2,
       gg_TFPA_PoreWater2=gg_TFPA_PoreWater2,
       gg_TRS_PoreWater2=gg_TRS_PoreWater2,
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
    ggtitle("Ammonium")+
    theme_CKM()
  
  
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
    ggtitle("Nitrate")+
    theme_CKM()
  
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
    ggtitle("Phosphate")+
    theme_CKM()
  
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
    ggtitle("TRS")+
    theme_CKM()
  
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
    ggtitle("phenolics")+
    theme_CKM()
  
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
    ggtitle("TFPA")+
    theme_CKM()
  
  gg_MBC_Extract =
    Extract_processed_all %>%
    ggplot(aes(x=DATE, y=MBC, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 2) +
    stat_summary(fun="mean",geom = "point",size = 3) +
    stat_summary(fun.data = mean_se, geom = "errorbar",lwd=1,width=1)+
    scale_y_continuous(oob=rescale_none,expand = c(0, 0))+
    facet_wrap(~YEAR, scale="free_x", nrow=1)+
    scale_x_date(labels = date_format("%m"))+
    scale_colour_manual(values= c("#009E73","#E69F00", "#56B4E9"))+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Month", 
         y = bquote('Microbial biomass ('*mu*'g C'~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Microbial Biomass Carbon")+
    theme_CKM3()+
    theme(legend.position = "bottom")
  
  gg_MBN_Extract =
    Extract_processed_all %>%
    ggplot(aes(x=DATE, y=MBN, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 2) +
    stat_summary(fun="mean",geom = "point",size = 3) +
    stat_summary(fun.data = mean_se, geom = "errorbar", lwd=1,width=1)+
    facet_wrap(~YEAR, scale="free", nrow=1)+
    scale_y_continuous(oob=rescale_none,expand = c(0, 0))+
    scale_x_date(labels = date_format("%m"))+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Month", 
         y = bquote('Microbial biomass ('*mu*'g N'~g^-1 ~ dry ~ soil*')'))+
    ggtitle("Microbial Biomass Nitrogen")+
    theme_CKM3()
  
  gg_MBP_Extract =
    Extract_processed_all %>%
    ggplot(aes(x=DATE, y=Mic.PO4, color=Site, shape=treatment))+
    stat_summary(fun="mean",geom = "line",size = 1) +
    stat_summary(fun="mean",geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~YEAR, scale="free_x", nrow=1)+
    scale_y_continuous(oob=rescale_none,expand = c(0, 0))+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    labs(x = "Date", 
         y = bquote('Microbial biomass ('*mu*'g P'~g^-1 ~ dry ~ soil*')'))+
    ggtitle("MBP")+
    theme_CKM3()
  
  A<-Extract_processed_all %>%
    group_by(Date,YEAR,Site)%>%
    summarise(MBC2=mean(MBC),MBCse= sqrt(var(MBC, na.rm = TRUE) / sum(!is.na(MBC))),MBN2=mean(MBN),MBNse= sqrt(var(MBN, na.rm = TRUE) / sum(!is.na(MBN))))
  
  
  COMB<-Combine_plots2(gg_MBC_Extract,gg_MBN_Extract)
  
  
  
  gg_MBC_3year =
    Extract_processed_all %>%
    filter(YEAR %in% c("2017","2018","2019") )%>%
    ggplot(aes(x=DATE, y=MBC, color=Site))+
    stat_summary(fun="mean",geom = "line",size = 1.3) +
    stat_summary(fun="mean",geom = "point",size = 1.9) +
    stat_summary(fun.data = mean_se, geom = "errorbar", size=0.8)+
    facet_wrap(~YEAR, scale="free_x")+
    theme_light()+
    theme(title = element_blank(),strip.text.x = element_blank(),
          axis.text.x = element_text(angle=90),
          axis.text=element_text(size=18),
          axis.title=element_text(size=20,face="bold"),
          legend.text = element_text(size=20))+
    scale_x_date(date_labels = "%m/%Y")+
    scale_colour_manual(values= c("#009E73","#E69F00", "#56B4E9"))+
    labs(x = "Date", 
         y = expression(atop('Microbial biomass',paste('('*mu*'g C'~g^-1 ~ dry ~ soil*')'))))+
    ggtitle("Microbial biomass carbon")+
    theme(legend.position = "bottom")
  
  ggsave(gg_MBC_3year,filename="MBC_3year.png","Graphs/", device= "png" ,width = 18, height = 8 , units = "in")
  
  list("all NH4"= gg_NH4_Extract,
       "all NO3"= gg_NO3_Extract,
       "all PO4"= gg_PO4_Extract,
       "all TFPA"= gg_TFPA_Extract,
       "all Phenolics"= gg_phenolics_Extract,
       "all TRS"= gg_TRS_Extract,
       "all MBC"= gg_MBC_Extract,
       "all MBN"= gg_MBN_Extract,
       "all MBP"= gg_MBP_Extract,
       COMB=COMB
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

  PoreWater_data2=PoreWater_data%>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    filter(YEAR %in% c("2017","2018","2019"))%>%
    mutate(across(Site,str_replace,"Hydric", "East wet"))%>%
    mutate(across(Site,str_replace,"Mesic", "West wet"))%>%
    mutate(across(Site,str_replace,"Xeric", "East dry"))
  
  PoreWater_data=PoreWater_data%>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate(across(Site,str_replace,"Hydric", "East wet"))%>%
    mutate(across(Site,str_replace,"Mesic", "West wet"))%>%
    mutate(across(Site,str_replace,"Xeric", "East dry"))
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
  
  
  gg_NH4_PoreWater2 =
    PoreWater_data2 %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','TRS'), as.numeric) %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary","fertilized"),Site %in% c("East wet","West wet","East dry"))%>%
    mutate(treatment=factor(treatment, levels=c("Ancillary","Control","Snowfence","fertilized")),DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    ggplot(aes(x=DATE, y=NH4, color=treatment))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~Site, scales="free", ncol=1)+
    theme_light()+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank())+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_x_break(breaks=as.Date(c("2017-09-03","2018-05-30","2018-08-30","2019-06-05")))+
    scale_x_date(labels = date_format("%m-%Y"))+
    labs(x = "Date", 
         y = bquote('Ammonium ('*mu*'g '*NH[4]^"+"~-N~mL^-1 ~ Soil ~ Solution*')'))+
    ggtitle("Ammonium")
  
  
  gg_NO3_PoreWater2 =
    PoreWater_data2 %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','TRS'), as.numeric) %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary","fertilized"),Site %in% c("East wet","West wet","East dry"))%>%
    mutate(treatment=factor(treatment, levels=c("Ancillary","Control","Snowfence","fertilized")),DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    ggplot(aes(x=DATE, y=NO3, color=treatment))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~Site, scales="free", ncol=1)+
    theme_light()+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank())+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_x_break(breaks=as.Date(c("2017-09-03","2018-05-30","2018-08-30","2019-06-05")))+
    scale_x_date(labels = date_format("%m-%Y"))+
    labs(x = "Date", 
         y = bquote('Nitrate ('*mu*'g '*NO[3]^"-"~-N~mL^-1 ~ Soil ~ Solution*')'))+
    ggtitle("Nitrate")
  
  
  gg_PO4_PoreWater2 =
    PoreWater_data2 %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','TRS'), as.numeric) %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary","fertilized"),Site %in% c("East wet","West wet","East dry"))%>%
    mutate(treatment=factor(treatment, levels=c("Ancillary","Control","Snowfence","fertilized")),DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    ggplot(aes(x=DATE, y=PO4, color=treatment))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~Site, scales="free", ncol=1)+
    theme_light()+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank())+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_x_break(breaks=as.Date(c("2017-09-03","2018-05-30","2018-08-30","2019-06-05")))+
    scale_x_date(labels = date_format("%m-%Y"))+
    scale_y_continuous(limits= c(0,5),oob=rescale_none)+
    labs(x = "Date", 
         y = bquote('Phosphate ('*mu*'g '*PO[4]^"3-"~-P~mL^-1 ~ Soil ~ Solution*')'))+
    ggtitle("Phosphate")
  
  
  
  gg_TRS_PoreWater2 =
    PoreWater_data2 %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','TRS'), as.numeric) %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary","fertilized"),Site %in% c("East wet","West wet","East dry"))%>%
    mutate(treatment=factor(treatment, levels=c("Ancillary","Control","Snowfence","fertilized")),DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    ggplot(aes(x=DATE, y=TRS, color=treatment))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~Site, scales="free", ncol=1)+
    theme_light()+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank())+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_x_break(breaks=as.Date(c("2017-09-03","2018-05-30","2018-08-30","2019-06-05")))+
    scale_x_date(labels = date_format("%m-%Y"))+
    labs(x = "Date", 
         y = bquote('Total reducing sugars-glucose equiv. ('*mu*'g' ~mL^-1 ~ Soil ~ Solution*')'))+
    ggtitle("TRS")
  
  
  gg_TFPA_PoreWater2 =
    PoreWater_data2 %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','TRS'), as.numeric) %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary","fertilized"),Site %in% c("East wet","West wet","East dry"))%>%
    mutate(treatment=factor(treatment, levels=c("Ancillary","Control","Snowfence","fertilized")),DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    ggplot(aes(x=DATE, y=TFPA, color=treatment))+
    stat_summary(geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~Site, scales="free", ncol=1)+
    theme_light()+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank())+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_x_break(breaks=as.Date(c("2017-09-03","2018-05-30","2018-08-30","2019-06-05")))+
    scale_x_date(labels = date_format("%m-%Y"))+
    labs(x = "Date", 
         y = bquote('Total free primary amines-Leucine equiv. (nMol' ~mL^-1 ~ Soil ~ Solution*')'))+
    ggtitle("TFPA")
  
  
  
  
  
  
  
  
  
  
  
  
  
  PoreWater_data3 = PoreWater_data %>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE))%>%
    filter(Site %in% c("East wet","West wet","East dry"),
           treatment %in% c("Snowfence","Control","Ancillary","fertilized")) %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','TRS'), as.numeric)
  
  PoreWater_processed_long = PoreWater_data3 %>%
    pivot_longer(cols= Mass:TRS,
                 names_to= "analyte",
                 values_to= "conc") 
  
  
  Fit.LME=function(PoreWater_processed_long){
    a = nlme::lme(conc ~ MONTH * YEAR * Site * treatment,
                  random = ~1|Plot,
                  data = PoreWater_processed_long,na.action=na.exclude)%>%
      anova()
    
    a %>% 
      as.data.frame() %>% 
      rownames_to_column("variable") %>% 
      filter(variable %in% c("MONTH","YEAR","Site","treatment","MONTH:YEAR","MONTH:Site","YEAR:Site","MONTH:treatment","YEAR:treatment","Site:treatment","MONTH:YEAR:Site","MONTH:YEAR:treatment","MONTH:Site:treatment","YEAR:Site:treatment","MONTH:YEAR:Site:treatment")) %>%
      dplyr::rename(p_value = 'p-value') %>%
      mutate(p_value == round(p_value, 3),asterisk = case_when(`p_value` <= 0.05 ~ "*"))
  }
  
  
  Porewater_LME = 
    PoreWater_processed_long %>% 
    group_by(analyte)%>%
    do(Fit.LME(.))%>%
    na.omit()%>%
    knitr::kable(caption = "Significant LME comparisons")
  
  Porewater_LME2 = 
    PoreWater_processed_long %>% 
    group_by(analyte)%>%
    do(Fit.LME(.))%>%
    knitr::kable(caption = "All LME comparisons")
  
  
  
  
  
  list("Fert PoreWater_NH4"= gg_NH4_PoreWater,
       "Fert PoreWater_NO3"= gg_NO3_PoreWater,
       "Fert PoreWater_PO4"= gg_PO4_PoreWater,
       "Fert PoreWater_TFPA"= gg_TFPA_PoreWater,
       #"Fert PoreWater_Phenolics"= gg_phenolics_PoreWater,
       "Fert PoreWater_TRS"= gg_TRS_PoreWater,
       Porewater_LME=Porewater_LME,
       Porewater_LME2=Porewater_LME2,
       gg_NH4_PoreWater2=gg_NH4_PoreWater2,
       gg_NO3_PoreWater2=gg_NO3_PoreWater2,
       gg_PO4_PoreWater2=gg_PO4_PoreWater2,
       gg_TFPA_PoreWater2=gg_TFPA_PoreWater2,
       gg_TRS_PoreWater2=gg_TRS_PoreWater2
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
  mutate( Site="East wet")
  
  Table<-Resin_processed %>%
    group_by(Site,YEAR) %>%
    summarise(NH4=mean(Ammonium,na.rm=T),NH4se= sqrt(var(Ammonium, na.rm = TRUE) / sum(!is.na(Ammonium))), 
              NO3=mean(Nitrate,na.rm=T),NO3se= sqrt(var(Nitrate, na.rm = TRUE) / sum(!is.na(Nitrate))),
              PO4=mean(Phosphate,na.rm=T),PO4se= sqrt(var(Phosphate, na.rm = TRUE) / sum(!is.na(Phosphate))))
 
  
   gg_NH4_Extract =
    Resin_processed %>%
    mutate(Purpose2=factor(Purpose2, levels= c("OW-GS","OW","GS")))%>%
    ggplot(aes(x=Purpose2, y=Ammonium, color=Site))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2,
                 position = position_dodge(width = 1),
                 aes(group = interaction(Site,Purpose2,YEAR)),
                 width=0.7)+
    geom_point(position = position_dodge(width = 1), size = 2)+
    facet_wrap(~YEAR, scale="free_x", nrow=1)+
    geom_text(data = Extract_resin_aov2 %>% filter(analyte == "Ammonium"), aes(y = 0.04, label = asterisk), size=6, color="black",show.legend = FALSE)+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
     scale_x_discrete(breaks=c("OW","OW-GS","GS","OW","GS","OW-GS","OW"),
                      labels=c(" W '16-'17", "GW '17- '18",  "G '18","W '18-'19","G '19","WG '19-'20","W '20-'21"))+
    labs(x = "W- Winter G- Growing season", 
         y = bquote('Ammonium ('*mu*'g '*NH[4]^"+"~-N~cm^-2 ~ Day^-1*')'))+
    ggtitle("Ammonium")
  
  
  gg_NO3_Extract =
    Resin_processed %>%
    mutate(Purpose2=factor(Purpose2, levels= c("OW-GS","OW","GS")))%>%
    ggplot(aes(x=Purpose2, y=Nitrate, color=Site))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2,
                 position = position_dodge(width = 1),
                 aes(group = interaction(Site,Purpose2,YEAR)),
                 width=0.7)+
    geom_point(position = position_dodge(width = 1), size = 2)+
    facet_wrap(~YEAR, scale="free_x", nrow=1)+
    geom_text(data = Extract_resin_aov2 %>% filter(analyte == "Ammonium"), aes(y = 0.04, label = asterisk), size=6, color="black",show.legend = FALSE)+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_x_discrete(breaks=c("OW","OW-GS","GS","OW","GS","OW-GS","OW"),
                     labels=c(" W '16-'17", "GW '17- '18",  "G '18","W '18-'19","G '19","WG '19-'20","W '20-'21"))+
    labs(x = "W- Winter G- Growing season", 
         y = bquote('Nitrate ('*mu*'g '*NO[3]^"-"~-N~cm^-2 ~ Day^-1*')'))+
    ggtitle("Nitrate")
  
  gg_PO4_Extract =
    Resin_processed %>%
    mutate(Purpose2=factor(Purpose2, levels= c("OW-GS","OW","GS")))%>%
    ggplot(aes(x=Purpose2, y=Phosphate, color=Site))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2,
                 position = position_dodge(width = 1),
                 aes(group = interaction(Site,Purpose2,YEAR)),
                 width=0.7)+
    geom_point(position = position_dodge(width = 1), size = 2)+
    facet_wrap(~YEAR, scale="free_x", nrow=1)+
    geom_text(data = Extract_resin_aov2 %>% filter(analyte == "Ammonium"), aes(y = 0.04, label = asterisk), size=6, color="black",show.legend = FALSE)+
    theme_light()+
    scale_colour_manual(values=cbPalette2)+
    scale_fill_manual(values=cbPalette2)+
    scale_x_discrete(breaks=c("OW","OW-GS","GS","OW","GS","OW-GS","OW"),
                     labels=c(" W '16-'17", "GW '17- '18",  "G '18","W '18-'19","G '19","WG '19-'20","W '20-'21"))+
    labs(x = "W- Winter G- Growing season",  
         y = bquote('Phosphate ('*mu*'g '*PO[4]^"3-"~-P~cm^-2 ~ Day^-1*')'))+
    ggtitle("Phosphate")
  
  
  COMB<-Combine_plots(gg_NH4_Extract,gg_NO3_Extract,gg_PO4_Extract)
  
  
  
  legend = cowplot::get_legend(gg_NH4_Extract+ guides(color = guide_legend(nrow = 1)) +
                                          theme(legend.position = "bottom"))
  gg_Resin_combine= cowplot::plot_grid(
    gg_NH4_Extract + theme(legend.position="none"),
    gg_NO3_Extract + theme(legend.position="none"),
    gg_PO4_Extract + theme(legend.position="none"),
    align = 'vh',
    labels = c("A", "B"),
    label_y= 0.93,
    hjust = -1,
    nrow = 1
  )
  gg_Legend=cowplot::plot_grid(gg_Resin_combine,legend, ncol=1, rel_heights =c(1,0.03))
  
  
  
  
  
  
  
  
  
  
  
  
  
  gg_NH4_Extract2 =
    Resin_processed %>%
    mutate(Purpose2=factor(Purpose2, levels= c("OW-GS","OW","GS")))%>%
    filter(Treatment %in% c("Snowfence","Control","Ancillary"))%>%
    ggplot(aes(x=Purpose2, y=Ammonium, color=Site,alpha=Treatment))+
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
    #mutate( Site="East wet")
  

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
    ggbiplot(pca_polar$pca_int, obs.scale = 1,var.scale = 1,
             groups = factor(pca_polar$grp$Site, levels=c("Xeric","Hydric","Mesic")), 
             ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0, varname.size=4,varname.adjust=1) +
    geom_point(size=3,stroke=1, alpha = 0.6,
               aes(#shape = groups,
                 color = groups))+
    #scale_shape_manual(values = c(21, 22, 19), name = "", guide = "none")+
    xlim(-4,4)+
    ylim(-3.5,3.5)+
    scale_color_manual(values= cbPalette2, labels=c("East dry","East wet","West wet"))+
    labs(shape="",
         title = "Polar seperation by site")+
    guides(color=guide_legend(title="Site"))+
    theme(plot.margin = unit(c(0,0,0,0), "cm"))+
    theme_CKM()
  
  
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
         title = "all samples-Polar",
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
         title = "all samples-Polar",
         subtitle = "separation by Site")+
    theme_kp()+
    NULL
  
  
  list("pca_by_year"= gg_pca_by_year,
       "pca_by_season"= gg_pca_by_season
  )
  
}

plot_pca_by_site_nonpolar= function(pca_nonpolar){
  
  gg_pca_by_site = 
    ggbiplot(pca_nonpolar$pca_int, obs.scale = 1, var.scale = 1,
             groups = factor(pca_nonpolar$grp$Site, levels=c("Xeric","Hydric","Mesic")), 
             ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0, varname.size=4,varname.adjust=1) +
    geom_point(size=3,stroke=1, alpha = 0.6,
               aes(#shape = groups,
                 color = groups))+
    #scale_shape_manual(values = c(21, 22, 19), name = "", guide = "none")+
    xlim(-4,4)+
    ylim(-3.5,3.5)+
    scale_color_manual(values= cbPalette2, labels=c("East dry","East wet","West wet"))+
    guides(color=guide_legend(title="Site"))+
    labs(shape="",
         title = "Non-Polar seperation by site")+
    theme(plot.margin = unit(c(0,0,0,0), "cm"))+
    theme_CKM()
  
  
  list("pca_by_site"= gg_pca_by_site
  )
  
}

plot_pca_nonpolar= function(pca_nonpolar){
  
  gg_pca_by_year = 
    ggbiplot(pca_nonpolar$pca_int, obs.scale = 1, var.scale = 1,
             groups = as.character(pca_nonpolar$grp$Year), 
             ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
    geom_point(size=3,stroke=1, alpha = 1,
               aes(
                 #shape = as.character(pca_polar$grp$Site),
                 color = groups))+
    #scale_shape_manual(values = c(21, 22, 19), name = "", guide = "none")+
    xlim(-4,4)+
    ylim(-3.5,3.5)+
    labs(shape="",
         title = "all samples-nonpolar",
         subtitle = "separation by Site")+
    theme_kp()+
    NULL
  
  gg_pca_by_season = 
    ggbiplot(pca_nonpolar$pca_int, obs.scale = 1, var.scale = 1,
             groups = as.character(pca_nonpolar$grp$Season), 
             ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
    geom_point(size=3,stroke=1, alpha = 1,
               aes(#shape = groups,
                 color = groups))+
    #scale_shape_manual(values = c(21, 22, 19), name = "", guide = "none")+
    xlim(-4,4)+
    ylim(-3.5,3.5)+
    labs(shape="",
         title = "all samples-nonpolar",
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
    filter(Site %in% c("West wet", "East wet","East dry"))
  
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
  
 knitr::kable(permanova_fticr_all$aov.tab, caption = "Polar PERMANOVA results")
  
  
}

plot_permanova_nonpolar= function( relabund_cores_nonpolar){
  relabund_wide = 
    relabund_cores_nonpolar %>% 
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
  
  knitr::kable(permanova_fticr_all$aov.tab, caption = "Non-Polar PERMANOVA results")
  
  
}


plot_pca_PNP= function( gg_pca_by_site_nonpolar,gg_pca_by_site){

  Nutrient_legend = cowplot::get_legend(gg_pca_by_site$pca_by_site+ guides(color = guide_legend(nrow = 1)) +
                                 theme(legend.position = "bottom",
                                       legend.title=element_text("Site"),
                                       plot.margin = unit(c(0,0,0,0), "cm")
                                       ))
  
  gg_Ncombine= cowplot::plot_grid(
    gg_pca_by_site$pca_by_site + theme(legend.position="none"),
    gg_pca_by_site_nonpolar$pca_by_site + theme(legend.position="none"),
    align = 'vh',
    labels = c("A", "B"),
    label_y= 0.93,
    vjust = 5,
    nrow = 1
  )
  gg_PCA_Legend=cowplot::plot_grid(gg_Ncombine,NULL, ncol=1,Nutrient_legend, rel_heights =c(1,0,0.1))
  
  
  list(gg_PCA_Legend=gg_PCA_Legend)
  
  
}


#Plot soil temp

plot_soilTemp=function(SoilTempHydric_data,SoilTempMesic_data,SoilTempXeric_data,Kotz_proccessed){
  
  hydric.met.station.daily <- summary_by(data=SoilTempHydric_data, Control_10~Date, FUN=c(length2, mean))
  SoilTempHydric_data2 = hydric.met.station.daily %>%
    mutate(DATE=as.Date(Date),
           YEAR=year(Date),
           MONTH=month(Date),
           WEEK=week(Date)
    )
  hydric.met.station.monthly <- summary_by(data=SoilTempHydric_data2, Control_10.mean~YEAR+MONTH+WEEK, FUN=c(length2, mean))
  HydricSoilMonthly_Processed <- subset(hydric.met.station.monthly, Control_10.mean.length2>27)
  
  Mesic.met.station.daily <- summary_by(data=SoilTempMesic_data, Control_10~Date, FUN=c(length2, mean))
  SoilTempMesic_data2 = Mesic.met.station.daily %>%
    mutate(DATE=as.Date(Date),
           YEAR=year(Date),
           MONTH=month(Date),
           WEEK=week(Date)
    )
  Mesic.met.station.monthly <- summary_by(data=SoilTempMesic_data2, Control_10.mean~YEAR+MONTH+WEEK, FUN=c(length2, mean))
  MesicSoilMonthly_Processed <- subset(Mesic.met.station.monthly, Control_10.mean.length2>27)
  
  
  MesicTemp<-Mesic.met.station.daily %>%
    select(c(Date,Control_10.mean))%>%
    rename(Mesic=Control_10.mean)
  
  
  
  
  Xeric.met.station.daily <- summary_by(data=SoilTempXeric_data, Control_10~Date, FUN=c(length2, mean))
  SoilTempXeric_data2 = Xeric.met.station.daily %>%
    mutate(DATE=as.Date(Date),
           YEAR=year(Date),
           MONTH=month(Date),
           WEEK=week(Date)
    )
  Xeric.met.station.monthly <- summary_by(data=SoilTempXeric_data2, Control_10.mean~YEAR+MONTH+WEEK, FUN=c(length2, mean))
  XericSoilMonthly_Processed <- subset(Xeric.met.station.monthly, Control_10.mean.length2>27)
  
  DF<-data.frame(Year=XericSoilMonthly_Processed$YEAR,MONTH=XericSoilMonthly_Processed$MONTH,Xeric=XericSoilMonthly_Processed$Control_10.mean.mean,Mesic=MesicSoilMonthly_Processed$Control_10.mean.mean,Hydric=HydricSoilMonthly_Processed$Control_10.mean.mean)
  
  
  DF_Daily= Xeric.met.station.daily %>%
    full_join(Mesic.met.station.daily, by="Date")%>%
    full_join(hydric.met.station.daily, by="Date")%>%
    rename("East dry"=Control_10.mean.x,"West wet"=Control_10.mean.y,"East wet"=Control_10.mean)%>%
    select(c(Date,"East dry","West wet","East wet"))%>%
    pivot_longer("East dry":"East wet")
  
  gg_soilTemp= Xeric.met.station.daily %>%
    ggplot(aes(x = as.Date(Date), y = Control_10.mean, color="#009E73")) +
    geom_line(lwd=1)+
    geom_line(data=Mesic.met.station.daily,aes(x = as.Date(Date), y = Control_10.mean, color="#E69F00"),lwd=1) +
    geom_line(data=hydric.met.station.daily,aes(x = as.Date(Date), y = Control_10.mean, color="#56B4E9"),lwd=1) +
    guides(fill=guide_legend(title= "Site"))+
    scale_color_manual(name="Site",
                       values = c("#009E73", "#E69F00", "#56B4E9" ),
                      breaks=c("#009E73", "#E69F00", "#56B4E9" ),
                      labels=c("East dry", "East wet", "West wet"))+
    xlab("Year")+
    ylab("°C")+
    ggtitle("Soil temperature at 10 cm")+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank(),
           legend.position = "bottom")+
    theme_CKM2()
    
    
    gg_soilTemp2= DF_Daily %>%
      ggplot(aes(x = as.Date(Date), y = value, color=name)) +
      geom_line(lwd=1)+
      guides(fill=guide_legend(title= "Legend"))+
      xlab("Year")+
      ylab("Soil temperature at 10 cm")+
      scale_color_manual(values=cbPalette2)+
      facet_wrap(~name,ncol=1)+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank(),
           legend.position = "none"
           )+
      theme_CKM2()
    
    gg_soilTemp3= Xeric.met.station.daily %>%
      filter(Date < '2020-01-01')%>%
      ggplot(aes(x = as.Date(Date), y = Control_10.mean, color="#009E73")) +
      geom_line()+
      geom_line(data=Mesic.met.station.daily%>%
                  filter(Date < '2020-01-01'),aes(x = as.Date(Date), y = Control_10.mean, color="#56B4E9" )) +
      geom_line(data=hydric.met.station.daily%>%
                  filter(Date < '2020-01-01'),aes(x = as.Date(Date), y = Control_10.mean, color="#E69F00" )) +
      guides(fill=guide_legend(title= "Legend"))+
      scale_color_manual(name="Site",
                         values = c("#009E73", "#E69F00", "#56B4E9" ),
                         breaks=c("#009E73", "#E69F00", "#56B4E9" ),
                         labels=c("East dry", "East wet", "West wet"))+
      xlab("Year")+
      ylab("Soil temperature at 10 cm")+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank())+
      theme_CKM2()
  
  list(gg_soilTemp=gg_soilTemp,
       gg_soilTemp2=gg_soilTemp2,
       gg_soilTemp3=gg_soilTemp3)
  
  
}

plot_soilwater=function(SoilTempHydric_data,SoilTempMesic_data,SoilTempXeric_data,Kotz_proccessed){
  
  hydric.met.station.daily <- summary_by(data=SoilTempHydric_data, Control_10_sw~Date, FUN=c(length2, mean))
  SoilTempHydric_data2 = hydric.met.station.daily %>%
    mutate(DATE=as.Date(Date),
           YEAR=year(Date),
           MONTH=month(Date),
           WEEK=week(Date)
    )
  hydric.met.station.monthly <- summary_by(data=SoilTempHydric_data2, Control_10_sw.mean~YEAR+MONTH+WEEK, FUN=c(length2, mean))
  HydricSoilMonthly_Processed <- subset(hydric.met.station.monthly, Control_10_sw.mean.length2>27)
  
  Mesic.met.station.daily <- summary_by(data=SoilTempMesic_data, Control_10_sw~Date, FUN=c(length2, mean))
  SoilTempMesic_data2 = Mesic.met.station.daily %>%
    mutate(DATE=as.Date(Date),
           YEAR=year(Date),
           MONTH=month(Date),
           WEEK=week(Date)
    )
  Mesic.met.station.monthly <- summary_by(data=SoilTempMesic_data2, Control_10_sw.mean~YEAR+MONTH+WEEK, FUN=c(length2, mean))
  MesicSoilMonthly_Processed <- subset(Mesic.met.station.monthly, Control_10_sw.mean.length2>27)
  
  
  MesicTemp<-Mesic.met.station.daily %>%
    select(c(Date,Control_10_sw.mean))%>%
    rename(Mesic=Control_10_sw.mean)
  
  
  
  
  Xeric.met.station.daily <- summary_by(data=SoilTempXeric_data, Control_10_sw~Date, FUN=c(length2, mean))
  SoilTempXeric_data2 = Xeric.met.station.daily %>%
    mutate(DATE=as.Date(Date),
           YEAR=year(Date),
           MONTH=month(Date),
           WEEK=week(Date)
    )
  Xeric.met.station.monthly <- summary_by(data=SoilTempXeric_data2, Control_10_sw.mean~YEAR+MONTH+WEEK, FUN=c(length2, mean))
  XericSoilMonthly_Processed <- subset(Xeric.met.station.monthly, Control_10_sw.mean.length2>27)
  
  DF<-data.frame(Year=XericSoilMonthly_Processed$YEAR,MONTH=XericSoilMonthly_Processed$MONTH,Xeric=XericSoilMonthly_Processed$Control_10.mean.mean,Mesic=MesicSoilMonthly_Processed$Control_10.mean.mean,Hydric=HydricSoilMonthly_Processed$Control_10.mean.mean)
  
  
  DF_Daily= Xeric.met.station.daily %>%
    full_join(Mesic.met.station.daily, by="Date")%>%
    full_join(hydric.met.station.daily, by="Date")%>%
    rename("East dry"=Control_10_sw.mean.x,"West wet"=Control_10_sw.mean.y,"East wet"=Control_10_sw.mean)%>%
    select(c(Date,"East dry","West wet","East wet"))%>%
    pivot_longer("East dry":"East wet")
  
  gg_soilwater= Xeric.met.station.daily %>%
    ggplot(aes(x = as.Date(Date), y = Control_10_sw.mean, color="#009E73")) +
    geom_line(lwd=1)+
    geom_line(data=Mesic.met.station.daily,aes(x = as.Date(Date), y = Control_10_sw.mean, color="#56B4E9"),lwd=1) +
    geom_line(data=hydric.met.station.daily,aes(x = as.Date(Date), y = Control_10_sw.mean, color="#E69F00" ),lwd=1) +
    guides(fill=guide_legend(title= "Site"))+
    scale_color_manual(name="Site",
                       values = c("#009E73", "#E69F00", "#56B4E9" ),
                       breaks=c("#009E73", "#E69F00", "#56B4E9" ),
                       labels=c("East dry", "East wet", "West wet"))+
    xlab("Year")+
    ylab("Soil water content (v/v)")+
    ggtitle("Soil moisture at 10 cm")+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank())+
    theme_CKM2()
  
  
  gg_soilwater2= DF_Daily %>%
    ggplot(aes(x = as.Date(Date), y = value, color=name)) +
    geom_line(lwd=1)+
    guides(fill=guide_legend(title= "Legend"))+
    xlab("Year")+
    ylab("Soil water content (v/v)")+
    scale_color_manual(values=cbPalette2)+
    facet_wrap(~name,ncol=1)+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank(),
           legend.position = "none"
    )+
    theme_CKM2()
  
  gg_soilwater3= Xeric.met.station.daily %>%
    filter(Date < '2020-01-01')%>%
    ggplot(aes(x = as.Date(Date), y = Control_10_sw.mean, color="#009E73")) +
    geom_line()+
    geom_line(data=Mesic.met.station.daily%>%
                filter(Date < '2020-01-01'),aes(x = as.Date(Date), y = Control_10_sw.mean, color="#56B4E9" )) +
    geom_line(data=hydric.met.station.daily%>%
                filter(Date < '2020-01-01'),aes(x = as.Date(Date), y = Control_10_sw.mean, color="#E69F00" )) +
    guides(fill=guide_legend(title= "Legend"))+
    scale_color_manual(name="Site",
                       values = c("#009E73", "#E69F00", "#56B4E9" ),
                       breaks=c("#009E73", "#E69F00", "#56B4E9" ),
                       labels=c("East dry", "East wet", "West wet"))+
    xlab("Year")+
    ylab("Soil water content (v/v)")+
    theme( axis.text.x.top = element_blank(),
           axis.ticks.x.top = element_blank())+
    theme_CKM2()
  
  list(gg_soilwater=gg_soilwater,
       gg_soilwater2=gg_soilwater2,
       gg_soilwater3=gg_soilwater3)
  
  
}

plot_soilTempwater=function(Soil_Water,Soil_Temp){
  
  Soil_Water$gg_soilwater
  Soil_Temp$gg_soilTemp
  COMB<-Combine_plots2(Soil_Water$gg_soilwater,Soil_Temp$gg_soilTemp)
  list(COMB=COMB)
  
  
}

plot_Ibutton=function(Ibutton_data){
  
  
  Ibutton_data$date<-as.Date(Ibutton_data$date, "%m/%d/%Y")
  
  Ibutton_data%>%
    ggplot(aes(x=date,y=temp_C, color=trt) )+
    stat_summary(fun="mean",geom = "point") +
    facet_wrap(~site)
  
  a<-lm(data=Ibutton_data, temp_C~ trt + date)
  anova(a)
  
  
  list()
  
  
}

#Plot branch extension

plot_BranchExtension=function(AggieBranch_data){
  
  AggieBranch_data%>%
    rename("Gmm"=growth_mm.mean)%>%
    ggplot(aes(x=site, y=Gmm, color=trt))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2,
                 position = position_dodge(width = 1),
                 aes(group = interaction(site,trt)),
                 width=0.7)+
    facet_wrap(~year)
  
  list()
  
  
}


plot_AggieNNeedle=function(AggieNNeedle_data){
  
  AggieNNeedle_data%>%
    pivot_longer(June2018N:August2017N)%>%
    filter(site!="")%>%
    ggplot(aes(x=site, y=value, color=trt))+
    geom_boxplot(show.legend = T, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2,
                 position = position_dodge(width = 1),
                 aes(group = interaction(site,trt)),
                 width=0.7)+
    facet_wrap(~name)
  
  list()
  
  
}

plot_Tree = function(AggieTree_data){
  
  Tree_info<-AggieTree_data%>%
    knitr::kable(caption = "Significant comparisons")
  
  
  
  
  
  list(Tree_info
  )
  
}

#pH
plot_ph = function(ph_data){
  
  ph_data%>%
    group_by(Site)%>%
    summarize(mean = mean(pH, na.rm = TRUE),
            se= sqrt(var(pH, na.rm = TRUE) / sum(!is.na(pH))))
  
  
  
  ph_data2=ph_data%>%
    mutate(across(Site,str_replace,"Hydric", "East wet"))%>%
    mutate(across(Site,str_replace,"Mesic", "West wet"))%>%
    mutate(across(Site,str_replace,"Xeric", "East dry"))
  ph_data2$Site<-as.factor(ph_data2$Site)
  
  
  gg_ph =
    ph_data2 %>%
    ggplot(aes(x=Site, y=pH, color=Site))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2,
                 position = position_dodge(width = 1),
                 aes(group = interaction(Site)),
                 width=0.7)+
    geom_point(position = position_dodge(width = 1), size = 2)+
    theme_light()+
    scale_color_manual(values= cbPalette2, labels=c("East dry","East wet","West wet"))+
    scale_fill_manual(values=cbPalette2)+
    ggtitle("pH")+
    theme_CKM()+
    theme(legend.position = "bottom")

 
  
  list(gg_ph
  )
  
}

#Precip
plot_Precip = function(Precip_data){
  

  
  Precip_data2<- Precip_data%>%
    pivot_longer(cols=Jan:Dec)%>%
    mutate(value2=value*25.4)
    
  
 
  
  
  gg_Precip =
    Precip_data2 %>%
    ggplot(aes(x=name, y=value2))+
    geom_point()+
    theme_light()+
    facet_wrap(~Year)+
    ggtitle("Precip monthly accumulation at Kotz")+
    ylab("Precip (mm)")+
    xlab("Month")+
    theme_CKM()+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  
  
  list(gg_Precip
  )
  
}


plot_Daily_Precip = function(Daily_Precip_data){
  

  
  Precip_data2<- Daily_Precip_data%>%
    mutate(value2=as.numeric(Precip)*25.4,Date2=mdy(paste(Month,Day,Year)))
  
  
  
  
  
  gg_Daily_Precip =
    Precip_data2 %>%
    ggplot(aes(x=Date2, y=value2))+
    geom_col()+
    theme_light()+
    scale_y_continuous(expand=c(0,0),limits=c(0,39),oob=rescale_none)+
    #facet_wrap(~Year, scales="free")+
    ggtitle("Precip at Kotz")+
    ylab("Precip (mm)")+
    xlab("Year")+
    theme_CKM()+
    theme(legend.position = "none")
  
  
  
  list(gg_Daily_Precip
  )
  
}