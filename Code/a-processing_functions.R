

process_PoreWater =function(PoreWater_data){
  
  processed_PoreWater = PoreWater_data %>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','TRS'), as.numeric) %>%
    filter(treatment %in% c("Snowfence","Control"))%>%
    drop_na(YEAR)%>%
    mutate(across(Site,str_replace,"Hydric", "East wet"))%>%
    mutate(across(Site,str_replace,"Mesic", "West wet"))%>%
    mutate(across(Site,str_replace,"Xeric", "East dry"))
}

process_Lysim_Destruct =function(PoreWater_data){
  
  processed_PoreWater_Lysim_Destruct = PoreWater_data %>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','TRS'), as.numeric) %>%
    mutate( NH4_2= NH4*14.0067,
            NO3_2= NO3*14.0067,
            PO4_2= PO4*30.973762,
            NC= NH4_2+NO3_2+TFPA)%>%
    filter(YEAR %in% "2017")%>%
    mutate(across(Site,str_replace,"Hydric", "East wet"))%>%
    mutate(across(Site,str_replace,"Mesic", "West wet"))%>%
    mutate(across(Site,str_replace,"Xeric", "East dry"))

  
  
  
  
  }

process_Extract =function(Extract_data){
  
  processed_Extract = Extract_data %>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','phenolics','TRS','MBC','MBN','Mic.PO4'), as.numeric) %>%
    filter(treatment %in% c("Snowfence","Control")) %>%
    mutate(MONTH=as.factor(MONTH))%>%
    mutate (TIME= ifelse(MONTH == 3, "Late Winter", as.character(MONTH))) %>%
    mutate (TIME= ifelse(TIME == 4, "Late Winter", as.character(TIME)))%>%
    mutate (TIME= ifelse(TIME == 5, "Early Spring", as.character(TIME)))%>%
    mutate (TIME= ifelse(TIME == 6, "Early Spring", as.character(TIME)))%>%
    mutate (TIME= ifelse(TIME == 9, "Late Summer", as.character(TIME))) %>%
    mutate(TIME = factor(TIME, levels=c("Late Winter","Early Spring","Late Summer")))%>%
    mutate(across(Site,str_replace,"Hydric", "East wet"))%>%
    mutate(across(Site,str_replace,"Mesic", "West wet"))%>%
    mutate(across(Site,str_replace,"Xeric", "East dry"))
    
    
}

process_Extract_H2O =function(Extract_data){
  
  processed_Extract = Extract_data %>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4.H2O', 'NO3.H2O','PO4.H2O','TFPA.H2O','TRS.H2O'), as.numeric) %>%
    filter(treatment %in% c("Snowfence","Control"))
  
  processed_Extract = processed_Extract %>%
    mutate(MONTH=as.factor(MONTH))%>%
    mutate (TIME= ifelse(MONTH == 3, "Late Winter", as.character(MONTH))) %>%
    mutate (TIME= ifelse(TIME == 4, "Late Winter", as.character(TIME)))%>%
    mutate (TIME= ifelse(TIME == 5, "Early Spring", as.character(TIME)))%>%
    mutate (TIME= ifelse(TIME == 6, "Early Spring", as.character(TIME)))%>%
    mutate (TIME= ifelse(TIME == 9, "Late Summer", as.character(TIME))) %>%
    mutate(TIME = factor(TIME, levels=c("Late Winter","Early Spring","Late Summer")))%>%
    mutate(across(Site,str_replace,"Hydric", "East wet"))%>%
    mutate(across(Site,str_replace,"Mesic", "West wet"))%>%
    mutate(across(Site,str_replace,"Xeric", "East dry"))
}


process_Resin =function(Resin_data){
  
  processed_Resin = Resin_data %>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE))%>%
    mutate(across(Site,str_replace,"Hydric", "East wet"))%>%
    mutate(across(Site,str_replace,"Mesic", "West wet"))%>%
    mutate(across(Site,str_replace,"Xeric", "East dry"))
}


process_PoreWater_Seasonal =function(PoreWater_data){
  
  processed_PoreWater_Seasonal = PoreWater_data %>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','TRS'), as.numeric) %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary"))%>%
    mutate(across(Site,str_replace,"Hydric", "East wet"))%>%
    mutate(across(Site,str_replace,"Mesic", "West wet"))%>%
    mutate(across(Site,str_replace,"Xeric", "East dry"))
}

process_Extract_Seasonal =function(Extract_data){
  
  processed_Extract_Seasonal = Extract_data %>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','phenolics','TRS','MBC','MBN','Mic.PO4'), as.numeric) %>%
    filter(treatment %in% c("Ancillary"))%>%
    mutate(across(Site,str_replace,"Hydric", "East wet"))%>%
    mutate(across(Site,str_replace,"Mesic", "West wet"))%>%
    mutate(across(Site,str_replace,"Xeric", "East dry"))
  
}


process_Extract_Seasonal_H2O =function(Extract_data){
  
  processed_Extract_Seasonal = Extract_data %>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4.H2O', 'NO3.H2O','PO4.H2O','TFPA.H2O','TRS.H2O'), as.numeric) %>%
    filter(treatment %in% c("Ancillary"))%>%
    mutate(across(Site,str_replace,"Hydric", "East wet"))%>%
    mutate(across(Site,str_replace,"Mesic", "West wet"))%>%
    mutate(across(Site,str_replace,"Xeric", "East dry"))
  
}

process_PoreWater_all =function(PoreWater_data){
  
  processed_PoreWater_all = PoreWater_data %>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','TRS'), as.numeric) %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary"))%>%
    mutate(across(Site,str_replace,"Hydric", "East wet"))%>%
    mutate(across(Site,str_replace,"Mesic", "West wet"))%>%
    mutate(across(Site,str_replace,"Xeric", "East dry"))
}

process_Extract_all =function(Extract_data){
  
  processed_Extract_all = Extract_data %>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','phenolics','TRS','MBC','MBN','Mic.PO4'), as.numeric) %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary"))%>%
    mutate(across(Site,str_replace,"Hydric", "East wet"))%>%
    mutate(across(Site,str_replace,"Mesic", "West wet"))%>%
    mutate(across(Site,str_replace,"Xeric", "East dry"))
  
}


process_Extract_all_H2O =function(Extract_data){
  
  processed_Extract_all = Extract_data %>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4.H2O', 'NO3.H2O','PO4.H2O','TFPA.H2O','TRS.H2O'), as.numeric) %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary"))%>%
    mutate(across(Site,str_replace,"Hydric", "East wet"))%>%
    mutate(across(Site,str_replace,"Mesic", "West wet"))%>%
    mutate(across(Site,str_replace,"Xeric", "East dry"))
  
}

process_Extract_lysim =function(Extract_data){
  
  processed_Extract_lysim = Extract_data %>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','phenolics','TRS','MBC','MBN','Mic.PO4','NH4.H2O', 'NO3.H2O','PO4.H2O','TFPA.H2O','TRS.H2O'), as.numeric) %>%
    filter(treatment %in% c("Lysim","Lysim 2","Lysim 3","Lysim 4"))
  
}

process_resin =function(Resin_data){
  
  processed_resin = Resin_data %>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('Ammonium', 'Nitrate','Phosphate'), as.numeric)%>%
    mutate(across(Site,str_replace,"Hydric", "East wet"))%>%
    mutate(across(Site,str_replace,"Mesic", "West wet"))%>%
    mutate(across(Site,str_replace,"Xeric", "East dry"))
    
  
}


#### FTICR

process_fticr_hcoc =function(fticr_data_trt,fticr_meta){
  
  TREATMENTS = dplyr::quos(Site, Year, Season, Polar)
  
  fticr_hcoc = 
    fticr_data_trt %>% 
    reorder_seasons() %>% reorder_sites() %>% 
    left_join(dplyr::select(fticr_meta, formula, HC, OC), by = "formula")%>%
    mutate(across(Site,str_replace,"Hydric", "East wet"))%>%
    mutate(across(Site,str_replace,"Mesic", "West wet"))%>%
    mutate(across(Site,str_replace,"Xeric", "East dry"))
  
}

process_fticr_relabund_cores =function(fticr_data_longform,fticr_meta){
  
  TREATMENTS = dplyr::quos(Site, Year, Season, Polar)
  
  relabund_cores = 
    fticr_data_longform %>% 
    compute_relabund_cores(fticr_meta, TREATMENTS) %>% 
    reorder_seasons() %>% reorder_sites()
  
}


process_fticr_hcoc_polar =function(fticr_hcoc){
  
  fticr_hcoc_polar = fticr_hcoc %>% filter(Polar == "polar") 
  
}
process_fticr_hcoc_nonpolar =function(fticr_hcoc){
  
  fticr_hcoc_nonpolar = fticr_hcoc %>% filter(Polar == "nonpolar") 
  
}

process_fticr_relabund_cores_polar =function(relabund_cores){
  
  relabund_cores_polar = relabund_cores %>% filter(Polar == "polar")
  
}

process_fticr_relabund_cores_nonpolar =function(relabund_cores){
  
  relabund_cores_nonpolar = relabund_cores %>% filter(Polar == "nonpolar")
  
}

process_pca_polar =function(relabund_cores){
  
  pca_polar = fit_pca_function(relabund_cores %>% filter(Polar == "polar"))
  
}
process_pca_nonpolar =function(relabund_cores){
  
  pca_nonpolar = fit_pca_function(relabund_cores %>% filter(Polar == "nonpolar"))
  
}

process_pca_hydric =function(relabund_cores){
  
  pca_hydric = fit_pca_function(relabund_cores %>% filter(Polar == "nonpolar" & Site == "Hydric"))
  
}

# Soil temperature
process_KotzTemp = function(KotzTemp_data){
  KotzTemp_data2 = KotzTemp_data %>%
    mutate(DATE=as.Date(DATE),
           YEAR=year(DATE),
           MONTH=month(DATE)
    )
  Kotz_proccessed=summaryBy(data=KotzTemp_data2, TAVG~YEAR+MONTH, FUN=c(length2, mean), na.rm=TRUE)
}

process_SoilTemp = function(SoilTempHydric_data,SoilTempMesic_data,SoilTempXeric_data,Kotz_proccessed){
  
  hydric.met.station.daily <- summary_by(data=SoilTempHydric_data, Control_10~Date, FUN=c(length2, mean))
  SoilTempHydric_data2 = hydric.met.station.daily %>%
    mutate(DATE=as.Date(Date),
           YEAR=year(Date),
           MONTH=month(Date)
    )
  
  hydric.met.station.monthly <- summary_by(data=SoilTempHydric_data2, Control_10.mean~YEAR+MONTH, FUN=c(length2, mean))
  HydricSoilMonthly_Processed <- subset(hydric.met.station.monthly, Control_10.mean.length2>27)
  hydric.soil.kotz.air <- merge(HydricSoilMonthly_Processed, Kotz_proccessed, by=c("YEAR", "MONTH"))
  hydric.summer.model <- lm(data=subset(hydric.soil.kotz.air, TAVG.mean>=5), Control_10.mean.mean~TAVG.mean)
  hydric.winter.model <- lm(data=subset(hydric.soil.kotz.air, TAVG.mean<5), Control_10.mean.mean~TAVG.mean)
  Kotz_proccessed_H=Kotz_proccessed
  
  Kotz_proccessed_H$Hydric.Soil <- ifelse(Kotz_proccessed$TAVG.mean>=5, predict(hydric.summer.model, Kotz_proccessed), 
                                          predict(hydric.winter.model, Kotz_proccessed))
  
  
  
  Mesic.met.station.daily <- summary_by(data=SoilTempMesic_data, Control_10~Date, FUN=c(length2, mean))
  SoilTempMesic_data2 = Mesic.met.station.daily %>%
    mutate(DATE=as.Date(Date),
           YEAR=year(Date),
           MONTH=month(Date)
    )
  
  Mesic.met.station.monthly <- summary_by(data=SoilTempMesic_data2, Control_10.mean~YEAR+MONTH, FUN=c(length2, mean))
  MesicSoilMonthly_Processed <- subset(Mesic.met.station.monthly, Control_10.mean.length2>27)
  Mesic.soil.kotz.air <- merge(MesicSoilMonthly_Processed, Kotz_proccessed_H, by=c("YEAR", "MONTH"))
  Mesic.summer.model <- lm(data=subset(Mesic.soil.kotz.air, TAVG.mean>=5), Control_10.mean.mean~TAVG.mean)
  Mesic.winter.model <- lm(data=subset(Mesic.soil.kotz.air, TAVG.mean<5), Control_10.mean.mean~TAVG.mean)
  Kotz_proccessed_HM<-Kotz_proccessed_H
  Kotz_proccessed_HM$Mesic.Soil <- ifelse(Kotz_proccessed_H$TAVG.mean>=5, predict(Mesic.summer.model, Kotz_proccessed_H), 
                                          predict(Mesic.winter.model, Kotz_proccessed_H))
  
  
  Xeric.met.station.daily <- summary_by(data=SoilTempXeric_data, Control_10~Date, FUN=c(length2, mean))
  SoilTempXeric_data2 = Xeric.met.station.daily %>%
    mutate(DATE=as.Date(Date),
           YEAR=year(Date),
           MONTH=month(Date)
    )
  
  Xeric.met.station.monthly <- summary_by(data=SoilTempXeric_data2, Control_10.mean~YEAR+MONTH, FUN=c(length2, mean))
  XericSoilMonthly_Processed <- subset(Xeric.met.station.monthly, Control_10.mean.length2>27)
  Xeric.soil.kotz.air <- merge(XericSoilMonthly_Processed, Kotz_proccessed, by=c("YEAR", "MONTH"))
  Xeric.summer.model <- lm(data=subset(Xeric.soil.kotz.air, TAVG.mean>=5), Control_10.mean.mean~TAVG.mean)
  Xeric.winter.model <- lm(data=subset(Xeric.soil.kotz.air, TAVG.mean<5), Control_10.mean.mean~TAVG.mean)
  Kotz_proccessed_HMX<-Kotz_proccessed_HM
  Kotz_proccessed_HMX$Xeric.Soil <- ifelse(Kotz_proccessed_HM$TAVG.mean>=5, predict(Xeric.summer.model, Kotz_proccessed_HM), 
                                           predict(Xeric.winter.model, Kotz_proccessed_HM))
  
  return(Kotz_proccessed_HMX)
  
}
