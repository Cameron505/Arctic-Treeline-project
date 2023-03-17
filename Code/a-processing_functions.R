

process_PoreWater =function(PoreWater_data){
  
  processed_PoreWater = PoreWater_data %>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','TRS'), as.numeric) %>%
    filter(treatment %in% c("Snowfence","Control"))%>%
    drop_na(YEAR)
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
    filter(YEAR %in% "2017")

  
  
  
  
  }

process_Extract =function(Extract_data){
  
  processed_Extract = Extract_data %>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','phenolics','TRS','MBC','MBN','Mic.PO4'), as.numeric) %>%
    filter(treatment %in% c("Snowfence","Control")) %>%
    mutate(MONTH=as.factor(MONTH),
           TIME= ifelse(MONTH == 3, "Late winter", as.character(MONTH)), 
           TIME= ifelse(TIME == 4, "Late winter", as.character(TIME)),
           TIME= ifelse(TIME == 5, "Early spring", as.character(TIME)),
           TIME= ifelse(TIME == 6, "Early spring", as.character(TIME)),
           TIME= ifelse(TIME == 9, "Late summer", as.character(TIME))
    ) %>%
    mutate(TIME = factor(TIME, levels=c("Late winter","Early spring","Late summer")))
    
}

process_Extract_H2O =function(Extract_data){
  
  processed_Extract = Extract_data %>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4.H2O', 'NO3.H2O','PO4.H2O','TFPA.H2O','TRS.H2O'), as.numeric) %>%
    filter(treatment %in% c("Snowfence","Control"))
  
  processed_Extract = processed_Extract %>%
    mutate(MONTH=as.factor(MONTH),
           TIME= ifelse(MONTH == 3, "Late winter", as.character(MONTH)), 
           TIME= ifelse(TIME == 4, "Late winter", as.character(TIME)),
           TIME= ifelse(TIME == 5, "Early spring", as.character(TIME)),
           TIME= ifelse(TIME == 6, "Early spring", as.character(TIME)),
           TIME= ifelse(TIME == 9, "Late summer", as.character(TIME))
    ) %>%
    mutate(TIME = factor(TIME, levels=c("Late winter","Early spring","Late summer")))
}


process_Resin =function(Resin_data){
  
  processed_Resin = Resin_data %>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE))
}


process_PoreWater_Seasonal =function(PoreWater_data){
  
  processed_PoreWater_Seasonal = PoreWater_data %>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','TRS'), as.numeric) %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary"))
}

process_Extract_Seasonal =function(Extract_data){
  
  processed_Extract_Seasonal = Extract_data %>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','phenolics','TRS','MBC','MBN','Mic.PO4'), as.numeric) %>%
    filter(treatment %in% c("Ancillary"))
  
}


process_Extract_Seasonal_H2O =function(Extract_data){
  
  processed_Extract_Seasonal = Extract_data %>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4.H2O', 'NO3.H2O','PO4.H2O','TFPA.H2O','TRS.H2O'), as.numeric) %>%
    filter(treatment %in% c("Ancillary"))
  
}

process_PoreWater_all =function(PoreWater_data){
  
  processed_PoreWater_all = PoreWater_data %>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','TRS'), as.numeric) %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary"))
}

process_Extract_all =function(Extract_data){
  
  processed_Extract_all = Extract_data %>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','phenolics','TRS','MBC','MBN','Mic.PO4'), as.numeric) %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary"))
  
}


process_Extract_all_H2O =function(Extract_data){
  
  processed_Extract_all = Extract_data %>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4.H2O', 'NO3.H2O','PO4.H2O','TFPA.H2O','TRS.H2O'), as.numeric) %>%
    filter(treatment %in% c("Snowfence","Control","Ancillary"))
  
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
    mutate_at(c('Ammonium', 'Nitrate','Phosphate'), as.numeric)
    
  
}




#### FTICR

process_fticr_hcoc =function(fticr_data_trt,fticr_meta){
  
  TREATMENTS = dplyr::quos(Site, Year, Season, Polar)
  
  fticr_hcoc = 
    fticr_data_trt %>% 
    reorder_seasons() %>% reorder_sites() %>% 
    left_join(dplyr::select(fticr_meta, formula, HC, OC), by = "formula")
  
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

process_fticr_relabund_cores_polar =function(relabund_cores){
  
  relabund_cores_polar = relabund_cores %>% filter(Polar == "polar")
  
}

process_pca_polar =function(relabund_cores){
  
  pca_polar = fit_pca_function(relabund_cores %>% filter(Polar == "polar"))
  
}

process_pca_hydric =function(relabund_cores){
  
  pca_hydric = fit_pca_function(relabund_cores %>% filter(Polar == "nonpolar" & Site == "Hydric"))
  
}


