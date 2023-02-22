

process_PoreWater =function(PoreWater_data){
  
  processed_PoreWater = PoreWater_data %>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','TRS'), as.numeric) %>%
    filter(treatment==c("Snowfence","Control"))
}

process_Lysim_Destruct =function(PoreWater_data){
  
  processed_PoreWater_Lysim_Destruct = PoreWater_data %>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','TRS'), as.numeric) %>%
    mutate( NH4_2= NH4/14.0067,
            NO3_2= NO3/14.0067,
            NC= NH4_2+NO3_2+TFPA)%>%
    filter(YEAR=="2017")

  
  
  
  
  }

process_Extract =function(Extract_data){
  
  processed_Extract = Extract_data %>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','phenolics','TRS','MBC','MBN','Mic.PO4'), as.numeric) %>%
    filter(treatment==c("Snowfence","Control"))
  
  processed_Extract = processed_Extract %>%
    mutate(MONTH=as.factor(MONTH))
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
    filter(treatment==c("Snowfence","Control","Ancillary"))
}

process_Extract_Seasonal =function(Extract_data){
  
  processed_Extract_Seasonal = Extract_data %>%
    mutate(DATE=as.Date(Date, format= "%m/%d/%Y"),
           YEAR=year(DATE),
           MONTH=month(DATE)) %>%
    mutate_at(c('NH4', 'NO3','PO4','TFPA','phenolics','TRS','MBC','MBN','Mic.PO4'), as.numeric) %>%
    filter(treatment==c("Ancillary"))
  
}


