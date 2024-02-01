#Combining 2019-2021 data



Pore<-PoreWater_processed_all %>%
  filter(YEAR %in% c("2019","2020","2021")) %>%
  select(-c("DATE","YEAR","MONTH")) %>%
  mutate(Data.Type= "Pore water")
Extract<-Extract_processed_all%>%
  filter(YEAR %in% c("2019","2020","2021")) %>%
  select(-c("DATE","YEAR","MONTH"))%>%
  mutate(Data.Type= "Soil extracts")
pH<-ph_data%>%
  mutate(Data.Type= "Soil pH")%>%
  rename(treatment=Treatment)%>%
  rename(Plot=plot)
Loc<-AggieTree_data%>%
  mutate(Data.Type= "Tree locations")%>%
  rename(Plot=plot)
Res<-Resin_processed%>%
  filter(YEAR %in% c("2019","2020","2021")) %>%
  select(-c("DATE","YEAR","MONTH")) %>%
  mutate(Data.Type= "Resin strip")%>%
  rename(treatment=Treatment)%>%
  rename(Plot=Plot..)%>%
  rename(NH4=Ammonium)%>%
  rename(NO3=Nitrate)%>%
  rename(PO4=Phosphate)



Data<-Extract %>%
  full_join(Pore)%>%
  full_join(Res)%>%
  full_join(pH) %>%
  full_join(Loc)

Data$Site<-recode(Data$Site, "East wet" = 'Hydric', 
             "East dry" = 'Xeric',
             "West wet" = 'Mesic')
  
write.csv(Data, "Data/Weintraub_Soil_Analysis_Treeline_Summary2019-2021_Data.csv")  
  