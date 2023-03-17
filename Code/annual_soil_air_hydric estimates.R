hydric.met.station.daily <- summary_by(data=SoilTempHydric_data, Control_10~Date, FUN=c(length2, mean))
SoilTempHydric_data2 = hydric.met.station.daily %>%
  mutate(DATE=as.Date(Date),
         YEAR=year(Date),
         MONTH=month(Date)
  ) %>%
  mutate (TIME= ifelse(MONTH == 11, "Fall", as.character(MONTH))) %>%
  mutate (TIME= ifelse(TIME == 12, "Winter", as.character(TIME)))%>%
  mutate (TIME= ifelse(TIME == 1, "Winter", as.character(TIME)))%>%
  mutate (TIME= ifelse(TIME == 2, "Winter", as.character(TIME)))%>%
  mutate (TIME= ifelse(TIME == 3, "Winter", as.character(TIME))) %>%
  mutate (TIME= ifelse(TIME == 4, "Winter", as.character(TIME))) %>%
  mutate (TIME= ifelse(TIME == 5, "Spring", as.character(TIME)))%>%
  mutate (TIME= ifelse(TIME == 6, "Spring", as.character(TIME)))%>%
  mutate (TIME= ifelse(TIME == 7, "summer", as.character(TIME)))%>%
  mutate (TIME= ifelse(TIME == 8, "summer", as.character(TIME))) %>%
  mutate (TIME= ifelse(TIME == 9, "summer", as.character(TIME)))%>%
  mutate (TIME= ifelse(TIME == 10, "Fall", as.character(TIME)))


A<-SoilTempHydric_data2%>%
  filter(TIME=="summer")%>%
  plyr::summarize(MEAN=mean(Control_10.mean, na.rm = T))

B<-SoilTempHydric_data2%>%
  filter(TIME=="Winter")%>%
  plyr::summarize(MEAN=mean(Control_10.mean, na.rm = T))














air.hydric.met.station.daily <- summary_by(data=SoilTempHydric_data, Air_Temp~Date, FUN=c(length2, mean))
airTempHydric_data2 = air.hydric.met.station.daily %>%
  mutate(DATE=as.Date(Date),
         YEAR=year(Date),
         MONTH=month(Date)
  ) %>%
  mutate (TIME= ifelse(MONTH == 11, "Fall", as.character(MONTH))) %>%
  mutate (TIME= ifelse(TIME == 12, "Winter", as.character(TIME)))%>%
  mutate (TIME= ifelse(TIME == 1, "Winter", as.character(TIME)))%>%
  mutate (TIME= ifelse(TIME == 2, "Winter", as.character(TIME)))%>%
  mutate (TIME= ifelse(TIME == 3, "Winter", as.character(TIME))) %>%
  mutate (TIME= ifelse(TIME == 4, "Winter", as.character(TIME))) %>%
  mutate (TIME= ifelse(TIME == 5, "Spring", as.character(TIME)))%>%
  mutate (TIME= ifelse(TIME == 6, "Spring", as.character(TIME)))%>%
  mutate (TIME= ifelse(TIME == 7, "summer", as.character(TIME)))%>%
  mutate (TIME= ifelse(TIME == 8, "summer", as.character(TIME))) %>%
  mutate (TIME= ifelse(TIME == 9, "summer", as.character(TIME)))%>%
  mutate (TIME= ifelse(TIME == 10, "Fall", as.character(TIME)))


C<-airTempHydric_data2%>%
  filter(TIME=="summer")%>%
  plyr::summarize(MEAN=mean(Air_Temp.mean, na.rm = T))

D<-airTempHydric_data2%>%
  filter(TIME=="Winter")%>%
  plyr::summarize(MEAN=mean(Air_Temp.mean, na.rm = T))







A
B
C
D
