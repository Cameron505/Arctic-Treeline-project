

#read in
kotz.daily.air.temp <- read.csv("data/Temp/kotz.daily.air.temp.1900.2019.csv") 


#calculate sample size
length2= function (x, na.rm=FALSE) {
  if (na.rm) sum(!is.na(x))
  else       length(x)
}

#calculate monthly Kotz data
Kotz.Monthly<- kotz.daily.air.temp %>%
  mutate(DATE= as.Date(DATE, format = "%Y-%m-%d"))%>%
  mutate(Year= year(DATE),Month= month(DATE))%>%
  group_by(Year, Month) %>%
  mutate(Mean= mean(TAVG), SS= length2(TAVG))%>%
  ungroup()


#Organize hydric
hydric.met.station <- read.csv("data/Temp/Hydric Met Station.csv")

Hydric.Monthly<- hydric.met.station %>%
  mutate(Date= as.Date(Date, format = "%Y-%m-%d"))%>%
  mutate(Year= year(Date),Month= month(Date))%>%
  group_by(Year, Month) %>%
  summarize(Mean= mean(Control_10), SS= length2(Control_10))%>%
  subset(SS>27) %>%
  ungroup()
