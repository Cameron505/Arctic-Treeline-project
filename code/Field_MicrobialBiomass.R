MBC<-read.csv("Microbialbiomass.csv")
MBC$value<-as.numeric(MBC$value)
MBC$Date<-as.Date(MBC$x, "%m/%d/%Y")
MBC2 = MBC %>%
  group_by(Date, A) %>%
  summarize(mean_value = mean(value, na.rm = TRUE),
            se = sqrt(var(value, na.rm = TRUE) / sum(!is.na(value)))) %>%
  ungroup()




MBCG<- ggplot(MBC2,aes(x = Date, y = mean_value, color=A))+
  geom_point(stat= "identity", lwd=1.3)+
  geom_line(stat="identity", lwd=1)+
  scale_x_date(date_labels = "%b")+
  scale_x_break(c("2017-08-29", "2018-03-27"),ticklabels = NULL, scales = "free")+
  scale_x_break(c("2018-08-23", "2019-03-29", scales = "free"))+
  scale_x_break(c("2019-08-15", "2020-03-18", scales = "free"))+
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), lwd = 0.5, width = 0.1, color="black")+
  #facet_wrap(~ variable, ncol = 3,scales = "free")+
  scale_fill_manual(values=cbPalette)+
  ylab("Microbial biomass C")+
  theme_kp()+
  theme(axis.text.x = element_text(angle = 90))

ggsave("Microbial biomass.png", device="png")
