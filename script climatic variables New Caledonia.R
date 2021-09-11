rm(list=ls())

library(ggplot2)
library(dplyr)
library(lubridate)
library(gridExtra)
library(tibble)
library(lattice)
library(grid)

options(stringsAsFactors = FALSE)

setwd("D:/disque dur/Work from home/Isotope/climatic data and analysis")

moue <- read.table("Moue.txt",header=T)

moue$DATE <- as.character(moue$DATE)

moue <- moue %>%
  mutate(DATE = as.Date(paste0(DATE, '01'), format='%Y%m%d'))

#### extracting monthly average, confidence intervals 

#### Precipitation

moue <- mutate(moue, Year = year(DATE), Month = month(DATE))
climatemoue <- aggregate(moue[3], list(moue$Month), mean)
sdmoue <- aggregate(moue[3], list(moue$Month), sd)
countmoue <- aggregate(moue[3], list(moue$Month), length)
climatemoue <- climatemoue %>%
  mutate(sd = sdmoue[,2]) %>%
  mutate(n = countmoue[,2])
climatemoue$lowerci = with(climatemoue, RRE - qnorm(0.95)*sd/sqrt(n))
climatemoue$upperci = with(climatemoue, RRE + qnorm(0.95)*sd/sqrt(n))

moueclimateRRE <- climatemoue %>%
  ggplot()+
  geom_line(aes(x=Group.1,y=RRE),color = "blue4",size=1.4)+
  geom_line(aes(x=Group.1,y=lowerci),color = "blue4",linetype='dotdash',size=1.05)+
  geom_line(aes(x=Group.1,y=upperci),color = "blue4",linetype='dotdash',size=1.05)+
  ylim(0, 500)+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))

write.csv(climatemoue,file="precipitationMoue.csv",row.names=FALSE)

### Temperature

moue <- mutate(moue, Year = year(DATE), Month = month(DATE))
climatemoue <- aggregate(moue[12], list(moue$Month), mean,na.rm=TRUE)
sdmoue <- aggregate(moue[12], list(moue$Month), sd,na.rm=TRUE)
countmoue <- aggregate(moue[12], list(moue$Month), length)
climatemoue <- climatemoue %>%
  mutate(sd = sdmoue[,2]) %>%
  mutate(n = countmoue[,2])
climatemoue$lowerci = with(climatemoue, TM - qnorm(0.95)*sd/sqrt(n))
climatemoue$upperci = with(climatemoue, TM + qnorm(0.95)*sd/sqrt(n))


moueclimateMMT <- climatemoue %>%
  ggplot()+
  geom_line(aes(x=Group.1,y=TM),color = "brown2",size=1.4)+
  geom_line(aes(x=Group.1,y=lowerci),color = "brown2",linetype='dotdash',size=1.05)+
  geom_line(aes(x=Group.1,y=upperci),color = "brown2",linetype='dotdash',size=1.05)+
  ylim(18, 28)+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))

write.csv(climatemoue,file="temperatureMoue.csv",row.names=FALSE)

### Humidity

?na.action

moue <- mutate(moue, Year = year(DATE), Month = month(DATE))
climatemoue <- aggregate(moue[21], list(moue$Month),na.rm=T,mean)
sdmoue <- aggregate(moue[21], list(moue$Month), sd, na.rm=T)
countmoue <- aggregate(moue[21], list(moue$Month), length)
climatemoue <- climatemoue %>%
  mutate(sd = sdmoue[,2]) %>%
  mutate(n = countmoue[,2])
climatemoue$lowerci = with(climatemoue, UMM - qnorm(0.95)*sd/sqrt(n))
climatemoue$upperci = with(climatemoue, UMM + qnorm(0.95)*sd/sqrt(n))


moueclimateMMH <- climatemoue %>%
  ggplot()+
  geom_line(aes(x=Group.1,y=UMM),color = "deepskyblue3",size=1.4)+
  geom_line(aes(x=Group.1,y=lowerci),color = "deepskyblue3",size=1.05,linetype='dotdash')+
  geom_line(aes(x=Group.1,y=upperci),color = "deepskyblue3",size=1.05,linetype='dotdash')+
  ylim(74, 87.5)+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))


write.csv(climatemoue,file="humidityMoue.csv",row.names=FALSE)


### plots of raw data

moueprecipitation <- moue %>%
  ggplot(aes(x = DATE, y = RRE)) +
  geom_point() +
  geom_line() +
  scale_x_date(date_labels = "%y",date_breaks = "1 year")  +
  ylim(0, 900)+
  labs(y = "MP (mm)") + theme_bw(base_size = 15)
       

mouetemperature <- moue %>%
  ggplot(aes(x = DATE, y = TM)) +
  geom_point() +
  geom_line() +
  scale_x_date(date_labels = "%y",date_breaks = "1 year") +
  ylim(17.5, 30)+
  labs(y = "MMT(°C)") + theme_bw(base_size = 15)

mouehumidity <- moue %>%
  ggplot(aes(x = DATE, y = UMM)) +
  geom_point() +
  geom_line() +
  scale_x_date(date_labels = "%y",date_breaks = "1 year") +
  ylim(74,90)+
  labs(y = "MMH(%)") + theme_bw(base_size = 15)

##### Foret nord - riviere blanche

setwd("D:/disque dur/Work from home/Isotope/climatic data and analysis")

rblanche <- read.table("Rblanche.txt",header=T)

rblanche$DATE <- as.character(rblanche$DATE)

rblanche <- rblanche %>%
  mutate(DATE = as.Date(paste0(DATE, '01'), format='%Y%m%d'))


rblancheprecipitation <- rblanche %>%
  ggplot(aes(x = DATE, y = RRE)) +
  geom_point() +
  geom_line() +
  ylim(0, 900)+
  scale_x_date(date_labels = "%y",date_breaks = "1 year") +
  labs(y = "MP(mm)") + theme_bw(base_size = 15)

rblanchetemperature <- rblanche %>%
  ggplot(aes(x = DATE, y = TM)) +
  geom_point() +
  geom_line() +
  ylim(17.5, 30)+
  scale_x_date(date_labels = "%y",date_breaks = "1 year") +
  labs(y = "MMT °C") + theme_bw(base_size = 15)

rblanchehumidity <- rblanche %>%
  ggplot(aes(x = DATE, y = UMM )) +
  geom_point() +
  geom_line() +
  ylim(74,87.5)+
  scale_x_date(date_labels = "%y",date_breaks = "1 year") +
  labs(y = "MMH %") + theme_bw(base_size = 15)


### Precipitation

rblanche <- mutate(rblanche, Year = year(DATE), Month = month(DATE))
climaterb <- aggregate(rblanche[3], list(rblanche$Month), mean)
sdrb <- aggregate(rblanche[3], list(rblanche$Month), sd)
countrb <- aggregate(rblanche[3], list(rblanche$Month), length)
climaterb <- climaterb %>%
  mutate(sd = sdrb[,2]) %>%
  mutate(n = countrb[,2])
climaterb$lowerci = with(climaterb, RRE - qnorm(0.95)*sd/sqrt(n))
climaterb$upperci = with(climaterb, RRE + qnorm(0.95)*sd/sqrt(n))


rbclimateRRE <- climaterb %>%
  ggplot()+
  geom_line(aes(x=Group.1,y=RRE),color = "blue4",size=1.4)+
  geom_line(aes(x=Group.1,y=lowerci),color = "blue4",linetype='dotdash',size=1.05)+
  geom_line(aes(x=Group.1,y=upperci),color = "blue4",linetype='dotdash',size=1.05)+
  ylim(0, 500)+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))

#### Temperature

rblanche <- mutate(rblanche, Year = year(DATE), Month = month(DATE))
climaterb <- aggregate(rblanche[12], list(rblanche$Month), mean, na.rm=TRUE)
sdrb <- aggregate(rblanche[12], list(rblanche$Month), sd, na.rm=TRUE)
countrb <- aggregate(rblanche[12], list(rblanche$Month), length)
climaterb <- climaterb %>%
  mutate(sd = sdrb[,2]) %>%
  mutate(n = countrb[,2])
climaterb$lowerci = with(climaterb, TM - qnorm(0.95)*sd/sqrt(n))
climaterb$upperci = with(climaterb, TM + qnorm(0.95)*sd/sqrt(n))


rbclimateTM <- climaterb %>%
  ggplot()+
  geom_line(aes(x=Group.1,y=TM),color = "brown2",size=1.4)+
  geom_line(aes(x=Group.1,y=lowerci),color = "brown2",linetype='dotdash',size=1.05)+
  geom_line(aes(x=Group.1,y=upperci),color = "brown2",linetype='dotdash',size=1.05)+
  ylim(18, 28)+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))


#### humidity 

rblanche <- mutate(rblanche, Year = year(DATE), Month = month(DATE))
climaterb <- aggregate(rblanche[21], list(rblanche$Month), mean, na.rm=TRUE)
sdrb <- aggregate(rblanche[21], list(rblanche$Month), sd, na.rm=TRUE)
countrb <- aggregate(rblanche[21], list(rblanche$Month), length)
climaterb <- climaterb %>%
  mutate(sd = sdrb[,2]) %>%
  mutate(n = countrb[,2])
climaterb$lowerci = with(climaterb, UMM - qnorm(0.95)*sd/sqrt(n))
climaterb$upperci = with(climaterb, UMM + qnorm(0.95)*sd/sqrt(n))

rbclimateMMH <- climaterb %>%
  ggplot()+
  geom_line(aes(x=Group.1,y=UMM),color = "deepskyblue3",size=1.4)+
  geom_line(aes(x=Group.1,y=lowerci),color = "deepskyblue3",linetype='dotdash',size=1.05)+
  geom_line(aes(x=Group.1,y=upperci),color = "deepskyblue3",linetype='dotdash',size=1.05)+
  ylim(74, 90)+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))



require(gridExtra)
m1 <- moueprecipitation
m2 <- moueclimateRRE
m3 <- mouetemperature 
m4 <- moueclimateMMT
m5 <- mouehumidity
m6 <- moueclimateMMH
rb1 <- rblancheprecipitation
rb2 <- rbclimateRRE
rb3 <- rblanchetemperature
rb4 <- rbclimateTM
rb5 <- rblanchehumidity
rb6 <- rbclimateMMH


grid.arrange(moueprecipitation,moueclimateRRE,mouetemperature,moueclimateMMT,rblancheprecipitation,rbclimateRRE,rblanchetemperature,rbclimateTM,ncol=2)


vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)


grid.newpage()
pushViewport(viewport(layout = grid.layout(6, 3)))
print(m1, vp = vplayout(1, 1:2))
print(m2, vp = vplayout(1, 3))
print(m3, vp = vplayout(2, 1:2))
print(m4, vp = vplayout(2,3))
print(m5, vp = vplayout(3, 1:2))
print(m6, vp = vplayout(3,3))
print(rb1, vp = vplayout(4, 1:2))
print(rb2, vp = vplayout(4, 3))
print(rb3, vp = vplayout(5, 1:2))
print(rb4, vp = vplayout(5,3))
print(rb5, vp = vplayout(6, 1:2))
print(rb6, vp = vplayout(6,3))

