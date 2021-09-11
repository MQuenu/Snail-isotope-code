rm(list=ls())

library(ggplot2)
library(dplyr)
library(lubridate)
library(gridExtra)
library(tibble)
library(lattice)
library(grid)

options(stringsAsFactors = FALSE)

setwd("D:/Dropbox/Escargot/Isotope/climatic data and analysis")

#----- Precipitation ----

krre <- read.table("KaitaiaRREbis.txt",header=T)


### extracting and formatting dates from input files using lubridate and dplyr 
krre$DATE <- as.character(krre$Mon.YYYY.local.)
krre <- krre %>%
  mutate(DATE = as.Date(paste0(DATE, '01'), format='%Y%m%d'))
krre <- mutate(krre, Year = year(DATE), Month = month(DATE))

### computing average, standard deviations and counting number of observations for each month

climatekrre <- aggregate(krre[3], list(krre$Month), mean)  ## extract average
sd.krre <- aggregate(krre[3], list(krre$Month), sd) ## extract standard deviation
countkrre <- aggregate(krre[3], list(krre$Month), length) ## count number of measurements for each month

### using the three variables to calculate lower and upper 95% confidence interval, and store them in a dataframe
climatekrre <- climatekrre %>%
  mutate(sd = sd.krre[,2]) %>%
  mutate(n = countkrre[,2])
climatekrre$lowerci = with(climatekrre, RRE - qnorm(0.95)*sd/sqrt(n))
climatekrre$upperci = with(climatekrre, RRE + qnorm(0.95)*sd/sqrt(n))


## plotting and storing the 'climatology'
krreclimate <- climatekrre %>%
  ggplot()+
  geom_line(aes(x=Group.1,y=RRE),color = "blue4",size=1.4)+
  geom_line(aes(x=Group.1,y=lowerci),color = "blue4",linetype='dotdash',size=1.05)+
  geom_line(aes(x=Group.1,y=upperci),color = "blue4",linetype='dotdash',size=1.05)+
  ylim(0, 200)+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))


## plotting and storing the raw data
krrepreciplot <- krre %>%
  ggplot(aes(x = DATE, y = RRE)) +
  geom_point() +
  geom_line() +
  scale_x_date(date_labels = "%y",date_breaks = "1 year")  +
  ylim(0, 300)+
  labs(y = "MP (mm)") + theme_bw(base_size = 15)


#--- Temperature ---- 
# Same worklfow, but for two variables

# MDMinT

kmint <- read.table("KaitaiaMDminT.txt",header=T)

kmint$DATE <- as.character(kmint$MonY)
kmint <- kmint %>%
  mutate(DATE = as.Date(paste0(DATE, '01'), format='%Y%m%d'))
kmint <- mutate(kmint, Year = year(DATE), Month = month(DATE))
climatekmint <- aggregate(kmint[3], list(kmint$Month), mean)
sd.kmint <- aggregate(kmint[3], list(kmint$Month), sd)
countkmint <- aggregate(kmint[3], list(kmint$Month), length)
climatekmint <- climatekmint %>%
  mutate(sd = sd.kmint[,2]) %>%
  mutate(n = countkmint[,2])
climatekmint$lowerci = with(climatekmint, StatValue - qnorm(0.95)*sd/sqrt(n))
climatekmint$upperci = with(climatekmint, StatValue + qnorm(0.95)*sd/sqrt(n))


# MDMaxT  

kmaxt <- read.table("KaitaiaMDMaxT.txt",header=T)
kmaxt$DATE <- as.character(kmaxt$MonY)
kmaxt <- kmaxt %>%
  mutate(DATE = as.Date(paste0(DATE, '01'), format='%Y%m%d'))
kmaxt <- mutate(kmaxt, Year = year(DATE), Month = month(DATE))
climatekmaxt <- aggregate(kmaxt[3], list(kmaxt$Month), mean)
sd.kmaxt <- aggregate(kmaxt[3], list(kmaxt$Month), sd)
countkmaxt <- aggregate(kmaxt[3], list(kmaxt$Month), length)
climatekmaxt <- climatekmaxt %>%
  mutate(sd = sd.kmaxt[,2]) %>%
  mutate(n = countkmaxt[,2])
climatekmaxt$lowerci = with(climatekmaxt, StatValue - qnorm(0.95)*sd/sqrt(n))
climatekmaxt$upperci = with(climatekmaxt, StatValue + qnorm(0.95)*sd/sqrt(n))

# plots with two variables for temperature 

kplotT <- ggplot() +
  geom_point(data = kmaxt, aes(x=DATE,y=StatValue))+
  geom_line(data = kmaxt, aes(x=DATE,y=StatValue))+
  geom_point(data = kmint, aes(x=DATE,y=StatValue))+
  geom_line(data = kmint, aes(x=DATE,y=StatValue))+
  scale_x_date(date_labels = "%y",date_breaks = "1 year")  +
  ylim(0, 30)+
  labs(y = "T (°C)") + theme_bw(base_size = 15)
  
kclimateT <-  ggplot()+
  geom_line(data=climatekmint,aes(x=Group.1,y=StatValue),color = "brown2",size=1.4)+
  geom_line(data=climatekmint,aes(x=Group.1,y=lowerci),color = "brown2",linetype='dotdash',size=1.05)+
  geom_line(data=climatekmint,aes(x=Group.1,y=upperci),color = "brown2",linetype='dotdash',size=1.05)+
  geom_line(data=climatekmaxt,aes(x=Group.1,y=StatValue),color = "brown2",size=1.4)+
  geom_line(data=climatekmaxt,aes(x=Group.1,y=lowerci),color = "brown2",linetype='dotdash',size=1.05)+
  geom_line(data=climatekmaxt,aes(x=Group.1,y=upperci),color = "brown2",linetype='dotdash',size=1.05)+
  ylim(5,27)+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))

#------ Relative Humidity ---- 
# Again, same worklow

krh <- read.table("KaitaiaRH.txt",header=T)
krh$DATE <- as.character(krh$MonY)
krh <- krh %>%
  mutate(DATE = as.Date(paste0(DATE, '01'), format='%Y%m%d'))
krh <- mutate(krh, Year = year(DATE), Month = month(DATE))
climatekrh <- aggregate(krh[3], list(krh$Month), mean)
sd.krh <- aggregate(krh[3], list(krh$Month), sd)
countkrh <- aggregate(krh[3], list(krh$Month), length)
climatekrh <- climatekrh %>%
  mutate(sd = sd.krh[,2]) %>%
  mutate(n = countkrh[,2])
climatekrh$lowerci = with(climatekrh, StatValue - qnorm(0.95)*sd/sqrt(n))
climatekrh$upperci = with(climatekrh, StatValue + qnorm(0.95)*sd/sqrt(n))

krhclimate <- climatekrh %>%
  ggplot()+
  geom_line(aes(x=Group.1,y=StatValue),color = "deepskyblue2",size=1.4)+
  geom_line(aes(x=Group.1,y=lowerci),color = "deepskyblue2",linetype='dotdash',size=1.05)+
  geom_line(aes(x=Group.1,y=upperci),color = "deepskyblue2",linetype='dotdash',size=1.05)+
  ylim(70, 100)+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))

krhplot <- krh %>%
  ggplot(aes(x = DATE, y = StatValue)) +
  geom_point() +
  geom_line() +
  scale_x_date(date_labels = "%y",date_breaks = "1 year")  +
  ylim(70,100)+
  labs(y = "RH (%)") + theme_bw(base_size = 15)



#----- Grid arrange -----

## This is to represent the plots on the same figures
## Since using ggplot2, we have to use gridExtra 

require(gridExtra)

## this part is completely optional, I just store the plots in variables which are easier to call and manipulate 

k1 <- krrepreciplot
k2 <- krreclimate
k3 <- kplotT
k4 <- kclimateT
k5 <- krhplot
k6 <- krhclimate

## The following function authorize for a customized layout (2/3 : 1/3)

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 3)))
print(k1, vp = vplayout(1, 1:2))
print(k2, vp = vplayout(1, 3))
print(k3, vp = vplayout(2, 1:2))
print(k4, vp = vplayout(2,3))
print(k5, vp = vplayout(3, 1:2))
print(k6, vp = vplayout(3,3))

