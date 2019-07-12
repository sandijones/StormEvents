library("tidyverse")
setwd("~/R/win-library/3.6")

 Storm2004 <- read.csv("2004StormEvents.csv", header=TRUE, stringsAsFactors = FALSE)
 Storm2005 <- read.csv("2005StormEvents.csv", header=TRUE, stringsAsFactors = FALSE)
 Storm2006 <- read.csv("2006StormEvents.csv", header=TRUE, stringsAsFactors = FALSE)
 Storm2007 <- read.csv("2007StormEvents.csv", header=TRUE, stringsAsFactors = FALSE)
 Storm2008 <- read.csv("2008StormEvents.csv", header=TRUE, stringsAsFactors = FALSE)
 Storm2009 <- read.csv("2009StormEvents.csv", header=TRUE, stringsAsFactors = FALSE)
 Storm2010 <- read.csv("2010StormEvents.csv", header=TRUE, stringsAsFactors = FALSE)
 Storm2011 <- read.csv("2011StormEvents.csv", header=TRUE, stringsAsFactors = FALSE)
 Storm2012 <- read.csv("2012StormEvents.csv", header=TRUE, stringsAsFactors = FALSE)
 Storm2013 <- read.csv("2013StormEvents.csv", header=TRUE, stringsAsFactors = FALSE)
 Storm2014 <- read.csv("2014StormEvents.csv", header=TRUE, stringsAsFactors = FALSE)
 Storm2015 <- read.csv("2015StormEvents.csv", header=TRUE, stringsAsFactors = FALSE)
 Storm2016 <- read.csv("2016StormEvents.csv", header=TRUE, stringsAsFactors = FALSE)
 Storm2017 <- read.csv("2017StormEvents.csv", header=TRUE, stringsAsFactors = FALSE)
 Storm2018 <- read.csv("2018StormEvents.csv", header=TRUE, stringsAsFactors = FALSE)

 dim(Storm2004)+ dim(Storm2005)+dim(Storm2006)+dim(Storm2007)+dim(Storm2008)+dim(Storm2009)+dim(Storm2010) +dim(Storm2011)+dim(Storm2012)+dim(Storm2013)+dim(Storm2014)++dim(Storm2015)+dim(Storm2016)+dim(Storm2017)+dim(Storm2018)

 Storm <- do.call("rbind", list(Storm2004, Storm2005,Storm2006, Storm2007, Storm2008, Storm2009, Storm2010,
              Storm2011, Storm2012, Storm2013, Storm2014, Storm2015, Storm2016, Storm2017,
              Storm2018))
 write.csv(StormTemp,"~/R/win-library/3.6/Storm.csv", row.names = FALSE) 
 StormTemp <- Storm
write.csv(StormTemp,"~/R/win-library/3.6/StormTemp.csv", row.names = FALSE)
dim(StormTemp)

for (i in 1:nrow(StormTemp)){

damage <- toString(StormTemp[i, "DAMAGE_PROPERTY"])
year <- (StormTemp[i, "YEAR"])
date <- (StormTemp[i, "BEGIN_DATE_TIME"])

#Remove units from DAMAGE_PROPERTY
if (nchar(damage)==6 & str_sub(damage,6,6) %in% c("K", "M", "B")){
  case <- str_sub(damage,6,6)
  damage <- as.numeric(str_sub(damage, 1, 5))

}else if (nchar(damage==5) & str_sub(damage,5,5)%in%c("K", "M", "B")){
  case <- str_sub(damage,5,5)
  damage <- as.numeric(str_sub(damage, 1, 4))

}else if (nchar(damage)==4 & str_sub(damage,4,4)%in%c("K", "M", "B")){
  case <- str_sub(damage,4,4)
  damage <- as.numeric(str_sub(damage, 1, 3))

}else if (nchar(damage)==3 & str_sub(damage,3,3)%in%c("K", "M", "B")){
  case <- str_sub(damage,3,3)
  damage <- as.numeric(str_sub(damage, 1, 2))

}else if (nchar(damage)==2 & str_sub(damage,2,2)%in%c("K", "M", "B")){
  case <- str_sub(damage,2,2)
  damage <- as.numeric(str_sub(damage, 1, 1))

}else{
  case <- "X"
  damage <- as.numeric(damage)

}

#Adjust for inflation using CPI
if(year=="2004"){
  CPI_Factor <- 1.32
}else if(year=="2005"){
  CPI_Factor <- 1.28
}else if(year=="2006"){
  CPI_Factor <- 1.24
}else if(year=="2007"){
  CPI_Factor <- 1.2
}else if(year=="2008"){
  CPI_Factor <- 1.2
}else if(year=="2009"){
  CPI_Factor <- 1.16
}else if(year=="2010"){
  CPI_Factor <- 1.15
}else if(year=="2011"){
  CPI_Factor <- 1.11
}else if(year=="2012"){
  CPI_Factor <- 1.09
}else if(year=="2013"){
  CPI_Factor <- 1.08
}else if(year=="2014"){
  CPI_Factor <- 1.07
}else if(year=="2015"){
  CPI_Factor <- 1.06
}else if(year=="2016"){
  CPI_Factor <- 1.04
}else if(year=="2017"){
  CPI_Factor <- 1.02
}else{
  CPI_Factor <- 1
}

if(case=="K"){
  damage <- damage*1000*CPI_Factor

}else if(case=="M"){
  damage <- damage*1000000*CPI_Factor

}else if(case=="B"){
  damage <- damage*1000000000*CPI_Factor

}else{
  damage <- damage*CPI_Factor
}
StormTemp[i, "DAMAGE_PROPERTY"] <- damage
StormTemp[i, "DATE"] <- as.Date(str_sub(date, 1, 10), format="%m/%d/%Y")

}
save(StormTemp,file="StormTempData")
write.csv(StormTemp,"~/R/win-library/3.6/StormTemp15.csv", row.names = FALSE)
StormFinal <- StormTemp

# Combine Hurricane and Hurricane (Typhoon)
for (j in 1:nrow(StormFinal)){  
event <- toString(StormFinal[j, "EVENT_TYPE"])

if (event %in% c("Hurricane (Typhoon)", "Hurricane"))  {
  event="Hurricane"
}
StormFinal[j, "EVENT_TYPE"] <- event
}
save(StormFinal,file="StormFinalData")

#Summary Stats
summary(StormFinal)
mean(as.numeric(!is.na(StormFinal$DAMAGE_PROPERTY)))
sd(as.numeric(!is.na(StormFinal$DAMAGE_PROPERTY)))

#Visualizations
# EVENT_TYPE by Sum DAMAGE_PROPERTY
StormFilter <- StormFinal %>% filter(EVENT_TYPE == "Hurricane", !is.na(DAMAGE_PROPERTY)) %>%  group_by(EVENT_TYPE, YEAR) %>%
  summarize(DAMAGE_TOTAL=sum(as.numeric(DAMAGE_PROPERTY)))
view(StormFilter)

# Bar graph Hurricane DAMAGE_PROPERTY per year
StormFilter <- StormFinal %>% filter(EVENT_TYPE == "Hurricane", !is.na(DAMAGE_PROPERTY)) %>%  group_by(EVENT_TYPE, YEAR)
view(StormFilter)

ggplot(data = StormFilter) +
  geom_bar(mapping = aes(x = YEAR))+
  ggtitle("Damage per Hurricane") +
  xlab("Hurricane") + ylab("Damage amount")

# Scatterplot Hurricane count by year
StormFilter <- StormFinal %>% filter(EVENT_TYPE == "Hurricane") %>%  group_by(YEAR, EVENT_TYPE) %>%
    summarise(CountEvent=(count=n()))
view(StormCountType)

ggplot(data = StormFilter) + 
  geom_point(mapping=aes(x=YEAR, y=CountEvent, color=EVENT_TYPE))+
  ggtitle("number of hurricanes")

# Bargraph of EVENT_TYPE
StormFilter <- StormFinal %>% filter(EVENT_TYPE %in% c("Hurricane", "Flood", "Flash Flood")) %>% 
  select( STATE, YEAR, MONTH_NAME, EVENT_TYPE, DAMAGE_PROPERTY)

view(StormFilter)
ggplot(data = StormFilter) + 
  geom_bar(mapping = aes(x = EVENT_TYPE,fill=EVENT_TYPE))

# Bargraph of Hurricanes per month
StormFilter <- StormFinal %>% filter(EVENT_TYPE == "Hurricane")

ggplot(data = StormFilter) + 
  geom_bar(mapping = aes(x = str_sub(BEGIN_YEARMONTH,5,7)))+
  ggtitle("Hurricanes Per Month") +
  xlab("Month") + ylab("Number of Hurricanes")

#Storm Count by EVENT_TYPE
StormCountType <- StormFinal %>% filter(EVENT_TYPE %in% c("Hurricane", "Flood", "Flash Flood")) %>%  group_by(EVENT_TYPE) %>%
  select( YEAR, EVENT_TYPE) %>% summarise(count=n())
head(StormCountType)

