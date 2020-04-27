# Importing Data #
df <- datasets::mtcars #import data from R Repositories
data<-read.csv("E:/My Dictionary/Using R/Intermediate R/Melbourne_housing_FULL.csv", sep=",") #import external data
View(data)
dim(data)
summary(data)
setwd("E:/My Dictionary/Using R/Intermediate R/Output/")

# Pre-Processing DATA MANIPULATION #
#1. Inconsistent Data
str(data)
data<-data[-grep("#N/A",data$Distance),] #remove rows that contain #N/A
data<-data[-grep("#N/A",data$CouncilArea),]
write.csv(data,file="E:/My Dictionary/Using R/Intermediate R/Output/datan.csv")
datan<-read.csv("E:/My Dictionary/Using R/Intermediate R/Output/datan.csv")
datan<-datan[,-1]
datan$Date<-as.Date(datan$Date, "%d/%m/%y")
str(datan)
dim(datan)



#2. Missing Value
## Indentify Missing Value
summary(datan)
a<-data.frame(sum(is.na(datan[,1])))
colnames(a)<-"Jumlah missing value"
b<-data.frame(sum(is.na(datan[,2])))
colnames(b)<-"Jumlah missing value"
c<-rbind(a,b)
n<-length(datan)-2
for (i in 1:n)
{
  d<-data.frame(sum(is.na(datan[,i+2])))
  colnames(d)<-"Jumlah missing value"
  c<-rbind(c,d)
}
variabel<-data.frame(colnames(datan))
colnames(variabel)<-"variabel"
jumlahmv<-cbind(variabel,c) ;jumlahmv #the numbers of missing value

## Drop Missing Value
clean1<-na.omit(datan)
dim(clean1)
clean1$YearBuilt<-as.factor(clean1$YearBuilt)

sum(is.na(clean1))
write.csv(clean1,file="E:/My Dictionary/Using R/Intermediate R/Output/clean1.csv")

## Imputation Missing Value with Median dan Mode
clean2<-datan

meprice = median(clean2$Price,na.rm = TRUE)
clean2$Price[is.na(clean2$Price)] = 999999999
clean2$Price[clean2$Price==999999999]=meprice

mebed = median(clean2$Bedroom2,na.rm = TRUE)
clean2$Bedroom2[is.na(clean2$Bedroom2)] = 999999999
clean2$Bedroom2[clean2$Bedroom2==999999999]=mebed

mebath = median(clean2$Bathroom,na.rm = TRUE)
clean2$Bathroom[is.na(clean2$Bathroom)] = 999999999
clean2$Bathroom[clean2$Bathroom==999999999]=mebath

mecar = median(clean2$Car,na.rm = TRUE)
clean2$Car[is.na(clean2$Car)] = 999999999
clean2$Car[clean2$Car==999999999]=mecar

meland = median(clean2$Landsize,na.rm = TRUE)
clean2$Landsize[is.na(clean2$Landsize)] = 999999999
clean2$Landsize[clean2$Landsize==999999999]=meland

mebuilding = median(clean2$BuildingArea,na.rm = TRUE)
clean2$BuildingArea[is.na(clean2$BuildingArea)] = 999999999
clean2$BuildingArea[clean2$BuildingArea==999999999]=mebuilding

getmode<-function(v){
  uniqv<-unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
modeyear = getmode(clean2$YearBuilt[!is.na(clean2$YearBuilt)])
modeyear
clean2$YearBuilt[is.na(clean2$YearBuilt)] = 9999
clean2$YearBuilt[clean2$YearBuilt==9999]=modeyear
clean2$YearBuilt<-as.factor(clean2$YearBuilt)

mela = median(clean2$Lattitude,na.rm = TRUE)
clean2$Lattitude[is.na(clean2$Lattitude)] = 999999999
clean2$Lattitude[clean2$Lattitude==999999999]=mela

melong = median(clean2$Longtitude,na.rm = TRUE)
clean2$Longtitude[is.na(clean2$Longtitude)] = 999999999
clean2$Longtitude[clean2$Longtitude==999999999]=melong
sum(is.na(clean2))

str(clean2)

write.csv(clean2,file="E:/My Dictionary/Using R/Intermediate R/Output/clean2.csv")

# Exploratory Data Analysis #
str(clean1)
library(ggplot2)
theme_set(theme_light())
## HISTOGRAM
ggplot(clean1,aes(x=Price, fill=Type))+geom_histogram(color="white")
## BARCHART
ggplot(clean1,aes(x=Regionname,fill=Type))+geom_bar(color="black")
## PIE Chart
library(ggpubr)
summary(clean1$Type)
df = data.frame(group = c("u", "t", "h"),value = c(1540,722,6625))
df
labs<-paste0(df$group," (",df$value,")")
ggpie(df, "value", label = labs,lab.pos="in",lab.font="white",fill="group",color="white",palette = c("#00AFBB", "#E7B800", "#FC4E07"))
