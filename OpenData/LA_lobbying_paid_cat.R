require("dplyr")
require("ggplot2")
require('zoo')

# clear variables in environment
rm(list=ls())

# set path for data and script
setwd("/Users/powchow/data_science/OpenData")
data <- read.csv('Lobby_Pay_Categories.csv', header=TRUE)

data2014 = filter(data, Report.Year==2014) 
data_RDout = filter(data, Category != "Real Estate & Dev") 
data2013 = filter(data, Report.year==2013)

#group by categories to calculate change over years by Category
GRPdata <- group_by(data, Category)
GRPdata_cat <- arrange(summarise(GRPdata, total_paid=sum(Amount.Paid)), desc(total_paid))
#break out data by tiers 
GRPdata_catRD <- slice(GRPdata_cat,1)
GRPdata_cat2to15 <- slice(GRPdata_cat, 2:15)
GRPdata_cat16to31 <- slice(GRPdata_cat, 16:31)
GRPdata_catend <- slice(GRPdata_cat, 32:48)
  
GRPdata2 <- arrange(GRPdata, Report.Year, Report.Quarter)
GRPdata2 <- data.frame(GRPdata2)
GRPdata2$Report.Yr_Qrt <- paste(GRPdata2$Report.Year,GRPdata2$Report.Quarter, sep='-')
#GRPdata2$Report.Yr_Qrt <- as.Date(GRPdata2$Report.Yr_Qrt, format='%Y-m')
GRPdata3<- group_by(GRPdata2, Category, Report.Yr_Qrt)
data3 <- summarise(GRPdata3,
          total_paid = sum(Amount.Paid))

ggplot(data=subset(data3,Category=='Alcohol & Tobacco Related'),
       aes(x=Report.Yr_Qrt, y=total_paid, group=1,label=colnames(data3))) +
  geom_line() +
  ggtitle("Category: Alcohol & Tobacco Related") +
  geom_text()

GRPdata_yearvs <- summarise(GRPdata, total_paid = sum(Amount.Paid))

GRPdata2014 <- group_by(data2014, Category) #replace dataset
paid2014 <- summarise(GRPdata2014, count=n(), total_paid = sum(Amount.Paid))
pts <- pretty(paid2014$total_paid / 100000)
my.axis <-paste(pts,"MM",sep="")
axis(2, at = pts, labels =paste(pts,"MM",sep=""), las = 1)

#Create graph of circles -- amount paid by category
symbols(paid2014$count, paid2014$total_paid, circles=sqrt(paid2014$total_paid/ pi)/100000,
        inches=0.5, fg='white', bg='blue', xlab="Count Lobbying Firms", 
        ylab='Amount Paid')
title(main='Lobbying Payments to the City of LA by Category',
      cex.lab=0.75) 
text(paid2014$count, paid2014$total_paid, paid2014$Category,cex=0.35, offset=1)
dev.copy(png, file="lobbying_cat_paid2014.png", height=480, width=480)
dev.off()

#plot of data points with label/points -- still quite smashed as the circles
ggplot(data=paid2014,
       aes(x=count, y=total_paid, group=1,label=Category)) +
  geom_pts() +
  ggtitle("2014 Lobbyists Payments to LA City") +
  geom_text()

#################************** Used on Blog **********************************#####################
########### Create dataset & graph, wihtout Category Real Estate & Dev: Total donations over time (Year and Quarter)
GRPtime_RDout <- select(
  filter(data, Category != "Real Estate & Dev"), 
  Report.Year, Report.Quarter, Amount.Paid
  )
#GRPtime_RDout$Yr_Qrt <- as.yearqtr(paste(GRPtime_RDout$Report.Year, GRPtime_RDout$Report.Quarter, sep='-'),'%Y-%q') # unsupported type in summarise
GRPtime_RDout$Yr_Qrt <- paste(GRPtime_RDout$Report.Year, GRPtime_RDout$Report.Quarter, sep='-')

GRPdatatime <- summarise(
  group_by(GRPtime_RDout, Yr_Qrt), total_paid = sum(Amount.Paid))
ggplot(GRPdatatime, aes(x=Yr_Qrt, y=total_paid, group=1)) + 
  geom_line(colour='#FF00CC') + 
  labs(x='Year-Quarter', y= 'Total Payments') + 
  ggtitle('Lobbyist Payments to the City of Los Angeles - Exclude Real Estate & Dev')

#################************** Used on Blog **********************************#####################
########### Create dataset & graph, with all Categories: Total donations over time (Year and Quarter)
GRPtime <- select(data, Report.Year, Report.Quarter, Amount.Paid)
GRPtime$Yr_Qrt <- paste(GRPtime$Report.Year, GRPtime$Report.Quarter, sep='-')
GRPtime_all <- summarise(
  group_by(GRPtime, Yr_Qrt), total_paid = sum(Amount.Paid))

ggplot(GRPtime_all, aes(x=Yr_Qrt, y=total_paid, group=1)) + 
  geom_line(colour='#FF00CC') + 
  labs(x='Year-Quarter', y= 'Total Payments') + 
  ggtitle('Lobbyist Payments to the City of Los Angeles - All Categories')

#################************** Used on Blog **********************************#####################
############ Filter for specific categories, display comparisons

data2 = group_by(
  filter(data, Category== 'Sharing Economy' | Category=='Transportation Services'),
  Report.Year, Category)
len(dataSEvTS)
data_SEvTS = summarise(data2, total_paid = sum(Amount.Paid), count=n())

ggplot(data=data_SEvTS, aes(x=Category, y=total_paid, fill=as.factor(Report.Year))) +
  geom_bar(position='dodge',stat="identity") +
  labs(x='Name of Category', y='Amount Paid to Lobbying Firms') +
  ggtitle('Paid to Lobbying Firms by Year: Sharing vs. Transportation') +
  scale_fill_discrete(name='Report Year')
  


