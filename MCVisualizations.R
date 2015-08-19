library(ggplot2)

#mc<-sapply(mc,as.character)
#mc<-sapply(mc,as.numeric)
#mc<-data.frame(mc)
#class(mc)

mc<-read.csv('ReducedColumbus.csv')
head(mc)
mc$Ltv.Sales.Adjusted.Margin.Dollars<-as.numeric(as.character(mc$Ltv.Sales.Adjusted.Margin.Dollars))
mc$Total.Amt<-as.numeric(as.character(mc$Total.Amt))
mc$Ltv.Sales<-as.numeric(as.character(mc$Ltv.Sales))
mc$Ltv.Margin.Dollars<-as.numeric(as.character(mc$Ltv.Margin.Dollars))

ggplot(aes(x=Total.Amt,y=Ltv.Sales.Adjusted.Margin.Dollars),
       data=mc)+
  coord_cartesian(xlim = c(0, 1100),ylim=c(-200,1000))+
  geom_point(alpha = 1/20,color='orange')+
  geom_line(stat='summary',fun.y = mean)

+
  geom_line(stat='summary',fun.y = quantile,prob=.25,
              linetype =2, color = 'blue')+
  geom_line(stat='summary',fun.y = quantile,prob=.75,
            linetype =2, color = 'blue')
str(mc)
names(mc)

summary(mc)
head(mctest)
as.numeric(as.character(mc))

#ggplot(aes(x=age,y=friend_count),data=pf) +
#  coord_cartesian(xlim = c(13, 70),ylim=c(0,1000))+
#  geom_point(alpha = 1/20,position=position_jitter(h=0),
#             color = 'orange')+
# geom_line(stat='summary',fun.y = mean)+
#  geom_line(stat='summary',fun.y = quantile,prob=.1,
#            linetype =2, color = 'blue')+
  geom_line(stat='summary',fun.y = quantile,prob=.5,
            color = 'blue')+
  geom_line(stat='summary',fun.y = quantile,prob=.9,
            linetype =2, color = 'blue')+
  ggtitle('Friend Count by Age')