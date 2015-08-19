library(ggplot2)
library(gridExtra)
library(dplyr)
#mc<-sapply(mc,as.character)
#mc<-sapply(mc,as.numeric)
#mc<-data.frame(mc)
#class(mc)

mc<-read.csv('ReducedColumbus.csv')
names(mc)
mc$Ltv.Sales.Adjusted.Margin.Dollars<-as.numeric(as.character(mc$Ltv.Sales.Adjusted.Margin.Dollars))
mc$Total.Amt<-as.numeric(as.character(mc$Total.Amt))
mc$Ltv.Sales<-as.numeric(as.character(mc$Ltv.Sales))
mc$Ltv.Margin.Dollars<-as.numeric(as.character(mc$Ltv.Margin.Dollars))

ggsave('FirstBasketValueVsHedgehogMargin.png')
ggplot(aes(x=Total.Amt,y=Ltv.Sales.Adjusted.Margin.Dollars),
       data=mc)+
  coord_cartesian(xlim = c(0, 1100),ylim=c(-200,1000))+
  geom_point(alpha = 1/30,color='orange')+
  xlab('Value of First Basket')+
  ylab('Hedgehog Margin')+
  geom_smooth(method=lm)+
  ggtitle('Hedgehog Margin vs Value of First Basket')

ggsave('FirstBasketValueVsLTVSales.png')
ggplot(aes(x=Total.Amt,y=Ltv.Sales),
       data=mc)+
  geom_point(alpha = 1/30,color='orange')+
  xlab('Value of First Basket')+
  ylab('LTV Sales')+
  geom_smooth(method=lm)+
  ggtitle('LTV Sales vs Value of First Basket')

ggsave('FirstBasketValueVsLTVMargin.png')
ggplot(aes(x=Total.Amt,y=Ltv.Margin.Dollars),
       data=mc)+
  coord_cartesian(xlim = c(0, 1100),ylim=c(-500,1000))+
  geom_point(alpha = 1/30,color='orange')+
  xlab('Value of First Basket')+
  ylab('LTV Margin')+
  geom_smooth(method=lm)+
  ggtitle('LTV Margin vs Value of First Basket')

ggsave('PurchasesCntVsHedghogMargin.png')
ggplot(aes(x=Ltv.Purchases.Cnt,y=Ltv.Sales.Adjusted.Margin.Dollars),
       data=mc)+
  coord_cartesian(xlim = c(0, 30),ylim=c(-500,1000))+
  geom_point(alpha = 1/30,color='orange')+
  xlab('Number of Purchases')+
  ylab('Hedgehog Margin')+
  ggtitle('Hedgehog Margin vs Number of Purchases')+
  geom_line(stat='summary',fun.y = quantile,prob=.75,
            linetype =2, color = 'blue')+
  geom_line(stat='summary',fun.y = quantile,prob=.25,
          linetype =2, color = 'blue')+
  geom_line(stat='summary',fun.y = mean)

ggsave('PurchasesCntVsLTVSales.png')
ggplot(aes(x=Ltv.Purchases.Cnt,y=Ltv.Sales),
       data=mc)+
  coord_cartesian(xlim = c(0, 20),ylim=c(-100,1000))+
  geom_point(alpha = 1/30,color='orange')+
  xlab('Number of Purchases')+
  ylab('LTV Sales')+
  ggtitle('LTV Sales vs Number of Purchases')+
  geom_line(stat='summary',fun.y = quantile,prob=.75,
            linetype =2, color = 'blue')+
  geom_line(stat='summary',fun.y = quantile,prob=.25,
            linetype =2, color = 'blue')+
  geom_line(stat='summary',fun.y = mean)

ggsave('PurchasesCntVsLTVMargin.png')
ggplot(aes(x=Ltv.Purchases.Cnt,y=Ltv.Margin.Dollars),
       data=mc)+
  coord_cartesian(xlim = c(0, 20),ylim=c(-500,1000))+
  geom_point(alpha = 1/30,color='orange')+
  xlab('Number of Purchases')+
  ylab('LTV Margin')+
  ggtitle('LTV Margin vs Number of Purchases')+
  geom_line(stat='summary',fun.y = quantile,prob=.75,
            linetype =2, color = 'blue')+
  geom_line(stat='summary',fun.y = quantile,prob=.25,
            linetype =2, color = 'blue')+
  geom_line(stat='summary',fun.y = mean)

names(mc)
ggsave('FirstBasketSizeVsHedghogMargin.png')
ggplot(aes(x=Sku,y=Ltv.Sales.Adjusted.Margin.Dollars),
       data=mc)+
  coord_cartesian(xlim = c(0, 25),ylim=c(-500,1000))+
  geom_point(alpha = 1/30,color='orange')+
  xlab('First Basket Size')+
  ylab('Hedgehog Margin')+
  ggtitle('Hedgehog Margin vs First Basket Size')+
  geom_line(stat='summary',fun.y = quantile,prob=.75,
            linetype =2, color = 'blue')+
  geom_line(stat='summary',fun.y = quantile,prob=.25,
            linetype =2, color = 'blue')+
  geom_line(stat='summary',fun.y = mean)

ggsave('FirstBasketSizeVsLTVMargin.png')
ggplot(aes(x=Sku,y=Ltv.Margin.Dollars),
       data=mc)+
  coord_cartesian(xlim = c(0, 25),ylim=c(-500,1000))+
  geom_point(alpha = 1/30,color='orange')+
  xlab('First Basket Size')+
  ylab('LTV Margin')+
  ggtitle('LTV Margin vs First Basket Size')+
  geom_line(stat='summary',fun.y = quantile,prob=.75,
            linetype =2, color = 'blue')+
  geom_line(stat='summary',fun.y = quantile,prob=.25,
            linetype =2, color = 'blue')+
  geom_line(stat='summary',fun.y = mean)

ggsave('FirstBasketSizeVsLTVSales.png')
ggplot(aes(x=Sku,y=Ltv.Sales),
       data=mc)+
  coord_cartesian(xlim = c(0, 20),ylim=c(-100,1000))+
  geom_point(alpha = 1/30,color='orange')+
  xlab('First Basket Size')+
  ylab('LTV Sales')+
  ggtitle('LTV Sales vs First Basket Size')+
  geom_line(stat='summary',fun.y = quantile,prob=.75,
            linetype =2, color = 'blue')+
  geom_line(stat='summary',fun.y = quantile,prob=.25,
            linetype =2, color = 'blue')+
  geom_line(stat='summary',fun.y = mean)




names(mc)
ggplot(aes(x=Total.Amt,y=Ltv.Sales.Adjusted.Margin.Dollars),
       data=mc)+
  coord_cartesian(xlim = c(0, 1100),ylim=c(-200,1000))+
  geom_point(alpha = 1/20,color='orange')+geom_smooth(method=lm)



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