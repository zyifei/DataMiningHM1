# question 1
orange <- read.csv(file = "/Users/zhangyifei/Desktop/Semester_2/Data_Mining_for_Business_Analytics/Assignments/01/oj.csv")

orange_df = data.frame(orange)

# question 2
orange_row <- nrow(na.omit(orange_df))
orange_column <- ncol(na.omit(orange_df))

# question 3
summary(orange_df$price) 
# %min = 0.52 max = 3.870 mean = 2.282 %
oj_sd = sd(orange_df$price)
price_mean = mean(orange$price)

# question 4
oj_median_logmove = median(orange$logmove)

# question 5
oj_brand = unique(orange$brand)

oj_brand
# question 6
topicana = orange_df[ orange_df$brand == 'tropicana', ]
minute.maid = orange_df[ orange_df$brand == 'minute.maid', ]
dominicks = orange_df[ orange_df$brand == 'dominicks', ]

hist(topicana$price,
     ylim = c(0,1),
     freq=FALSE,
     main="Histogram for topicana price", 
     xlab="Price",
     col="palevioletred1", 
     breaks=5)

hist(minute.maid$price,
     ylim = c(0,1),
     freq=FALSE,
     main="Histogram for minute.maid price", 
     xlab="Price",
     col="royalblue2", 
     breaks=5)

hist(dominicks$price,
     freq=FALSE,
     main="Histogram for dominicks price", 
     xlab="Price",
     col="sienna", 
     breaks=5)


# question 7

boxplot(topicana$price, minute.maid$price,dominicks$price,
        col = c("palevioletred1","royalblue2","sienna"))
title(main="boxplot for each brand", col.main="black", font.main=4)
legend(2.5,3.9, # places a legend at the appropriate place c(“Health”,”Defense”), # puts text in the legend
       c("topicana","minute.maid","dominicks"),
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),
       col=c("palevioletred1","royalblue2","sienna"))

# question 9
plot(orange$logmove,orange$price, 
     col = orange$brand, ann = FALSE)
title(x="Logmove")
title(y="Price")
title(main="scatterplot of the logmove compared to price", col.main="black", font.main=4)
legend(4,3.9, # places a legend at the appropriate place c(“Health”,”Defense”), # puts text in the legend
       c("topicana","minute.maid","dominicks"),
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),
       col=c("green","red","black"))
# question 11
orange$week
orange$price

plot(tapply(orange$logmove, orange$week, mean),type = "l",col="blue",ann=FALSE)
title(x="Weeks")
title(y="Logmove")
title(main="Orange Juices - The mean price of orange juice sold each week", col.main="black", font.main=4)


# question 12
tapply(orange$logmove, INDEX=list(orange$week,orange$brand), mean)


# question 13
plot(tapply(orange$price, orange$week, mean),type = "l",ylim = c(0,4),col="green",ann=FALSE)
lines(topicana$price,type = "l", col = "palevioletred1")
lines(minute.maid$price,type = "l", col = "royalblue2")
lines(dominicks$price,type = "l",col = "sienna")
title(x="Weeks")
title(y="Price")
title(main="Comparing the mean weekly price of orange juice for all brands", col.main="black", font.main=4)
legend(80,1.5, # places a legend at the appropriate place c(“Health”,”Defense”), # puts text in the legend
       c("price","topicana","minute.maid","dominicks"),
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),
       col=c("green","palevioletred1","royalblue2","sienna"))

# question 14
  
oj_f = factor(orange$feat)
tapply(orange$logmove, oj_f, mean)

# %question 15%

withad=orange[orange$feat=="1",]
noad=orange[orange$feat=="0",]

meanlog_withad<-tapply(withad$logmove,withad$week,mean)
meanlog_noad<-tapply(noad$logmove,noad$week,mean)

plot(meanlog_withad, type="l",lwd=3,col="palevioletred1",ylim=c(7.5,12),ann=FALSE)
lines(meanlog_noad, type="l",col="royalblue2")
title(x="Weeks")
title(y="Logmove")
title(main="Orange Juices - Advertising Campaigns Effect on Sales", col.main="black", font.main=4)


  
sold_promotion <- c()
i = 1
for(i in 1:length(sold_no_promotion)){
  if(i == 1){
    sold_promotion[i]  <- sold_no_promotion[i]
  }else{
    sold_promotion[i] <- sold_no_promotion[i] + sold_promotion[i - 1]
  }
  i = i + 1
}


sold_no_promotion = tapply(orange$logmove, orange$week, mean)
s_p_df <- data.frame(sold_promotion)


plot(s_p_df$sold_promotion,type = 'o',col="blue")
lines(sold_no_promotion, type = 'o', col = "red")

unique(orange$store)
sale_performance = tapply(orange$CPWVOL5, orange$store, mean)
plot(tapply(orange$CPWVOL5, orange$INCOME, mean),type = 'o',col="blue")

max(tapply(orange$CPWVOL5, orange$store, mean))
max(sale_performance)
min(sale_performance)
sale_performance
best_sale_store <- orange_df[orange_df$store == '67',]
worse_sale_store <- orange_df[orange_df$store == '130',]

plot(best_sale_store$HVAL150, typy = 'o', ylim = c(0, 0.45),xlim = c(0,350),col = 'blue')
lines(worse_sale_store$HVAL150, typy = 'o',ylim = c(0, 0.45),xlim = c(0,350), col = 'red')



plot(best_sale_store$SSTRDIST, typy = 'o',ylim = c(0, 20), col = 'blue')
lines(worse_sale_store$SSTRDIST, type = 'o',ylim = c(0, 20), col = 'red')


orange_with_order <- orange[order(orange$CPWVOL5),]

plot(unique(orange_with_order$HVAL150),type = 'o',col="blue")
unique(orange_with_order$HVAL150)

plot(orange_with_order$INCOME, orange_with_order$HVAL150)
plot(orange_with_order$INCOME, orange_with_order$SSTRDIST)
plot(orange_with_order$INCOME, orange_with_order$CPDIST5)
plot(orange_with_order$INCOME, orange_with_order$WORKWOM)

plot(orange_with_order$CPWVOL5, orange_with_order$SSTRDIST)
plot(orange_with_order$INCOME, orange_with_order$SSTRDIST)
plot(orange$INCOME, orange$ETHNIC,pch=19)
plot(orange$INCOME, orange$EDUC,pch=19)
orange[orange$INCOME < 10,]


oj_logmove <- tapply(orange$logmove, orange$store, mean)
oj_income <- tapply(orange$INCOME, orange$store, mean)
oj_EDUC <- tapply(orange$EDUC, orange$store, mean)
oj_SSTRDIST <- tapply(orange$SSTRDIST, orange$store, mean)
oj_ETHNIC <- tapply(orange$ETHNIC, orange$store, mean)
oj_AGE60 <- tapply(orange$AGE60, orange$store, mean)
oj_HHLARGE <- tapply(orange$HHLARGE, orange$store, mean)
oj_CPDIST5 <- tapply(orange$CPDIST5, orange$store, mean)
AGE60
CPDIST5
HHLARGE
plot(oj_logmove, oj_income,pch=19, col= (orange$logmove > 9), xlim = c(3, 14), )
cor(oj_logmove,oj_CPDIST5)




# question 16
summary(orange$HVAL150)
orange_wealthy <- orange[orange$HVAL150>0.12,]
orange_poor <- orange[orange$HVAL150<0.12,]
plot(tapply(orange_wealthy$price, orange_wealthy$week,mean), type= "l",col="green", ylim = c(1.4, 10),ann=FALSE)
lines(tapply(orange_poor$price, orange_poor$week,mean),pch=19,col="palevioletred1", ylim = c(1.4, 10))

lines(tapply(orange_wealthy$logmove, orange_wealthy$week,mean), type= "l",col="red")
lines(tapply(orange_poor$logmove, orange_poor$week,mean),pch=19,col="royalblue2")
title(x="Weeks")
title(y="price and Logmove")
title(main="Orange Juices - HVAL150 Effect on Sales", col.main="black", font.main=4)

# the price in wealthy area 's price is higher than poor area

summary(orange$EDUC)
orange_EDUC <- orange[orange$EDUC>0.20,]
orange_no_EDUC <- orange[orange$EDUC<0.20,]
plot(tapply(orange_EDUC$price, orange_EDUC$week,mean), type= "l",col="blue", ylim = c(1.4, 10))
lines(tapply(orange_no_EDUC$price, orange_no_EDUC$week,mean),pch=19,col="palevioletred1", ylim = c(1.4, 10))

lines(tapply(orange_EDUC$logmove, orange_EDUC$week,mean), type= "l",col="yellow")
lines(tapply(orange_no_EDUC$logmove, orange_no_EDUC$week,mean),pch=19,col="green")

# the different of the price is not that significa for EDUC

summary(orange$AGE60)
orange_AGE60 <- orange[orange$AGE60>0.25,]
orange_no_AGE60 <- orange[orange$AGE60<0.25,]
plot(tapply(orange_AGE60$price, orange_AGE60$week,mean), type= "l",col="blue", ylim = c(1.4, 10))
lines(tapply(orange_no_AGE60$price, orange_no_AGE60$week,mean),pch=19,col="red", ylim = c(1.4, 10))

lines(tapply(orange_AGE60$logmove, orange_AGE60$week,mean), type= "l",col="yellow")
lines(tapply(orange_no_AGE60$logmove, orange_no_AGE60$week,mean),pch=19,col="green")

# the price does not show too much difference but the logmove is bigger for older group people
ETHNIC
summary(orange$ETHNIC)
orange_ETHNIC <- orange[orange$ETHNIC>0.15,]
orange_no_ETHNIC <- orange[orange$ETHNIC<0.15,]
plot(tapply(orange_ETHNIC$price, orange_ETHNIC$week,mean), type= "l",col="blue", ylim = c(1.4, 10))
lines(tapply(orange_no_ETHNIC$price, orange_no_ETHNIC$week,mean),pch=19,col="red", ylim = c(1.4, 10))

lines(tapply(orange_ETHNIC$logmove, orange_ETHNIC$week,mean), type= "l",col="yellow")
lines(tapply(orange_no_ETHNIC$logmove, orange_no_ETHNIC$week,mean),pch=19,col="green")

# no significa difference
INCOME
summary(orange$INCOME)
orange_INCOME <- orange[orange$INCOME>11,]
orange_low_INCOME <- orange[orange$INCOME<11,]
plot(tapply(orange_INCOME$price, orange_INCOME$week,mean), type= "l",col="blue", ylim = c(1.4, 10))
lines(tapply(orange_low_INCOME$price, orange_low_INCOME$week,mean),pch=19,col="red", ylim = c(1.4, 10))

lines(tapply(orange_INCOME$logmove, orange_INCOME$week,mean), type= "l",col="yellow")
lines(tapply(orange_low_INCOME$logmove, orange_low_INCOME$week,mean),pch=19,col="green")

HHLARGE
summary(orange$HHLARGE)
orange_HHLARGE <- orange[orange$HHLARGE>0.2,]
orange_low_HHLARGE <- orange[orange$HHLARGE<0.2,]
plot(tapply(orange_HHLARGE$price, orange_HHLARGE$week,mean), type= "l",col="blue", ylim = c(1.4, 10))
lines(tapply(orange_low_HHLARGE$price, orange_low_HHLARGE$week,mean),pch=19,col="red", ylim = c(1.4, 10))

lines(tapply(orange_HHLARGE$logmove, orange_HHLARGE$week,mean), type= "l",col="yellow")
lines(tapply(orange_low_HHLARGE$logmove, orange_low_HHLARGE$week,mean),pch=19,col="green")


WORKWOM
summary(orange$WORKWOM)
orange_WORKWOM <- orange[orange$WORKWOM>0.4,]
orange_low_WORKWOM <- orange[orange$WORKWOM<0.4,]
plot(tapply(orange_WORKWOM$price, orange_WORKWOM$week,mean), type= "l",col="blue", ylim = c(1.4, 10))
lines(tapply(orange_low_WORKWOM$price, orange_low_WORKWOM$week,mean),pch=19,col="red", ylim = c(1.4, 10))

lines(tapply(orange_WORKWOM$logmove, orange_WORKWOM$week,mean), type= "l",col="yellow")
lines(tapply(orange_low_WORKWOM$logmove, orange_low_WORKWOM$week,mean),pch=19,col="green")


# 

SSTRDIST
summary(orange$SSTRDIST)
orange_SSTRDIST <- orange[orange$SSTRDIST>6.65,]
orange_small_SSTRDIST <- orange[orange$SSTRDIST<6.65,]
plot(tapply(orange_SSTRDIST$price, orange_SSTRDIST$week,mean), type= "l",col="blue", ylim = c(1.4, 10))
lines(tapply(orange_small_SSTRDIST$price, orange_small_SSTRDIST$week,mean),pch=19,col="red", ylim = c(1.4, 10))

lines(tapply(orange_SSTRDIST$logmove, orange_SSTRDIST$week,mean), type= "l",col="yellow")
lines(tapply(orange_small_SSTRDIST$logmove, orange_small_SSTRDIST$week,mean),pch=19,col="green")

# the price is higher for the store which has longer distance to the nearest warehouse

CPDIST5
summary(orange$CPDIST5)
orange_CPDIST5 <- orange[orange$CPDIST5>1.62,]
orange_small_CPDIST5 <- orange[orange$CPDIST5<1.62,]
plot(tapply(orange_CPDIST5$price, orange_CPDIST5$week,mean), type= "l",col="blue", ylim = c(1.4, 10))
lines(tapply(orange_small_CPDIST5$price, orange_small_CPDIST5$week,mean),pch=19,col="red", ylim = c(1.4, 10))

lines(tapply(orange_CPDIST5$logmove, orange_CPDIST5$week,mean), type= "l",col="yellow")
lines(tapply(orange_small_CPDIST5$logmove, orange_small_CPDIST5$week,mean),pch=19,col="green")
# the price is higher when it is close to the nearest supermarket

CPWVOL5
summary(orange$CPWVOL5)
orange_CPWVOL5 <- orange[orange$CPWVOL5>0.27,]
orange_small_CPWVOL5 <- orange[orange$CPWVOL5<0.27,]
plot(tapply(orange_CPWVOL5$price, orange_CPWVOL5$week,mean), type= "l",col="blue", ylim = c(1.4, 10))
lines(tapply(orange_small_CPWVOL5$price, orange_small_CPWVOL5$week,mean),pch=19,col="red", ylim = c(1.4, 10))

lines(tapply(orange_CPWVOL5$logmove, orange_CPWVOL5$week,mean), type= "l",col="yellow")
lines(tapply(orange_small_CPWVOL5$logmove, orange_small_CPWVOL5$week,mean),pch=19,col="green")
# the samller ratio sale more then the bigger ratio??