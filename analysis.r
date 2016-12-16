library(AER)

cig.price.data <- read.csv("~/Dropbox/Econometrics/FinalProject/cigdata.csv")

cig.price.data$rtaxso <- cig.price.data$racpp + cig.price.data$rgstpp

cig.96 <- subset(cig.price.data,subset = (year==1996))
cig.14 <- subset(cig.price.data,subset = (year==2014))

#Cross Sectional Regressions
reg.12.11 <- ivreg(log(cc) ~ log(racpp) | rtaxso, data=cig.14)
summary(reg.12.11)

reg.12.15 <- ivreg(log(cc) ~ log(racpp) + log(rpcpi) | rtaxso + log(rpcpi), data=cig.14)
summary(reg.12.15)

reg.12.16 <- ivreg(log(cc) ~ log(racpp) + log(rpcpi) | rgstpp + log(rpcpi), data=cig.14)
summary(reg.12.16)

reg.extra <- ivreg(log(cc) ~ log(racpp) + log(rpcpi) | rgstpp + rtaxso + log(rpcpi), data=cig.14)
summary(reg.extra)

cigp <- merge(cig.96, cig.14, by="name", suffixes = c(".86", ".14"))

#Take the logs
cigp$log.cc.86 <- log(cigp$cc.86)
cigp$log.cc.14 <- log(cigp$cc.14)

cigp$log.racpp.86 <- log(cigp$racpp.86)
cigp$log.racpp.14 <- log(cigp$racpp.14)

cigp$log.rpcpi.86 <- log(cigp$rpcpi.86)
cigp$log.rpcpi.14 <- log(cigp$rpcpi.14)

#Take the differences

cigp$diff.log.cc <- cigp$log.cc.14 - cigp$log.cc.86
cigp$diff.log.racpp <- cigp$log.racpp.14 - cigp$log.racpp.86
cigp$diff.log.rpcpi <- cigp$log.rpcpi.14 - cigp$log.rpcpi.86
cigp$diff.rtaxso <- cigp$rtaxso.14 - cigp$rtaxso.86
cigp$diff.rgstpp <- cigp$rgstpp.14 - cigp$rgstpp.86

#Long Run Regressions

reg.12.1 <- ivreg(diff.log.cc ~ diff.log.racpp + diff.log.rpcpi | diff.rtaxso + diff.log.rpcpi, data=cigp)
summary(reg.12.1)

reg.12.2 <- ivreg(diff.log.cc ~ diff.log.racpp + diff.log.rpcpi | diff.rgstpp + diff.log.rpcpi, data=cigp)
summary(reg.12.2)

reg.12.3 <- ivreg(diff.log.cc ~ diff.log.racpp + diff.log.rpcpi | diff.rgstpp + diff.rtaxso + diff.log.rpcpi, data=cigp)
summary(reg.12.3)

#Short Run Analysis

cig.13 <- cig.96 <- subset(cig.price.data,subset = (year==2013))
cig.short <- merge(cig.96, cig.14, by="name", suffixes = c(".13", ".14"))

#Take the logs
cig.short$log.cc.13 <- log(cig.short$cc.13)
cig.short$log.cc.14 <- log(cig.short$cc.14)

cig.short$log.racpp.13 <- log(cig.short$racpp.13)
cig.short$log.racpp.14 <- log(cig.short$racpp.14)

cig.short$log.rpcpi.13 <- log(cig.short$rpcpi.13)
cig.short$log.rpcpi.14 <- log(cig.short$rpcpi.14)

#Take the differences

cig.short$diff.log.cc <- cig.short$log.cc.14 - cig.short$log.cc.13
cig.short$diff.log.racpp <- cig.short$log.racpp.14 - cig.short$log.racpp.13
cig.short$diff.log.rpcpi <- cig.short$log.rpcpi.14 - cig.short$log.rpcpi.13
cig.short$diff.rtaxso <- cig.short$rtaxso.14 - cig.short$rtaxso.13
cig.short$diff.rgstpp <- cig.short$rgstpp.14 - cig.short$rgstpp.13

#Short Run Regressions

short.1 <- ivreg(diff.log.cc ~ diff.log.racpp + diff.log.rpcpi | diff.rtaxso + diff.log.rpcpi, data=cig.short)
summary(short.1)
short.2 <- ivreg(diff.log.cc ~ diff.log.racpp + diff.log.rpcpi | diff.rtaxso + diff.log.rpcpi, data=cig.short)
summary(short.2)
short.3 <-ivreg(diff.log.cc ~ diff.log.racpp + diff.log.rpcpi | diff.rtaxso + diff.log.rpcpi, data=cig.short)
summary(short.3)
