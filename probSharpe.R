library(PerformanceAnalytics)

saf_scb <- read.csv("safRet.csv", stringsAsFactors = F)
PerformanceAnalytics::SharpeRatio.annualized((saf_scb[,5:6]))
returnsData <- xts::xts(saf_scb[, -1], order.by = as.Date(saf_scb[,1],"%m/%d/%Y"))

plot.ts(returnsData$Safcom)
plot.ts(returnsData$SCB)

PerformanceAnalytics::SharpeRatio.annualized(returnsData[,4:5],scale = 252)
PerformanceAnalytics::ProbSharpeRatio(returnsData[,4:5],refSR = c(-0,-0.7), ignore_skewness = FALSE, ignore_kurtosis = FALSE)



skewness(returnsData[,4:5])
kurtosis(returnsData[,4:5])
