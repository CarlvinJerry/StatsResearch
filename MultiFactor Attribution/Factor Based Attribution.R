library(tidyverse)
library(tidyquant)
#install.packages('quantmod)
library(quantmod)
#install.packages('dplyr)
library(dplyr)
#install.packages('infotheo)
library(infotheo)
#install.packages('caret')
library(caret)


# # get market data
# getSymbols(c("^GSPC"))
# head(GSPC)
# tail(GSPC)
#
# Ra <-GSPC c("GOOG") %>%
#   tq_get(get  = "stock.prices",
#          from = "2010-01-01",
#          to   = "2020-12-31") %>%
#   group_by(symbol)
# as.data.frame(Ra)%>%
#   tq_transmute(select     = adjusted,
#                mutate_fun = periodReturn,
#                period     = "monthly",
#                col_rename = "Ra")


factors <- read.csv("ff3Factors.csv")
tail(factors)

stockA <- P <- rnorm(1128, .01, .05)


stockdata <- cbind(factors,stockA)
stockdata <- stockdata %>% mutate(ExcessReturn = stockA - RF)


#Regress
lmHeight = lm(ExcessReturn ~ Mkt.RF+SMB+HML, data = stockdata) #Create the linear regression
summary(lmHeight)
names(stockdata)



stockdata$Mkt.RF

RiskFreeRate = 0.0157
MarketRiskCoeff <- 0.002975
MktPrem <- mean(stockdata$Mkt.RF)

SMBCoef <- 0.003073
SMBPrem <- mean(stockdata$SMB)

HMLCoef <- -0.004479
HMLPrem <- mean(stockdata$HML)

#Size
#Value

expectedRet <- RiskFreeRate + (MarketRiskCoeff * MktPrem) + (SMBCoef*SMBPrem) + (HMLCoef*HMLPrem)

