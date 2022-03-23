library(quantmod)
library(ggplot2)
library(markovchain)
library(Markovchart)
library(diagram)
library(expm)
library(pracma)
library(dplyr)

# #Fetch data
# getSymbols("QQQ", from = "2017-01-01", to = "2017-11-06") #Data for the Nasdaq 100 index
# quantmod::chart_Series(QQQ)
# chart_Series(QQQ,TA='addBBands();addBBands(draw="p"); addVo();addMACD()', subset='2017',theme="white")
#
#
# #Calculate performance
# QQQ_log_returns <- dailyReturn(saf,type = 'log')

lrets <- log(lag(saf$MarketPrice)) - log(saf$MarketPrice)




df = data.frame(date = saf$MarketDate, lrets, row.names=NULL)

ggplot(df, aes(date, lrets)) + geom_line() + ylab("Saf Log Daily Return") +
  xlab("") + labs(title = "Saf Daily Return (Log)")

#Check if the performance has Normal dist
x <- df$lrets
h=hist(df$lrets,breaks= 20,col="grey",main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="red", lwd=3)


#Estimate returns with probability quantiles
probs <- c(.005, .025, .25, .5, .75, .975, .995)
dist_log_returns <- quantile(lrets,probs = probs, na.rm = TRUE)
dist_log_returns

# #mean & SD
# mean_log_returns <- mean(QQQ_log_returns, na.rm = TRUE)
# sd_log_returns <- sd(QQQ_log_returns, na.rm = TRUE)
# mean_log_returns
# sd_log_returns
# mean_returns = exp(mean_log_returns)
# mean_returns #On average, the mean daily return is 0.00116% more than the previous day’s price.
#------------===----------===----------------===--------------===---------------===----------===-------------===

# states <- c("Bullish", "Neutral", "Bearish")
# statesTransition <- matrix(c(03,0.3,0.4,0.4,0.4,0.2,0.5,0.3,0.2),byrow = T, nrow = 3,
#                            dimnames = list(states,states))
# mkchain <- new("markovchain", states = states,
#                byrow= T, transitionMatrix = statesTransition, name = "Price Movement")
#------------===----------===----------------===--------------===---------------===----------===-------------===
#Create states
returns<-round(df$lrets, 2)
Neutral=length(mysequence[mysequence == 0])
Bearish=length(mysequence[mysequence < 0])
Bullish=length(mysequence[mysequence > 0])




v2 <- ifelse(returns == 0 ,0, ifelse(returns < 0,-1, ifelse(returns > 0,1,NA)))






df$trend = cut(v2, breaks = 2)
mysequence<-df$trend

#create Sequence
#Transition Matrix---------------------
markovchain::createSequenceMatrix(mysequence)

myFit<-markovchainFit(data=mysequence,confidencelevel = .95,method= )
myFit

mF<-myFit$estimate
mF


a11=mF[1,1]
a12=mF[1,2]
a13=mF[1,3]
a21=mF[2,1]
a22=mF[2,2]
a23=mF[2,3]
a31=mF[3,1]
a32=mF[3,2]
a33=mF[3,3]
## Hard code the transition matrix-----------------------
stateNames <- c("Neutral","Bullish","Bearish")
transMat <- matrix(c(a11,a12,a13,a21,a22,a23,a31,a32,a33),nrow=3, byrow=T, dimnames =list (stateNames, stateNames))


dtmcA <- new("markovchain",transitionMatrix= transMat, states=stateNames, name="MarkovChain A")


dtmcA

plot(dtmcA)


##  Plot the transition matrix
plotmat(transMat,pos = c(1,1,1),
        lwd = 2, box.lwd = 2,
        cex.txt = 0.8,
        box.size = 0.12,
        box.type = "circle",
        box.prop = 0.7,
        box.col = "light blue",
        arr.length=.4,
        arr.width=.2,
        self.cex = .6,
        self.shifty = -.01,
        self.shiftx = .17,
        main = "Markov Chain Transition Matrix on QQQ")

#Current State
#Today QQQ is Up, that means starting vector (0,1,0)
initial_State_Matrix <- matrix(c(0,1,0),nrow=1, byrow=TRUE)


#Forecast n days
transMat2 <- transMat %^% 2
transMat3 <- transMat %^% 3
transMat4 <- transMat %^% 4
transMat5 <- transMat %^% 5
transMat6 <- transMat %^% 6
transMat7 <- transMat %^% 7


cat("Day 1 Forecast")
initial_State_Matrix%*% transMat

cat("Day 2 Forecast")
round(initial_State_Matrix%*% transMat2,2)



nDayProbs <-  function(days){
  #cat("",days,"Day Forecast")
  round(initial_State_Matrix%*% (transMat %^% days),8)
}
nDayProbs(10)

matrix(c(3,1,2,6), byrow=T , ncol = 2) %*% matrix(c(1,0,0,1), byrow=T , ncol = 2)
