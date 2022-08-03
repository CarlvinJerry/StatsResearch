library(knitr)
library(printr)
library(np)
library(earth)


safcom <- read.csv("Data/safcom.csv")
kable(head(safcom,10),align = 'c')


logSaf <- glm(safcom$Result ~ safcom$X.1 ,family="binomial")
summary(logSaf)

#Scatter plot of benchmark vs stock---
plot(log(safcom$Value),safcom$Returns, ylab="Safcom returns", xlab="NSE Index Value")


#Fit a linear model
linearModel= lm(safcom$Returns~log(safcom$X.1))
lines(log(safcom$Value),fitted(linearModel))
summary(linearModel)

marsFit <- earth::earth(safcom$Returns[-1], safcom$X.1[-1] )
marsFit
summary(marsFit)


quadModel = lm(safcom$Returns~safcom$X.1+I(safcom$X.1)^2)
lines(log(safcom$Value),fitted(quadModel))
summary(quadModel)



##Kernel regression
kernSmooth = npreg(safcom$Returns~safcom$X.1)
plot(kernSmooth)
points(log(safcom$X.1),safcom$Returns)
summary(kernSmooth)


#Fit kernel regression
kernelModelbandwidth = npregbw(safcom$Returns~safcom$Value)
kernelModel = npreg(kernelModelbandwidth)
summary(kernelModel)
summary(quadModel)



#loading the Splines Packages
require(splines)
#ISLR contains the Dataset
require(ISLR)
##attach(Wage)

agelims<-range(age)
#Generating Test Data
age.grid<-seq(from=agelims[1], to = agelims[2])


#######------------Spline smoother

plot(safcom$Returns,log(safcom$Value), xlab="Safcom returns", ylab="NSE Index Value")
mktLims <- range(safcom$Value)

#Generating Test Data
mkt.grid<-seq(from=mktLims[1], to = mktLims[2] )


#3 cutpoints at ages 150, 155,160
fit<-lm(Returns ~ bs(log(Value),knots = c(150, 155,160)),data = safcom )
summary(fit)
##


#Plotting the Regression Line to the scatterplot
plot(safcom$Returns,safcom$Value,col="grey",ylab="Market",xlab="Safcom")
points(mkt.grid,predict(fit,newdata = list(Value=mkt.grid)),col="darkgreen",lwd=2,type="l")
#adding cutpoints
abline(v=c(150, 155,160),lty=2,col="darkgreen")


#Testing MARS------
#Fit a linear model
linearModel= lm(safcom$Returns~log(safcom$X.1))
#lines(log(safcom$Value),fitted(linearModel))
summary(linearModel)

marsFit <- earth::earth(safcom$Returns[-1], safcom$X.1[-1] )
marsFit
summary(marsFit)

