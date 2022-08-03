#loading the Splines Packages
require(splines)
#ISLR contains the Dataset
require(ISLR)
attach(Wage)

agelims<-range(age)
#Generating Test Data
age.grid<-seq(from=agelims[1], to = agelims[2])

##Cubic Spline 3 Knots----
#transforms the Regression Equation by transforming the Variables with a truncated Basis Function-b(x),with continious derivatives upto order 2.
#The order of the continuity=(d−1), where d is the number of degrees of polynomial
#The Regression Equation: - f(x)=yi=α+β1.b1(xi) +β2.b2(xi) + ....βk+3.bk+3(xi) +ϵi where bn(xi) is The Basis Function
## transform the variables and add a linear combination of the variables using the Basis power function to the regression function f(x).

#3 cutpoints at ages 25 ,50 ,60
fit<-lm(wage ~ bs(age,knots = c(25,40,60)),data = Wage )
summary(fit)


#Plotting the Regression Line to the scatterplot
plot(age,wage,col="grey",xlab="Age",ylab="Wages")
points(age.grid,predict(fit,newdata = list(age=age.grid)),col="darkgreen",lwd=2,type="l")
#adding cutpoints
abline(v=c(25,40,60),lty=2,col="blue")



##Smoothing Splines no Knots----
#does not require the selection of the number of Knots , but require
#selection of only a Roughness Penalty which accounts for the wiggliness(fluctuations) and controls the roughness of the function and variance of the Model.
#Let the RSS(Residual Sum of Squares) be g(xi)

#minimize g∈RSS: ∑i=1n( yi − g(xi) )^2+λ ∫g′′(t)^2dt,λ>0
#where , λ∫g′′(t)^2dt = Roughness Penalty.

fit1<-smooth.spline(age,wage,df=10)
plot(age,wage,col="grey",xlab="Age",ylab="Wages")
points(age.grid,predict(fit,newdata = list(age=age.grid)),col="darkgreen",lwd=2,type="l")
#adding cutpoints
abline(v=c(25,40,60),lty=2,col="darkgreen")
lines(fit1,col="red",lwd=2)
legend("topright",c("Smoothing Spline with 16 df","Cubic Spline"),col=c("red","darkgreen"),lwd=2)


