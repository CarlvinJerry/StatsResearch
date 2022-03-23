df <- read.csv("Clean_Marketindicators.csv")
df2 <- df[,c(6,5,3,4)]
#Measure correlation----
GGally::ggpairs(data=df2, columns=1:4, title="Market data") #,  ggplot2::aes(colour=index.value)


#---------------------logistic non linear model
library(dplyr)
library(ggplot2)
a <- df2$index.value
index.direction <-ifelse(lag(a) < a,1,0)
df2$index.direction
df3<-cbind(df2,index.direction)
glm.data <- df3[,c(5,2,3,4)]

glm.data[is.na(glm.data)] <- 1
GGally::ggpairs(data=glm.data, columns=1:4, title="Market data")

logit.model <- glm(index.direction ~ Inflation+ ExchangeRate+ InterestRates,family = binomial, #(link = "logit")
                   data = glm.data)

#predictive model------
predict.glm(logit.model,data.frame(Inflation = 7.357942,ExchangeRate = 85.3736,InterestRates  = 8.5))

library(ggplot2)
ggplot(glm.data, aes(x=Inflation, y=index.direction)) + geom_point() +
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

exp(coef(logit.model))

summary(logit.model)
library(ResourceSelection)
hoslem.test(glm.data$index.direction , fitted(logit.model))

#featurePlot(x = glm.data[, 1:4],
#           y = glm.data$index.direction,
#          plot = "box",
#         strip=strip.custom(par.strip.text=list(cex=.7)),
#        scales = list(x = list(relation="free"),
#                     y = list(relation="free")))
#Regression----
# Logistics Regression
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial)
fit_1 <- glm(index.value ~ ExchangeRate + InterestRates + Inflation, data = df2, family = poisson)
summary(fit_1)

#Visualize residuals-----
library(ggplot2)
ggplot2::ggplot(data=df2, aes(fit_1$residuals)) +
  geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Model Residuals")

#Visualize the model----
a <- ggplot(data = df2, aes(x = index.value, y = c(InterestRates))) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Interest Rates Data")

b <- ggplot(data = df2, aes(x = index.value, y = c(ExchangeRate ))) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to ExchangeRate Rates Data")

c <- ggplot(data = df2, aes(x = index.value, y = c(Inflation   ))) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Inflation  Data")


figure <- ggpubr::ggarrange(a, b, c,
                            labels = c("A", "B", "C"),
                            ncol = 2, nrow = 2)
figure


#Predicting the index
predict(fit_1, data.frame(InterestRates  = 8.5, Inflation = 7.357942, ExchangeRate = 85.3736))

DataExplorer::create_report(df2)
