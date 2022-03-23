df <- read.csv("Clean_Marketindicators.csv")
df2 <- df[,c(6,5,3,4)]

# Create Training and Test data ----
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(df2), 0.8*nrow(df2))  # row indices for training data
trainingData <- df2[trainingRowIndex, ]  # model training data
testData  <- df2[-trainingRowIndex, ]   # test data

#Measure correlation----
GGally::ggpairs(data=df2, columns=1:4, title="Market data") #,  ggplot2::aes(colour=index.value)


#Regression----
fit_1 <- lm(index.value ~ ExchangeRate + InterestRates , data = trainingData) #+ Inflation
summary(fit_1)

#Visualize residuals-----
# library(ggplot2)
# ggplot2::ggplot(data=df2, aes(fit_1$residuals)) +
#   geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
#   theme(panel.background = element_rect(fill = "white"),
#         axis.line.x=element_line(),
#         axis.line.y=element_line()) +
#   ggtitle("Histogram for Model Residuals")


# Calculate Relative Importance for Each Predictor---------------
library(relaimpo)
calc.relimp(fit_1,type=c("lmg","last","first","pratt"),
            rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples)
boot <- boot.relimp(fit_1, b = 1000, type = c("lmg",
                                            "last", "first", "pratt"), rank = TRUE,
                    diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort = TRUE)) # plot result


#Test goodness of fit of the model ---------
AIC(fit_1)

BIC(fit_1)


#Visualize the model----
plot(fit_1)


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


#Predicting the index----
# data.frame(InterestRates  = 8.5, Inflation = 7.357942, ExchangeRate = 85.3736)
predict(fit_1,testData)

# #Accuracy test----
# actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred))  # make actuals_predicteds dataframe.
# correlation_accuracy <- cor(actuals_preds)  # 82.7%
# head(actuals_preds)
#
# #> 31      50  50.717663
#
