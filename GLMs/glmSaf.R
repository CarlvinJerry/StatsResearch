sharesTraded <- readr::read_csv("volumes.csv")
df<- head(sharesTraded,500)
m2 <- glm(Marketprice ~  SharesTraded, sharesTraded, family=poisson())
plot(m2)




set.seed(1234)
create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

data_train <- create_train_test(sharesTraded, 0.8, train = TRUE)
data_test <- create_train_test(sharesTraded, 0.8, train = FALSE)
dim(data_train)
dim(data_test)



fit <- glm(Marketprice ~  SharesTraded, data_train, family=poisson())



predict <- predict(fit, data_test, type = 'response')
# confusion matrix
tail(data_test)

m2 <- lm(Marketprice ~  SharesTraded, sharesTraded)
plot(m2)

plot(density(resid(fit,type='pearson')))
lines(density(resid(m2,type='response')), col='red')
summary(m2)

dev_res <- residuals(fit, type='deviance')
dev_res_std <- residuals(fit, type='deviance') / sqrt(1 - hatvalues(fit))

pearson_res <- residuals(fit, type='pearson')
pearson_res_std <- residuals(fit, type='pearson') / sqrt(1 - hatvalues(fit))

par(mfrow=c(1, 2))
plot(density(pearson_res), main='Deviance (red) v. Pearson Residuals', xlim=c(-5, 5))
lines(density(dev_res), col='red')

plot(density(pearson_res_std), main='Deviance Standardized (red) v.\n Pearson Standardized Residuals', xlim=c(-5, 5))
lines(density(dev_res_std), col='red')


par(mfrow=c(1, 1))
faraway::halfnorm(residuals(fit))


coef(fit)
