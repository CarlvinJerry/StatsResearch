library(recipes)
library(tidyverse)
library(tidyquant)
library(timetk)
library(workflows)
library(parsnip)
library(yardstick)
library(glmnet)




saf <- readxl::read_excel("saf.xlsx")
View(saf)

saf <- saf[,c(1,4)]
head(saf)
#Tests
train <- saf %>%
  filter(MarketDate  < ymd("2016-01-01"))
# Everything in 2016 will be used for comparing the output
actual_future <- saf %>%
  filter(MarketDate >= ymd("2016-01-01"))

saf_df

#Augment signatures
train <- tk_augment_timeseries_signature(train)
View(train)


#Model prices
fit_lm <- lm(MarketPrice ~ ., data = train[,-1])
summary(fit_lm)


# # Holidays
# holidays <- c("2016-01-01", "2016-01-18", "2016-02-15", "2016-03-25", "2016-05-30",
#               "2016-07-04", "2016-09-05", "2016-11-24", "2016-12-23", "2016-12-26",
#               "2016-12-30") %>%
#   ymd()

# Build new data for prediction: 3 Steps
new_data <- train %>%
  tk_index() %>%
  tk_make_future_timeseries(length_out = 1160,inspect_weekdays = TRUE) %>%
  tk_get_timeseries_signature()
# New data should look like this
View(new_data)




# Prediction using a linear model, fit_lm, on future index time series signature
pred_lm <- predict(fit_lm, newdata = new_data)




# Add predicted values to actuals data
actual_future <- actual_future %>%
  add_column(yhat2 = pred_lm)
# Plot using ggplot
actual_future %>%
  ggplot(aes(x = MarketDate)) +
  geom_line(aes(y = MarketPrice), data = train, color = palette_light()[[1]]) +
  geom_line(aes(y = MarketPrice), color = palette_light()[[1]]) +
  geom_line(aes(y = yhat2), color = palette_light()[[2]]) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Forecasting Safcom daily stock Prices",
       subtitle = "Linear Regression Model Applied to Time Series Signature",
       x = "",
       y = "Price",
       caption = "Safcom data") +
  theme_tq(base_size = 12)



