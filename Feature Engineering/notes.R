# library(recipes)
# library(tidyverse)
# library(tidyquant)
# library(timetk)
# library(workflows)
# library(parsnip)
# library(yardstick)
# library(glmnet)
# #install.packages("timetk")
#
# Amazon <- FANG %>% filter(symbol == "AMZN")
#
# library(readxl)
# saf <- read_excel("saf.xlsx")
# View(saf)
#
# saf <- saf[,c(1,4)]
# # Augment (adds data frame columns)
# safTableAug<- saf %>%
#   tk_augment_timeseries_signature()
#
# head(safTableAug, 10)
#
# #Model
# fit_lm <- lm(MarketPrice ~ ., data = select(safTableAug, -c(MarketDate, diff)))
# summary(fit_lm)
#
#
#
#
#
# #Generate Feature data
# index <- saf %>% tk_index()
# future_idx <- index %>% tk_make_future_timeseries(length_out = 12)
# future_idx
#
# #Turn to signature
# new_data_tbl <- future_idx %>%
#   tk_get_timeseries_signature()
#
#
# #Predict new data
# # Make predictions
# pred <- predict(fit_lm, newdata = select(new_data_tbl, -c(index, diff)))
#
# predictions_tbl <- tibble(
#   date  = future_idx,
#   value = pred
# )
#
# predictions_tbl
#
#
# fit_lm <- lm(MarketPrice ~ Year + Month , data = select(new_data_tbl, -c(index, diff)))
# summary(fit_lm)
#
#
#
#
#
# new_data_tbl
#
# #Adding time series signatures---
# rec_obj <- recipe(adjusted ~ ., data = Amazon) %>%
#   step_timeseries_signature(date)
#
# # View the recipe object
# rec_obj
#
#
# #Generate new features
# View(bake(prep(rec_obj), Amazon))
#
#
#
# #  in this case, step 1 is the step_timeseries_signature step
# tidy(rec_obj)
#
# tidy(rec_obj, number = 1)
#
#
#
# recipe_spec_final <- rec_obj %>%
#   step_rm(date) %>%
#   step_rm(contains("iso"),
#           contains("second"), contains("minute"), contains("hour"),
#           contains("am.pm"), contains("xts")) %>%
#   step_normalize(contains("index.num"), date_year) %>%
#   step_interact(~ date_month.lbl * date_day) %>%
#   step_interact(~ date_month.lbl * date_mweek) %>%
#   step_interact(~ date_month.lbl * date_wday.lbl * date_yday) %>%
#   step_dummy(contains("lbl"), one_hot = TRUE)
#
# View(bake(prep(recipe_spec_final), new_data = Amazon))
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
