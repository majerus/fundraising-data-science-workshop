# load packages
library(tidyverse)
library(moderndive)
library(caret)
library(leaps)

# remove extra variables
glimpse(df)

model_df <- 
  df %>% 
  select(-name, -zip, -latitude, -longitude, -city, -state)

# create train and test data sets
train <- sample_frac(model_df, size = .8)
test <- 
  model_df %>% 
  filter(!id %in% train$id)

# drop id from train and test
train$id <- NULL
test$id <- NULL

# starter model example
model_1 <- 
  lm(af18 ~ af17 + af16 + class, data = train)

get_regression_table(model_1)
get_regression_points(model_1)

# Sum of squared residuals
get_regression_points(model_1) %>%
  mutate(sq_residuals = residual^2) %>%
  summarize(sum_sq_residuals = sum(sq_residuals))

# R^2
get_regression_points(model_1) %>%
  summarize(r_squared = 1 - var(residual) / var(af18))

# MSE
get_regression_points(model_1) %>%
  mutate(sq_residuals = residual^2) %>%
  summarize(mse = mean(sq_residuals))

# RMSE
get_regression_points(model_1) %>%
  mutate(sq_residuals = residual^2) %>%
  summarize(mse = mean(sq_residuals)) %>%
  mutate(rmse = sqrt(mse))


# stepwise approach 
# http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/

# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 2)

# Train the model
step.model <- train(af18 ~ ., data = train,
                    method = "leapForward", 
                    tuneGrid = data.frame(nvmax = 1:12),
                    trControl = train.control
)

# see results of stepwise approach
step.model$results

# which model is best?
step.model$bestTune

# what variables are in the best model?
summary(step.model$finalModel)

# what are these variables coefficients?
coef(step.model$finalModel, step.model$bestTune[1,1])

# replicate starter approach with new model
model_2 <- lm(af18 ~ af14 + af15 + af16 + af17, train)

# test model on test data
get_regression_points(model_2, newdata = test)

get_regression_points(model_2, newdata = test) %>%
  summarize(r_squared = 1 - var(residual) / var(af18))

# alternative using the predict function
predict(model_2, newdata = test)



# keep this
df %>% 
  mutate(predicted_giving = predict(model_2, newdata = df)) %>% 
  select(af18, predicted_giving)

