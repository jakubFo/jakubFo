library(caret)
library(xgboost)
library(dplyr)

xgboostmodelSeverity <- function(basic_table, outcome) {

  # Data Preprocessing: Filter and select necessary columns
  data_model <- basic_table %>%
    dplyr::filter(!is.na(severity)) %>%
    dplyr::select(severity_category_pedestrian, safety_equipment_group, light_condition_category,
                  route_category, weather_category, surface_category, substance_category,
                  severity)

  # Convert categorical variables to factors
  data_model$safety_equipment_group <- as.factor(data_model$safety_equipment_group)
  data_model$light_condition_category <- as.factor(data_model$light_condition_category)
  data_model$route_category <- as.factor(data_model$route_category)
  data_model$weather_category <- as.factor(data_model$weather_category)
  data_model$surface_category <- as.factor(data_model$surface_category)
  data_model$substance_category <- as.factor(data_model$substance_category)

  # Ensure the outcome variable is numeric or factor with valid names
  data_model$severity <- factor(data_model$severity)

  # Make sure all factor levels are valid R variable names
  levels(data_model$severity) <- make.names(levels(data_model$severity))

  # Remove rows with NA values in the target or features
  data_clean <- data_model %>% filter(!is.na(severity))

  # Prepare X (features) and y (target)
  X <- data_clean %>%
    select(light_condition_category, route_category,
           weather_category, surface_category, substance_category) %>%
    data.matrix()

  y <- as.factor(data_clean$severity)  # Treat target as factor for classification

  # Split data into training and testing sets
  set.seed(123)
  train_index <- caret::createDataPartition(y, p = 0.8, list = FALSE)
  X_train <- X[train_index, ]
  X_test <- X[-train_index, ]
  y_train <- y[train_index]
  y_test <- y[-train_index]

  # Train XGBoost model using caret
  control <- trainControl(method = "cv", number = 2, savePredictions = "final", classProbs = TRUE)

  # Train the model using `train` from caret
  xgb_model <- train(
    x = X_train,
    y = y_train,
    method = "xgbTree",
    trControl = control,
    tuneLength = 2  # Automatically tune hyperparameters
  )


  # Predictions
  y_pred_class <- predict(xgb_model, newdata = X_test)

  # Confusion Matrix
  confusion_matrix <- confusionMatrix(y_pred_class, y_test)

  # Return the model and confusion matrix in a summary list
  summary <- list(
    xgb_model = xgb_model,
    confusion_matrix = confusion_matrix
  )

  return(summary)
}
