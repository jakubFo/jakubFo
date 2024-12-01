library(caret)
library(xgboost)
library(dplyr)

xgboostmodel <- function(basic_table) {

  # Data Preprocessing: Filter and select necessary columns
  data_model <- basic_table %>%
    dplyr::filter(safety_equipment_group %in% c("None", "Helmet-related", "Other", "Lighting-related")) %>%
    dplyr::filter(!is.na(severity_category_pedestrian)) %>%
    dplyr::select(severity_category_pedestrian, safety_equipment_group, light_condition_category,
                  route_category, weather_category, surface_category, substance_category)

  # Convert categorical variables to factors
  data_model$safety_equipment_group <- as.factor(data_model$safety_equipment_group)
  data_model$light_condition_category <- as.factor(data_model$light_condition_category)
  data_model$route_category <- as.factor(data_model$route_category)
  data_model$weather_category <- as.factor(data_model$weather_category)
  data_model$surface_category <- as.factor(data_model$surface_category)
  data_model$substance_category <- as.factor(data_model$substance_category)

  # Ensure the outcome variable is a factor for binary classification
  data_model$severity_category_pedestrian <- as.factor(data_model$severity_category_pedestrian)

  levels(data_model$severity_category_pedestrian) <- make.names(levels(data_model$severity_category_pedestrian))

  # Remove rows with NA values in the target or features
  data_clean <- data_model %>% filter(!is.na(severity_category_pedestrian))

  # Prepare X (features) and y (target)
  X <- data_clean %>%
    select(safety_equipment_group, light_condition_category, route_category,
           weather_category, surface_category, substance_category) %>%
    data.matrix()

  y <- as.factor(data_clean$severity_category_pedestrian)  # Target as a factor for classification

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
