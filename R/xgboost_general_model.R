library(caret)
library(dplyr)

xgboost_general_model <- function(basic_table, predictors, outcome, filter_condition = NULL, filter_na = TRUE) {
  # Apply optional filtering condition
  if (!is.null(filter_condition)) {
    basic_table <- basic_table %>% dplyr::filter(!!rlang::parse_expr(filter_condition))
  }

  # Ensure outcome variable is not NA
  if (filter_na) {
    basic_table <- basic_table %>% dplyr::filter(!is.na(!!sym(outcome)))
  }

  # Select predictors and outcome
  data_model <- basic_table %>% dplyr::select(all_of(c(predictors, outcome)))

  # Convert categorical variables to factors
  data_model <- data_model %>%
    dplyr::mutate(across(where(is.character), as.factor))

  # Ensure the outcome levels are valid R variable names
  valid_levels <- make.names(levels(data_model[[outcome]]))
  levels(data_model[[outcome]]) <- valid_levels

  # Ensure the outcome is treated as a factor for classification
  data_model[[outcome]] <- factor(data_model[[outcome]], levels = valid_levels)

  # Remove rows with NA values in predictors or outcome
  data_model <- data_model %>% dplyr::filter(complete.cases(.))

  # Train-test split
  set.seed(123)
  train_index <- caret::createDataPartition(data_model[[outcome]], p = 0.8, list = FALSE)
  train_data <- data_model[train_index, ]
  test_data <- data_model[-train_index, ]

  # Train XGBoost model using caret
  control <- trainControl(method = "cv", number = 2, savePredictions = "final", classProbs = TRUE)
  xgb_model <- train(
    reformulate(predictors, response = outcome),
    data = train_data,
    method = "xgbTree",
    trControl = control,
    tuneLength = 2
  )

  # Predictions and evaluation
  y_pred_class <- predict(xgb_model, test_data)
  confusion_matrix <- caret::confusionMatrix(y_pred_class, test_data[[outcome]])

  # Return model and summary
  summary <- list(
    xgb_model = xgb_model,
    confusion_matrix = confusion_matrix
  )

  return(summary)
}
