library(MatchIt)
library(cobalt)

linearMod <- function(basic_table) {


  causal_data <- basic_table %>%
    dplyr::filter(!is.na(severity_category_pedestrian)) %>%
    dplyr::select(severity_category_pedestrian,safety_equipment_group, light_condition_category, substance_category, weather_category, surface_category)
  causal_data_clean <- causal_data %>%
    filter(!is.na(severity_category_pedestrian)) %>%
    filter(!is.na(safety_equipment_group)) %>%
    dplyr::filter(safety_equipment_group != "Other") %>%
    dplyr::mutate(safety_equipment_group = as.factor(safety_equipment_group))

  psm_model <- matchit(
    severity_category_pedestrian ~ safety_equipment_group,
    data = causal_data_clean,
    method = "nearest",  # Nearest neighbor matching
    distance = "logit",  # Logit model for calculating the propensity score
    ratio = 1  # 1:1 matching
  )

  # View the matching summary
  summary(psm_model)

  # Get the matched dataset
  matched_data <- match.data(psm_model)

  # Check the balance of covariates after matching
  bal.table <- bal.tab(psm_model)
  print(bal.table)

  causal_model <- glm( severity_category_pedestrian ~ safety_equipment_group,
                      data = matched_data, family = binomial())

  # Summarize the causal model
  summary(causal_model)
  #z-value = 0 not explaining any variance in the outcome
}
