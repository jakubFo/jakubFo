library(ggplot2)
# Load the crash data
data_list <- read_data()

#inspect relationship
showRelationship(data_list)

# structurize data, and unify data in important columns (i.e. fatal outcome,
# road type, safety equipment use etc.)
data_list <- preprocess(data_list)

#agregate data
basic_table <- aggregate(data_list)

#clustering to identify distinct crash profiles
clustered <- cluster(basic_table, 5)

#plot clusters
ggplot(clustered,
       aes(
         x = severity,
         y = weather_category,
         color = factor(PCA_cluster)
       )) +
  geom_jitter(
    shape = 1,
    size = 1,
    alpha = 0.1,
    width = 0.1,
    height = 0.1
  ) +
  labs(title = "PCA Clusters", x = "severity", y = "weather_category") +
  theme_minimal() +
  theme(legend.position = "top")

#decrease number of clusters
clustered <- cluster(basic_table, 2)

#plot clusters
ggplot(clustered,
       aes(
         x = severity,
         y = weather_category,
         color = factor(PCA_cluster)
       )) +
  geom_jitter(
    shape = 1,
    size = 1,
    alpha = 0.1,
    width = 0.1,
    height = 0.1
  ) +
  labs(title = "PCA Clusters", x = "severity", y = "weather_category") +
  theme_minimal() +
  theme(legend.position = "top")

#add columns for visualization purposes
patterns <- patterns(basic_table)

#plot patterns
ggplot(patterns, aes(x = day_of_week)) +
  geom_bar(aes(fill = factor(day_of_week)), show.legend = FALSE) +
  labs(title = "Accidents by Day of Week",
       x = "Day of Week",
       y = "Count of Accidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#plot patterns
ggplot(patterns, aes(x = hour_of_day)) +
  geom_bar(aes(fill = factor(hour_of_day)), show.legend = FALSE) +
  labs(title = "Accidents by Hour of Day",
       x = "Hour of Day",
       y = "Number of Accidents") +
  scale_x_continuous(breaks = 0:23) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#let's explore some models
xgboostmodel <- xgboostmodel(basic_table)
xgboostmodel$confusion_matrix
#Kappa 0 -

linearMod <- linearMod(basic_table)
linearMod
#z-value = 0 not explaining any variance in the outcome
#let's try to build model based on accident severity (Injury vs Property)

xgboostmodelSeverity <- xgboostmodelSeverity(basic_table)
xgboostmodelSeverity$confusion_matrix
#better than previously but improvements are needed. Seems there is a space
#for creating 1 standard xgboostmodel function to handle predictors/outcoms
# and data mapping based on parameters, it helps to run this model several times
#with other parameters
#define predictors
predictors <-
  c(
    "movement_category",
    "route_category",
    # "safety_equipment_group",
    "light_condition_category",
    "weather_category",
    "surface_category",
    "substance_category"
    # "report_type"
  )
#define outcome
outcome = "severity"

xgboost_general_model <- xgboost_general_model(
  basic_table,
  predictors = predictors,
  outcome = outcome,
  filter_condition = NULL,
  filter_na = TRUE
)
xgboost_general_model$confusion_matrix

#compare models
results <-
  resamples(
    list(
      xgboost_general_model = xgboost_general_model$xgb_model,
      xgboostmodelSeverity = xgboostmodelSeverity$xgb_model,
      xgboostmodel = xgboostmodel$xgb_model
    )
  )
summary(results)

#let's visualize models
bwplot(results)


