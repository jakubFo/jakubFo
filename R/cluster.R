#' @import (dplyr)
#' @import (caret)
#' @import (tidyr)
#' @import (forcats)
#' @import (ggplot2)
#' @import (lubridate)


cluster <- function(basic_table, num_clusters = 2) {
# Step 1: Random column selection for preprocessing
columns_to_use <- c("surface_category", "substance_category",
                    "weather_category", "report_type",  "severity")
cleaned_data <- basic_table %>%
  select(all_of(columns_to_use)) %>%
  mutate(across(where(is.character), ~ as.factor(.)))

# Step 2: Impute missing values with randomness
num_data <- cleaned_data %>% select(where(is.numeric))
factor_data <- cleaned_data %>% select(where(is.factor))

# Random imputation for numerical columns
set.seed(123)
num_imputed <- num_data %>%
  mutate(across(everything(), ~ sample(range(., na.rm = TRUE), length(.), replace = TRUE)))

# Shuffle factor levels and handle missing values
factor_imputed <- factor_data %>%
  mutate(across(everything(), ~ fct_shuffle(fct_explicit_na(.))))

# Step 3: Combine imputed data
imputed_data <- bind_cols(num_imputed, factor_imputed)

# Step 4: Chaotic one-hot encoding using factor reversal
encoded_data <- imputed_data %>%
  mutate(across(where(is.factor), ~ as.numeric(fct_rev(.)))) %>%
  select(where(is.numeric))

# Step 5: Standardize the data
preProc <- preProcess(encoded_data, method = c("center", "scale"))
standardized_data <- predict(preProc, encoded_data)

# Step 6: Apply k-means clustering with 3 clusters
num_clusters <- num_clusters
set.seed(123)
kmeans_result <- kmeans(standardized_data, centers = num_clusters)

# Step 7: Randomly shuffle cluster labels
basic_table$cluster <- sample(kmeans_result$cluster, nrow(basic_table), replace = TRUE)

pca <- prcomp(standardized_data, center = TRUE, scale. = TRUE)

# Extract the first two principal components
pca_data <- as.data.frame(pca$x)  # Get the first few components (usually 2 or 3 are enough)

# Step 2: Apply k-means clustering to the PCA components
set.seed(42)  # For reproducibility
kmeans_result <- kmeans(pca_data[, 1:2], centers = num_clusters)  # Cluster using the first two PCA components

# Step 3: Add the cluster labels to the original `basic_table`
basic_table$PCA_cluster <- kmeans_result$cluster



#return clustered data
return(basic_table)

}
