# Load necessary libraries
library(RSocrata)
library(dplyr)

get_data <- function() {

  # Function to fetch data in chunks using RSocrata with pagination
  fetch_paginated_data <- function(base_url, limit = 5000, max_rows = 500000) {
    all_data <- list()  # List to store all data chunks
    offset <- 0         # Start with offset 0

    # Loop until we've retrieved the maximum number of rows or exhausted the data
    while (TRUE) {
      # Construct the URL with limit and offset parameters (without $order=:id)
      query_url <- paste0(base_url, "?$limit=", limit, "&$offset=", offset)

      # Fetch data using RSocrata with the constructed query URL
      data <- tryCatch({
        # Download the data as a dataframe and convert all columns to character
        df <- read.socrata(query_url)
        # Convert all columns in the dataframe to characters
        df <- df %>% mutate(across(everything(), as.character))
      }, error = function(e) {
        message("Error fetching data: ", e)
        return(NULL)
      })

      # Append the current chunk to the all_data list
      all_data <- append(all_data, list(data))

      # Update the offset for the next request
      offset <- offset + limit

      # If we've reached the max number of rows, stop fetching more
      if (offset >= max_rows) {
        break
      }
    }

    # Combine all data into a single data frame
    final_data <- bind_rows(all_data, .id = "source")

    # Ensure all columns are treated as characters (no type conversion logic)
    final_data <- final_data %>%
      mutate(across(everything(), as.character))  # Convert all columns to character

    return(final_data)
  }

  # URLs for the datasets
  pedestrians_url <- "https://data.montgomerycountymd.gov/resource/n7fk-dce5.csv"
  drivers_url <- "https://data.montgomerycountymd.gov/resource/mmzv-x632.csv"
  incidents_url <- "https://data.montgomerycountymd.gov/resource/bhju-22kf.csv"

  # Fetch data for each dataset with pagination
  pedestrians_data <- fetch_paginated_data(pedestrians_url, limit = 5000, max_rows = 500000)
  drivers_data <- fetch_paginated_data(drivers_url, limit = 3000, max_rows = 200000)
  incidents_data <- fetch_paginated_data(incidents_url, limit = 3000, max_rows = 200000)

  # drivers_data999 <- fetch_paginated_data(drivers_url, limit = 999, max_rows = 200000)
  # incidents_data999 <- fetch_paginated_data(incidents_url, limit = 999, max_rows = 200000)
  #
  # drivers_data1 <- fetch_paginated_data(drivers_url, limit = 1234, max_rows = 200000)
  # incidents_data1 <- fetch_paginated_data(incidents_url, limit = 1234, max_rows = 200000)
  #
  # drivers_data2 <- fetch_paginated_data(drivers_url, limit = 5678, max_rows = 200000)
  # incidents_data2 <- fetch_paginated_data(incidents_url, limit = 5670, max_rows = 200000)
  #
  # drivers_data3 <- fetch_paginated_data(drivers_url, limit = 12345, max_rows = 200000)
  # incidents_data3 <- fetch_paginated_data(incidents_url, limit = 12345, max_rows = 200000)
  #
  # drivers_data4 <- fetch_paginated_data(drivers_url, limit = 123, max_rows = 200000)
  # incidents_data4 <- fetch_paginated_data(incidents_url, limit = 123, max_rows = 200000)

  # Save results as a list
  results <- list(
    pedestrians_data = pedestrians_data,
    drivers_data = drivers_data,
    incidents_data = incidents_data
  )

  # Return results
  return(results)
}
