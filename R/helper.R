# Function to identify the most common words in a specified column of a dataset
common_words <- function(data, column, top_n) {
  # Define a list of stop words to exclude from the analysis
  # These are common words that do not add meaning (e.g., articles, prepositions)
  stop_words <- c(
    "the", "is", "a", "and", "for", "to", "of", "in", "on", "at", "by", "with",
    "this", "that", "it", "as", "or", "an", "be", "are", "was", "were", "has",
    "have", "had"
  )

  # Tokenize the text in the specified column, count word frequencies, and exclude stop words
  word_counts <- data %>%
    unnest_tokens(word, !!sym(column)) %>%  # Break the text into individual words (tokens)
    filter(!word %in% stop_words) %>%       # Exclude stop words from the analysis
    count(word, sort = TRUE)               # Count word occurrences, sorted in descending order

  # Extract the top N most common words using `slice_max`
  top_n_words <- word_counts %>%
    slice_max(order_by = n, n = top_n)     # Get the top `n` words based on their frequency

  # Print the top N words for visualization
  print(top_n_words)


}

# Example usage:
# common_words(Crash_Reporting_Non_Motorists, "Off-Road Description", 5)

