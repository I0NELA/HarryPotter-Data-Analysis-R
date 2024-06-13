##
## Load the libraries

library(readr)
library(tm)
library(SnowballC)
library(dplyr)
library(ggplot2)


##
##  STEP 01 - Load the data ###

base_path <- "01_tidy_data/Harry-Potter-Movies-Dataset-Kaggle/"
hp_df <- read_csv(paste0(base_path, "hp-scripts-combined.csv"))
summary(hp_df)
head(hp_df)

##
##  STEP 02 - Data Cleaning ###

# Define the stop words
stopwords <- stopwords("en")

#' Clean the text by removing punctuation, stop words, and stemming the words
#' @param text The text to clean
#' @return The cleaned text as a character vector
#'
clean_text <- function(text) {
  text <- tolower(text)                 # tm package

  text <- removePunctuation(            # tm package
      text,
      preserve_intra_word_dashes = TRUE,
      preserve_intra_word_contractions = TRUE)

  text <- removeWords(text, stopwords)  # tm package

  text <- stripWhitespace(text)         # tm package

  text <- stemDocument(text)            # SnowballC package

  return(as.character(text))
}

# Apply clean_text to each dialog entry and store the results in a list
cleaned_dialog_list <- lapply(hp_df$dialog, clean_text)

# Convert the list to a character vector
cleaned_dialog_vector <- unlist(cleaned_dialog_list)

# Add the cleaned dialog as a new column in the data frame
hp_df$cleaned_dialog_text <- cleaned_dialog_vector

summary(hp_df)
head(hp_df)


##
##  STEP 03 - Exploratory Data Analysis ###

### Plot the number of characters in each `movie` ###

# Num of characters per movie
num_characters_per_movie <- hp_df %>%
  group_by(movie) %>%
  summarize(num_characters = n_distinct(character))
head(num_characters_per_movie)

# Plot the number of characters per movie and 
# save the plot as a PNG file in the 
# `03_plots/basic_analysis_01` directory:
plot <- {
  ggplot(num_characters_per_movie, aes(x = movie, y = num_characters, fill = movie)) +
    geom_bar(stat = "identity") +
    labs(title = "Number of Characters in Each Movie",
        x = "",
        y = "N° of Characters") +
    theme(
      axis.text.x = element_text(angle = 65, hjust = 1),
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)
    )
}

ggsave("03_plots/basic_analysis_01/characters_per_movie.png", plot, width = 10, height = 10, dpi = 300)
