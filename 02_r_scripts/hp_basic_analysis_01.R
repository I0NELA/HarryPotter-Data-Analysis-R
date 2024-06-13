##
## Load the libraries

library(readr)
library(tm)
library(SnowballC)


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

View(hp_df)
