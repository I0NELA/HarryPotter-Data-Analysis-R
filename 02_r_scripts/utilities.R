# Load the necessary libraries
library(readr)
library(dplyr)

#' Get the release year of a movie
#' @param movie The movie to get the release year for
#' @return The release year as a character vector
#' @examples
#' get_release_year("Harry Potter and the Sorcerer's Stone")
#'
get_release_year <- function(movie) {
  base_path <- "01_tidy_data/Harry-Potter-Movies-Dataset-Kaggle/"
  hp_df <- read_csv2(paste0(base_path, "movies.csv"))
  head(hp_df, 10)

  released_year <- hp_df %>%
    filter(movie == movie) %>%
    select(released_year) %>%
    unique()

  return(released_year)
}
