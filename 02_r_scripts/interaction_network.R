##
## Load the necessary libraries
library(readr)
library(dplyr)
library(igraph)
library(tidyr)
library(ggplot2)


##
## Load the data

base_path <- "01_tidy_data/Harry-Potter-Movies-Dataset-Kaggle/"
hp_df <- read_csv(paste0(base_path, "hp-scripts-combined.csv"))
head(hp_df)


## Extract only the columns we need
hp_df <- hp_df %>%
  select(movie, chapter, character)
head(hp_df)

# Init the list to store the interactions
chapter_interactions <- list()

# Iterate through each movie
unique_movies <- unique(hp_df$movie)
for (m in unique_movies) {
  # Filter the dataframe for the current movie and group by chapter
  grouped_chapters_df <- hp_df %>%
    filter(movie == m) %>%
    group_by(chapter) %>%
    summarise(characters = list(unique(character))) %>%
    pull(characters)

  # Iterate through each chapter
  for (chapter in grouped_chapters_df) {
    # Create all unique pairs of characters for the chapter
    if (length(chapter) > 1) {
      for (character_01 in 1 : (length(chapter) - 1)) {
        for (character_02 in (character_01 + 1) : length(chapter)) {
          pair <- sort(c(chapter[character_01], chapter[character_02]))
          pair_key <- paste(pair, collapse = " - ")

          # Increment the count for the pair
          if (!is.null(chapter_interactions[[pair_key]])) {
            chapter_interactions[[pair_key]] <- chapter_interactions[[pair_key]] + 1
          } else {
            chapter_interactions[[pair_key]] <- 1
          }
        }
      }
    }
  }
}

# Convert the list to a dataframe for easier manipulation and visualization
interactions_df <- data.frame(
  pair = names(chapter_interactions),
  count = unlist(chapter_interactions),
  stringsAsFactors = FALSE
)
head(interactions_df)


# Split the pair into two separate columns
interactions_df <- interactions_df %>%
  separate(pair, into = c("Character_01", "Character_02"), sep = " - ")
head(interactions_df)

##
## Plot top 10 most frequent interactions
top_10_interactions <- interactions_df %>%
  arrange(desc(count)) %>%
  head(10)
head(top_10_interactions)

require("ggplot2")
top_10_interactions_plot <- {
  ggplot(
    top_10_interactions,
    aes(
      x = reorder(
        paste(Character_01, Character_02, sep = " - "),
        count,
        decreasing = TRUE
      ),
      y = count,
      fill = paste(Character_01, Character_02, sep = " - ")
    )
  ) +
    geom_bar(stat = "identity") +
    labs(
      title = "Top 10 Most Frequent Interactions in Harry Potter Dialogs",
      x = "Character Pair",
      y = "Count"
    ) +
    theme(
      axis.text.x = element_text(angle = 65, hjust = 1),
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)
    )
}

if (!dir.exists("03_plots/interaction_network"))
  dir.create("03_plots/interaction_network")

ggsave("03_plots/interaction_network/top_10_interactions.png", top_10_interactions_plot, width = 10, height = 10, dpi = 300)
