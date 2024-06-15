library(readr)
library(stringr)
library(dplyr)
library(reshape2) 
library(tm)
library(igraph)
file_path <- ("HP_Scripts_dataset/datasets/combined.csv")
# Read the UTF-16 encoded file
hpscripts <- read_csv(file_path, locale = locale(encoding = "UTF-16"))
# View the data
head(hpscripts)
head(hpscripts$character)
# Get the list of unique movies
single_word_names <- hpscripts$character[grep("^\\w+$", hpscripts$character)]
multi_word_names <- hpscripts$character[!(hpscripts$character %in% single_word_names)]
multi_word_names <- c(multi_word_names, "Voldemort", "Dobby")
characters<- unique(multi_word_names)
split_names <- str_split(characters, "\\s+")
# Extract "name" and "surname" into separate vectors
names <- sapply(split_names, function(x) x[1])
surnames <- sapply(split_names, function(x) ifelse(length(x) > 1, x[2], ""))
# Combine into a data frame
characters <- c(rbind(names, surnames))
# View the resulting vector
stopwords <- stopwords("en")
characters <- characters[!tolower(characters) %in% stopwords]
characters
character_mentions <- list()

collect_character_mentions <- function(character_name) {
  # Find rows where the character is mentioned in dialogues
  mentions <- hpscripts %>%
    filter(str_detect(tolower(tolower(hpscripts$dialog)), tolower(character_name)))
  
  if (nrow(mentions) > 0) {
    # Count the number of occurrences
    occurrence_count <- nrow(mentions)
    # Get unique speakers
    unique_speakers <- unique(mentions$character)
    
    # Return data as a list
    return(list(Character = character_name,
                Frequency = occurrence_count,
                Speakers = paste(unique_speakers, collapse = ", ")))
  } else {
    # If character is not mentioned, return NA values
    return(list(Character = character_name,
                Frequency = 0,
                Speakers = ""))
  }
}
for (char in characters) {
  character_mentions[[char]] <- collect_character_mentions(char)
}
character_mentions_df <- do.call(rbind.data.frame, character_mentions)
rownames(character_mentions_df) <- NULL
character_mentions_df <- character_mentions_df[order(-character_mentions_df$Frequency), ]
head(character_mentions_df)


directed_edges_df <- data.frame(From = character(), To = character(), stringsAsFactors = FALSE)

# Iterate through each row in hpscripts
for (i in 1:nrow(hpscripts)) {
  speaker <- hpscripts$character[i]
  mentioned_characters <- unique(unlist(str_extract_all(tolower(hpscripts$dialog[i]), tolower(paste0(characters1, collapse = "|")))))
  
  # Create directed edges from speaker to each mentioned character
  for (mentioned in mentioned_characters) {
    if (mentioned != speaker && mentioned %in% characters1) {
      directed_edges_df <- rbind(directed_edges_df, data.frame(From = speaker, To = mentioned, stringsAsFactors = FALSE))
    }
  }
}

# Remove the first empty row from initialization
directed_edges_df <- directed_edges_df[-1, ]
directed_edges_df
# Create graph object
g <- graph_from_data_frame(directed_edges_df, directed = TRUE)

# Plot the graph (messy)
plot(g, layout = layout.circle,
     edge.arrow.size = 0.5, vertex.color = "lightblue",
     vertex.size = 0.1, vertex.label.cex = 1, edge.color = "gray",
     main = "Character Mentions Network")
