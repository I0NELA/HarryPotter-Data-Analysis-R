library(readr)
library(stringr)
library(dplyr)
library(reshape2) 
library(tm)
library(igraph)
library(tidyr)
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
characters <- characters[characters != "2" & characters != "Hat" & characters != "head" & characters != "Old"]
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
character_mentions_df
character_mentions_df$Character
character_mentions_df$Speakers

#####
single_words <- str_split(character_mentions_df$Speakers, ", ")[[1]]

# Step 2: Ensure Uniqueness of Single Words
unique_single_words <- unique(unlist(str_split(single_words, " ")))
num_rows <- nrow(character_mentions_df)
num_unique_words <- length(unique_single_words)

# Create a vector of unique single words repeating as needed
character_mentions_df$Speakers <- unique_single_words[seq_len(num_rows) %% num_unique_words + 1]
characters <- characters[characters != "2" & characters != "Hat" & characters != "head" & characters != "Old"]
characters <- characters[1:nrow(character_mentions_df)]
character_mentions_df$Speakers <- characters
character_mentions_df$Speakers
character_mentions_df$Character
character_mentions_df
#####
speaker_mentions <- character_mentions_df %>%
  filter(Frequency > 0) %>%  # Filter out characters with zero mentions
  group_by(Speakers) %>%
  summarize(Characters = list(unique(Character))) %>%
  unnest(cols = Characters)

# Step 2: Create a Data Frame for Edges
directed_edges_df <- speaker_mentions %>%
  rename(from = Speakers, to = Characters) %>%
  distinct()  # Remove duplicate pairs

print(directed_edges_df)

# Step 3: Create the Graph
g <- graph_from_data_frame(directed_edges_df, directed = TRUE)
g <- simplify(g, remove.multiple = FALSE, remove.loops = TRUE) 

vertices <- V(g)
print(vertices)
num_vertices <- vcount(g)
print(num_vertices)
vertex_names <- V(g)$name
print(vertex_names)

edges <- E(g)
print(edges)


plot(g,
     layout = layout.random,  # Choose a layout algorithm (e.g., circular layout)
     vertex.color = "lightblue",  # Color of vertices
     vertex.size = 5,  # Size of vertices
     vertex.label.dist = 0.8,  # Distance of vertex labels from vertices
     vertex.label.color = "black",  # Color of vertex labels
     edge.color = "gray",  # Color of edges
     edge.width = 2,  # Width of edges
     main = "General Graph Visualization"  # Title of the plot
)
