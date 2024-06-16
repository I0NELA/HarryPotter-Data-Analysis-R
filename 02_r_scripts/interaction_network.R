##
## Load the necessary libraries
library(readr)
library(dplyr)
library(igraph)
library(tidyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(webshot)
library(purrr)
library(dplyr)
webshot::install_phantomjs()

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
      for (character_01 in 1:(length(chapter) - 1)) {
        for (character_02 in (character_01 + 1):length(chapter)) {
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

if (!dir.exists("03_plots/interaction_network")) {
  dir.create("03_plots/interaction_network")
}

ggsave("03_plots/interaction_network/top_10_interactions.png", top_10_interactions_plot, width = 10, height = 10, dpi = 300)


##
## Create an interaction network GRAPH
require("igraph")
head(interactions_df)

# Create an undirected graph from the dataframe
g <- graph_from_data_frame(d = interactions_df, directed = FALSE)
E(g)$weight <- interactions_df$count

# Extract the Giant Connected Component (GCC)
gcc_indices <- which.max(components(g)$csize)
gcc <- induced_subgraph(g, which(components(g)$membership == gcc_indices))

# Detect community using the Edge Betweenness method
partition <- cluster_edge_betweenness(gcc)
# partition_louvain <- cluster_louvain(gcc)

# To add the community information back to the graph as an attribute
V(gcc)$community <- membership(partition)

# A dictionary-like structure for node names and their community
communities <- data.frame(node = V(gcc)$name, community = V(gcc)$community)
head(communities)

# Plot the top 7 biggest communities:
top_7_communities <- communities %>%
  group_by(community) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(7)
head(top_7_communities)

communities_plot <- {
  ggplot(
    top_7_communities,
    aes(
      x = reorder(community, count, decreasing = TRUE),
      y = count,
      fill = as.factor(community)
    )
  ) +
    geom_bar(stat = "identity") +
    labs(
      title = "Top 7 Largest Communities in Harry Potter Interaction Network",
      x = "Community",
      y = "Count"
    ) +
    theme(
      axis.text.x = element_text(angle = 65, hjust = 1),
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)
    )
}

if (!dir.exists("03_plots/interaction_network")) {
  dir.create("03_plots/interaction_network")
}

ggsave("03_plots/interaction_network/top_7_communities.png", communities_plot, width = 10, height = 10, dpi = 300)


##
## Analyze and display the top biggest communities

# Convert partition membership to a named vector for easier access
membership_vector <- membership(partition)
names(membership_vector) <- V(gcc)$name

# Initialize an empty list to store nodes by community
communities_to_nodes <- list()

# Populate the list with nodes for each community
for (node in names(membership_vector)) {
  community <- as.character(membership_vector[node])

  if (is.null(communities_to_nodes[[community]])) {
    communities_to_nodes[[community]] <- c(node)
  } else {
    communities_to_nodes[[community]] <- c(communities_to_nodes[[community]], node)
  }
}
head(communities_to_nodes, 7)

# Filter out communities with less than 2 members and order by size descending
top_communities <- communities_to_nodes %>%
  purrr::keep(~ length(.x) > 1) %>%
  {
    .[order(-map_int(., length))]
  }
length(top_communities)
head(top_communities)

# Iterate through each community to analyze and display top members by degree
for (community_name in names(top_communities)) {
  curr_comm <- communities_to_nodes[[community_name]]
  curr_com_with_degrees <- data.frame(
    member = character(),
    degree = numeric(),
    stringsAsFactors = FALSE
  )

  for (member in curr_comm) {
    curr_com_with_degrees <- rbind(
      curr_com_with_degrees,
      data.frame(member = member, degree = degree(gcc, v = member))
    )
  }

  top_members <- curr_com_with_degrees %>%
    dplyr::arrange(desc(degree)) %>%
    head(20)

  # Save the top members of this community in PNG image:
  require("ggplot2")
  top_members_plot <- {
    ggplot(
      top_members,
      aes(
        x = reorder(member, degree, decreasing = TRUE),
        y = degree,
        fill = member
      )
    ) +
      geom_bar(stat = "identity") +
      labs(
        title = paste("Top Members of Community", community_name),
        x = "Character",
        y = "Degree"
      ) +
      theme(
        axis.text.x = element_text(angle = 65, hjust = 1),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5)
      )
  }

  if (!dir.exists("03_plots/interaction_network/community_members")) {
    dir.create("03_plots/interaction_network/community_members")
  }

  ggsave(
    paste0("03_plots/interaction_network/community_members/community_", community_name, ".png"),
    top_members_plot,
    width = 10, height = 10, dpi = 300
  )
}

