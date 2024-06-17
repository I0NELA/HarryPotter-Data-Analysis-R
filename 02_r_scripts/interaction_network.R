##
## Load the necessary libraries
library(readr)
library(dplyr)
library(igraph)
library(tidyr)
library(ggplot2)
library(ggforce)
library(knitr)
library(kableExtra)
library(webshot)
library(purrr)
library(dplyr)
webshot::install_phantomjs()
library(RColorBrewer)
library(visNetwork)

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
E(g)$weight <- interactions_df$count # Add the weight to the edges
E(g)[1]$weight

# Extract the Giant Connected Component (GCC)
gcc_indices <- which.max(components(g)$csize)
gcc <- induced_subgraph(g, which(components(g)$membership == gcc_indices))

# Detect community using the Edge Betweenness method
partition <- cluster_edge_betweenness(gcc)

# To add the community information back to the graph as an attribute
V(g)$community <- membership(partition)

# A dictionary-like structure for node names and their community
communities_df <- data.frame(node = V(g)$name, community = V(g)$community)
head(communities_df, 10)

# Plot the top 7 biggest communities in the network:
top_7_communities <- communities_df %>%
  group_by(community) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(7)
head(top_7_communities, 10)

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
names(membership_vector) <- V(g)$name

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
head(communities_to_nodes, 10)

# Filter out communities with less than 2 members and order by size descending
top_communities <- communities_to_nodes %>%
  # purrr::keep(~ length(.x) > 1) %>%
  {
    .[order(-map_int(., length))]
  } %>%
  head(7)
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
      data.frame(member = member, degree = degree(g, v = member))
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


##
## Build the communities graph.
##
## Generate the Graph Layout and Plot the Communities Network
## the top 7 communities in the network with visNetwork

# Create a vector of these top 7 community IDs
top_7_communities_ids <- top_7_communities$community
top_7_communities_ids

# Filter nodes in 'g' to include only those in the top 7 communities
nodes_in_top_communities <- V(g)[community %in% top_7_communities_ids]

# Color palette for the top communities with RColorBrewer
require("RColorBrewer")
community_colors_brewer <- brewer.pal(
  n = length(top_7_communities_ids),
  name = "Set3"
)
community_colors_brewer

# Assign colors to the communities
color_community_mapping <- setNames(
  community_colors_brewer,
  as.character(top_7_communities_ids)
)

# Correctly assign colors to nodes in the dataframe
nodes_df <- data.frame(
  id = nodes_in_top_communities$name,
  label = NA,
  size = log(degree(g, v = nodes_in_top_communities) * 0.5 + 1) * 16,
  color = sapply(
    nodes_in_top_communities$community,
    function(community) color_community_mapping[[as.character(community)]]
  ),
  group = as.character(nodes_in_top_communities$community)
)
head(nodes_df, 100)

# Prepare Edges Data Frame
edges_df <- as_data_frame(g, what = "edges")

# Generate the visNetwork Plot
require("visNetwork")

if (!dir.exists("03_plots/interaction_network/communities_network")) {
  dir.create("03_plots/interaction_network/communities_network")
}

vis_network <- visNetwork(
  nodes_df,
  edges_df,
  width = "100%",
  main = "Communities Interactions Network",
  submain = "( the top 7 communities )",
) %>%
  visNodes(
    shape = "dot",
    scaling = list(label = list(enabled = TRUE)),
    font = list(size = 40, face = "arial", color = "#000000"),
    color = list(background = nodes_df$color, border = nodes_df$color)
  ) %>%
  visEdges(
    smooth = FALSE,
    width = 0.5,
    color = list(color = "rgba(200,200,200,0.5)")
  ) %>%
  visOptions(
    height = "100%",
    width = "100%",
    highlightNearest = TRUE,
    nodesIdSelection = TRUE,
  ) %>%
  visLayout(randomSeed = 222) %>%
  visPhysics(
    enabled = TRUE,
    solver = "forceAtlas2Based",
    forceAtlas2Based = list(
      gravitationalConstant = -150, # More negative to increase repulsion
      centralGravity = 0.01,
      springLength = 500, # Increase for more space between nodes
      springConstant = 0.08,
      damping = 0.4,
      avoidOverlap = 1 # Enable to avoid node overlap
    )
  ) %>%
  addFontAwesome() %>%
  visGroups(
    groupname = "1",
    color = community_colors_brewer[1],
  ) %>%
  visGroups(
    groupname = "3",
    color = community_colors_brewer[2],
  ) %>%
  visGroups(
    groupname = "8",
    color = community_colors_brewer[3],
  ) %>%
  visGroups(
    groupname = "5",
    color = community_colors_brewer[4],
  ) %>%
  visGroups(
    groupname = "16",
    color = community_colors_brewer[5],
  ) %>%
  visGroups(
    groupname = "53",
    color = community_colors_brewer[6],
  ) %>%
  visGroups(
    groupname = "2",
    color = community_colors_brewer[7],
  ) %>%
  visLegend(
    useGroups = TRUE,
    position = "left",
    main = "IDs",
    width = 0.1
  )
vis_network

# Save the plot to an HTML file
visSave(
  vis_network,
  "03_plots/interaction_network/communities_network/communities_network.html",
  selfcontained = FALSE
)


##
## General network analysis and visualization of the `vis_network`:
## Aggregate everything in a single plot


##
### STEP 1: Convert the vis_network to a igraph object:
head(edges_df)
g7 <- graph_from_data_frame(d = edges_df, directed = FALSE)
E(g7)[1]$weight


## STEP 2: Calculate the network diameter
## (the longest shortest path between any two nodes):
g7_diameter       <- diameter(g7, directed = FALSE, weights = NA)       # 5 #
g7_diameter_path  <- get_diameter(g7, directed = FALSE, weights = NA)
g7_diameter             # 3 #
g7_diameter_path$name   # "Ghosts"          "Harry Potter"    "Severus Snape" "Charity Burbage"

g7_diameter_weighted        <- diameter(g7, directed = FALSE)
g7_diameter_weighted_path   <- get_diameter(g7, directed = FALSE)
g7_diameter_weighted      # 5 #
g7_diameter_weighted_path$name
# "Ghosts" -> "Harry Potter" -> "Severus Snape" -> "Charity Burbage" -> "Albus Dumbledore"


##
### STEP 3: Calculate the network degree distribution:
g7_degree_distribution <- degree_distribution(
  g7,
  mode = "all",
  cumulative = TRUE
)

# The cumulative distribution for each degree
# (adjust to match the len of the degree distribution)
degrees         <- 0 : (length(g7_degree_distribution.dist) - 1)
cumulative_freq <- 1 - g7_degree_distribution.dist

degree_distribution_plot <- {
  ggplot(
    data.frame(degree = degrees, cumulative_freq = cumulative_freq),
    aes(x = degree, y = cumulative_freq)
  ) +
    geom_line(color = "skyblue") +
    labs(
      title = "Degree Distribution of Harry Potter Interaction Network",
      x = "Degree",
      y = "Cumulative Frequency"
    ) +
    theme_light()

}


if (!dir.exists("03_plots/interaction_network/communities_network")) {
  dir.create("03_plots/interaction_network/communities_network")
}

ggsave(
  "03_plots/interaction_network/communities_network/degree_distribution.png",
  plot = degree_distribution_plot,
  width = 10,
  height = 10,
  dpi = 300
)


##
## STEP 4: Calculate the centrality

## Degree Centrality ##
g7_degree <- degree(g7)
head(g7_degree)

top_7_degree <- g7_degree %>%
  sort(decreasing = TRUE) %>%
  head(7)
# top_7_degree
## 'Harry Potter'       = 169,
## 'Ron Weasley'        = 148,
## 'Hermione Granger'   = 146,
## 'Neville Longbottom' = 89,
## 'Rubeus Hagrid'      = 75,
## 'Draco Malfoy'       = 75,
## 'Fred Weasley'       = 74


g7_degree_centrality <- centr_degree(
  g7,
  normalized = TRUE
)
g7_degree_centrality$res
g7_degree_centrality$centralization     # 0.8461412 #
g7_degree_centrality$theoretical_max    # 30450     #

## Eigenvectors Centrality ##
g7_eigenvector_centrality <- eigen_centrality(
  g7,
  directed = FALSE,
  weights = NA
)
g7_eigenvector_centrality$value         # 39.47075  #
g7_eigenvector_centrality$options$bmat  # "I"       #
g7_eigenvector_centrality$options$n     # 175       #
g7_eigenvector_centrality$vector

g7_eigenvector_centrality_top07_vectors <- g7_eigenvector_centrality$vector %>%
  sort(decreasing = TRUE) %>%
  head(7)
# View(g7_eigenvector_centrality_top07_vectors)
## 'Harry Potter'       = 1,
## 'Ron Weasley'        = 0.962454743753122,
## 'Hermione Granger'   = 0.955959631715495,
## 'Neville Longbottom' = 0.720774365947422,
## 'Fred Weasley'       = 0.648718154478896,
## 'Draco Malfoy'       = 0.645982405413439,
## 'George Weasley'     = 0.633657905056388


## Betweenness Centrality ##
g7_betweenness_centrality <- betweenness(
  g7,
  directed = FALSE,
  weights = NA,
)
g7_betweenness_centrality_top07 <- g7_betweenness_centrality %>%
  sort(decreasing = TRUE) %>%
  head(7)
# View(g7_betweenness_centrality_top07)

## 'Harry Potter'       = 4381.7969429253,
## 'Ron Weasley'        = 2182.47473081813,
## 'Hermione Granger'   = 2078.9304027596,
## 'Neville Longbottom' = 455.13473200257,
## 'Voldemort'          = 370.571001900237,
## 'Severus Snape'      = 357.721290140979,
## 'Rubeus Hagrid'      = 334.847394905651

g7_betweenness_centrality_normalized <- betweenness(
  g7,
  directed = FALSE,
  weights = NA,
  normalized = TRUE
)

g7_betweenness_centrality_normalized_top07 <- g7_betweenness_centrality_normalized %>%
  sort(decreasing = TRUE) %>%
  head(7)
# View(g7_betweenness_centrality_normalized_top07)

## "Harry Potter"       = 0.291129954350229,
## "Ron Weasley"        = 0.145005297376794,
## "Hermione Granger"   = 0.138125732692818,
## "Neville Longbottom" = 0.0302395011628842,
## "Voldemort"          = 0.0246210219852659,
## "Severus Snape"      = 0.0237672772666918,
## "Rubeus Hagrid"      = 0.0222475180988407


##
## STEP 5: Network Modularity

## CEB (Community Edge Betweenness) ##
g7_ceb <- cluster_edge_betweenness(g7)

modularity(g7_ceb)
## 0.05458263
##  High modularity indicates a strong community structure.
##

# Plot the modularity dendrogram
png(
  "03_plots/interaction_network/ceb_modularity.png",
  width = 10 * 300, height = 10 * 300, res = 300
)
plot(
  g7_ceb,
  g7,
  edge.arrow.size = 0.5,
  edge.curved = 0.2,
  vertex.label.cex = 0.5,
  vertex.size = 5,
  vertex.label.dist = 1.6,
  vertex.label.color = "black",
  vertex.label.family = "sans",
  vertex.label.font = 2,
  vertex.label.degree = 0,
  vertex.label = V(g7)$name,
  edge.color = "gray",
  edge.width = 0.5,
  layout = layout_with_fr,
  main = "Modularity Dendrogram of Harry Potter Interaction Network (CEB)",
  sub = paste("The modularity is", round(modularity(g7_ceb), 4))
)
dev.off()


## CLP (Cluster Label Propagation) ##
g7_clp <- cluster_label_prop(g7)
plot(
  g7_clp,
  g7,
  edge.arrow.size = 0.5,
  edge.curved = 0.2,
  vertex.label.cex = 0.5,
  vertex.size = 5,
  vertex.label.dist = 1.6,
  vertex.label.color = "black",
  vertex.label.family = "sans",
  vertex.label.font = 2,
  vertex.label.degree = 0,
  vertex.label = V(g7)$name,
  edge.color = "gray",
  edge.width = 0.5,
  layout = layout_with_fr
)


## CFG (Community Fast Greedy) ##
g7_cfg <- cluster_fast_greedy(simplify(g7))

# Plot the dendrogram of the modularity
plot(g7_cfg, simplify(g7))


##
## STEP 6: Assortativity

## Assortativity ##
g7_assortativity <- assortativity_degree(g7)
g7_assortativity
# ''' -0.3279172
#   Negative assortativity indicates disassortative mixing.
# '''


##
## STEP 7: Network Density & Clustering Coefficient (Transitivity)

## Network Density ##
g7_density <- graph.density(g7)
g7_density
# ''' 0.1251232
#   The density of a network is the ratio of the number of edges to the number of possible edges.
# '''

## Clustering Coefficient (Transitivity) ##
g7_transitivity <- transitivity(g7)
g7_transitivity
# ''' 0.3423269
#   The transitivity of a network is the ratio of the number of triangles to the number of connected triples.
# '''


##
## RECAP: Print the summary of the network analysis
cat(
  "\n",
  "Harry Potter Interaction Network Analysis Summary\n",
  "-----------------------------------------------\n",
  "Network Diameter: \t", g7_diameter, "\n",
  "Network Density: \t", g7_density, "\n",
  "Network Transitivity: \t", g7_transitivity, "\n",
  "Network Assortativity: \t", g7_assortativity, "\n",
  "Network Modularity: \t", modularity(g7_ceb), "\n",
  "Degree Centrality Centralization: \t", g7_degree_centrality$centralization, "\n\n"
)


##
## EXTRA: Sankey Circle Diagram of the Network
# TODO: Try to implement the Sankey Circle Diagram of the Network
