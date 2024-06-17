library(readr)
library(stringr)
library(dplyr)
library(reshape2) 
library(tm)
library(igraph)
library(tidyr)
library(visNetwork)
library(RColorBrewer)
library(gridExtra)
library(png)
library(readr)
file_path <- ("HP_Scripts_dataset/datasets/combined.csv")
hpscripts <- read_csv(file_path, locale = locale(encoding = "UTF-16"))
characters <-unique(hpscripts$character)
characters
filtered_characters <- characters[grepl("^[A-Za-z]+ [A-Za-z]+$", characters)]
filtered_characters <- c(filtered_characters, "Voldemort", "Dobby")
characters=filtered_characters
characters

####
character_mentions <- list()
collect_character_mentions <- function(character_name) {
  mentions <- hpscripts %>%
    filter(str_detect(tolower(tolower(hpscripts$dialog)), tolower(character_name)))
  
  if (nrow(mentions) > 0) {
    occurrence_count <- nrow(mentions)
    unique_speakers <- unique(mentions$character)
    
    return(list(Character = character_name,
                Frequency = occurrence_count,
                Speakers = paste(unique_speakers, collapse = ", ")))
  } else {
    return(list(Character = character_name,
                Frequency = 0,
                Speakers = ""))
  }
}
characters <- characters[nzchar(characters)]
for (i in seq_along(characters)) {
  char <- characters[i]
  
  if (grepl(" ", char)) {
    character_mentions[[char]] <- collect_character_mentions(char)
  } else {
    character_mentions[[char]] <- collect_character_mentions(char)
  }
}
character_mentions_df <- do.call(rbind.data.frame, character_mentions)
rownames(character_mentions_df) <- NULL
character_mentions_df <- character_mentions_df[order(-character_mentions_df$Frequency), ]
head(character_mentions_df)
write.csv(character_mentions_df, "HP_Scripts_dataset/img/character_mentions.csv", row.names = FALSE)

####
directed_edges_df <- data.frame(From = character(), To = character(), Weight = numeric(), stringsAsFactors = FALSE)
for (i in 1:nrow(character_mentions_df)){
  split_names <- unlist(strsplit(character_mentions_df[i,]$Speakers, ",\\s*"))
  split_names
  for (x in split_names){
    speaker <- x
    mentioned_character <- character_mentions_df$Character[i]
    frequency <- character_mentions_df$Frequency[i]
    if (mentioned_character != x) {
        directed_edges_df <- rbind(directed_edges_df, data.frame(From = mentioned_character, To = x, Weight = frequency, stringsAsFactors = FALSE))
      }
    
  }
}
directed_edges_df

####
directed_edges_df <- directed_edges_df %>%
  filter(From != "" & To != "")
directed_edges_df <- directed_edges_df %>%
  group_by(From, To) %>%
  summarise(Weight = sum(Weight)) %>%
  ungroup()

g <- graph_from_data_frame(directed_edges_df, directed = TRUE)
E(g)$weight <- directed_edges_df$Weight

#### chaotic
E(g)$weight <- edges$weight
E(g)$color <- ifelse(E(g)$weight > 100, "purple",
                     ifelse(E(g)$weight > 50, "red",
                            ifelse(E(g)$weight > 30, "yellow",
                                   ifelse(E(g)$weight > 20, "blue", "green"))))

data <- toVisNetworkData(g)
ledges <- data.frame(color =c("green", "blue", "yellow", "red", "purple"),
                     label = c("0-10", "10-20", "20-30", "30-50", "100+"))
node_degrees <- degree(g)
degree_distribution <- data.frame(Node = V(g)$name, Degree = node_degrees)

w<-visNetwork(nodes = data$nodes, edges = data$edges,width = "110%",main = "Mentions Network",)  %>%
  visInteraction(navigationButtons = TRUE) %>%
  visPhysics(solver = "repulsion", 
             barnesHut = list(
               gravitationalConstant = -2000,
               centralGravity = 0,
               springLength = 2000,
               springConstant = 0.05,
               damping = 0.09,
               stabilization = TRUE
             ))%>%         
  visLayout(randomSeed = 20) %>%
  visNodes() %>%
  visEdges(color = "color")%>%
  visLegend(addEdges = ledges, useGroups = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
w
png("HP_Scripts_dataset/img/degreedistrimentions.png", width = 1700, height =1200, units = "px", res = 300)
barplot(table(node_degrees), 
        main = "Degree Distribution", 
        xlab = "Degree", 
        ylab = "Frequency", 
        col = "skyblue", 
        border = "black",  # Add border for bars
        space = 0.5,       # Adjust space between bars
        width = 0.8        # Adjust thickness of bars
)
dev.off()
reciprocity_coefficient <- reciprocity(g)
cat("Reciprocity coefficient:", reciprocity_coefficient, "\n") 
#Reciprocity coefficient: 0.1170213 0: Indicates no reciprocity; edges are entirely unidirectional.

optimal <- cluster_optimal(g)
modularity_score <- modularity(optimal)
cat("Modularity score:", modularity_score, "\n") 
#Higher values (closer to 1) indicate stronger community structure where nodes within communities are densely connected, and connections between communities are sparse.

####
visSave(
  w,
  "HP_Scripts_dataset/img/mentions_network.html",
  selfcontained = FALSE
)

write.csv(data$nodes, "HP_Scripts_dataset/img/character_mentions_nodes.csv", row.names = FALSE)
edges_df <- data$edges[, c("from", "to", "weight", "color")]
data$edges$Weight <- NULL
write.csv(data$edges, "HP_Scripts_dataset/img/character_mentions_edges.csv", row.names = FALSE)

####
relationships_text <- paste(head(directed_edges_df$From), " -> ", head(directed_edges_df$To))
label_y <- seq(0.2, 0.1, length.out = nrow(head(directed_edges_df)))
plot.new()
text(x = rep(0.5, nrow(head(directed_edges_df))), y = label_y, labels = relationships_text, cex = 1.2)
text(x=0.5, y=0.23,"Instances of edges",  cex = 1.5)
png("HP_Scripts_dataset/img/relationships.png", width = 1700, height =1200, units = "px", res = 300)
label_y <- seq(0.6, 0.1, length.out = nrow(head(directed_edges_df)))
plot.new()
text(x = rep(0.5, nrow(head(directed_edges_df))), y = label_y, labels = relationships_text, cex = 1.2)
text(x=0.5, y=0.8,"Instances of edges",  cex = 1.5)
dev.off()


####
g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = FALSE, 
                 edge.attr.comb=c(weight="sum"))              
E(g)$color <- ifelse(E(g)$weight > 100, "purple",
               ifelse(E(g)$weight > 50, "red",
               ifelse(E(g)$weight > 30, "yellow",
               ifelse(E(g)$weight > 20, "blue", "green"))))
png(paste0("HP_Scripts_dataset/img/mentiongraph.png"), width = 800, height = 800)
set.seed(2.9) # Plot the graph (weighted)  
par(mar = c(0,0,0,0))
plot(g, vertex.shape="circle",vertex.label.color="black", edge.arrow.size = 0.3, vertex.size = 3,
     vertex.label.cex=1,vertex.label.dist=1, edge.curved=0.5,layout=layout_with_fr(g)*0.9,rescale=F,edge.color = E(g)$color,layout.center = TRUE)
# Add a legend
legend("bottomright", legend = c("0-10", "10-20", "20-30", "30-50", "100+"),
       col = c("green", "blue", "yellow", "red", "purple"), lwd = 2, cex = 1,
       title = "Edge Weight Range")
dev.off()
