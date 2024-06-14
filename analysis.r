library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidytext)
library(wordcloud)
library(wordcloud2)
characters <- read.csv("HP_Scripts_dataset/datasets/Characters.csv", header = TRUE, row.names = NULL)
characters<-characters[1:125,] 
characters$Name
# Specify the path to the UTF-16 file
file_path <- ("HP_Scripts_dataset/datasets/combined.csv")
# Read the UTF-16 encoded file
hpscripts <- read_csv(file_path, locale = locale(encoding = "UTF-16"))
# View the data
head(hpscripts)
head(hpscripts$character)
# Use the table() function to create the frequency table
freq_table <- table(hpscripts$character)
# Sort frequencies in descending order
sorted_freq <- sort(freq_table, decreasing = TRUE)
freq_df <- data.frame(Name = names(sorted_freq), frequency = as.numeric(sorted_freq))
# Print the frequency table as a data frame
print(freq_df)
total_freq <- sum(freq_df$frequency)
cat("Total sum of frequencies:", total_freq, "\n")
top_20 <- freq_df[1:20, ]

# Create a bar plot for the top 20 names (with their freq)
barplot(top_20$frequency,
        names.arg = top_20$Name,
        main = "Top 20 Names by Frequency",
        xlab = "",
        ylab = "Frequency",
        col = "skyblue",
        border = "black",
        ylim = c(0, max(top_20$frequency) + 100),  # Adjusted y-axis limits for better visualization
        las = 2,  # Rotate x-axis labels vertically for better readability
        cex.names = 0.8)  # Adjust font size of x-axis labels for better fit
png("HP_Scripts_dataset/img/top_20_names_barplot.png", width = 5000, height = 5000, units = "px", res = 300)  # Adjust dimensions and resolution as needed
barplot(top_20$frequency,
        names.arg = top_20$Name,
        main = "Top 20 Names by Frequency",
        xlab = "",
        ylab = "Frequency",
        col = "skyblue",
        border = "black",
        ylim = c(0, max(top_20$frequency) + 100),
        las = 2,
        cex.names = 0.8)
dev.off()

colnames(characters)[colnames(characters) == "Character.Name"] <- "Name"
names(characters)
characters$Name
common_names <- intersect(characters$Name, top_20$Name)
# Subset characters based on common names
subset_characters <- characters[characters$Name %in% common_names, ]
# Merge subset_characters with top_20 by 'Name'
merged_data <- merge(subset_characters, top_20, by = "Name", all.x = TRUE)
final_df <- merged_data[, c("Name", "frequency", "House")]
final_df <- final_df[order(-final_df$frequency), ]
print(final_df) #top 20 of people who 'talk' the most (freq + house)

freq_table <- table(characters$House)
sorted_freq <- sort(freq_table, decreasing = TRUE)
freq_df <- data.frame(Name = names(sorted_freq), frequency = as.numeric(sorted_freq))
print(freq_df)
total_freq <- sum(freq_df$frequency)
cat("Total sum of frequencies:", total_freq, "\n")
freq_ <- freq_df[2:5,]
freq_$Name
png("HP_Scripts_dataset/img/pie of the houses.png", width = 1500, height = 1500, res = 300)  # Adjust dimensions and resolution as needed
pie(freq_$frequency, labels =freq_$Name, main="Houses at Hogwarts") #clearly a preference over the houses 
dev.off()

character_freq <- hpscripts %>%
  group_by(movie, character) %>%
  summarise(Frequency = n()) %>%
  arrange(movie, desc(Frequency))

# Print the resulting dataframe
head(character_freq)
character_freq_table <- as.data.frame(character_freq)
# Create a summary table for better readability

unique(character_freq_table$character) #all the names one time (should be)
single_word_names <- character_freq_table$character[grep("^\\w+$", character_freq_table$character)]
multi_word_names <- character_freq_table$character[!(character_freq_table$character %in% single_word_names)]
multi_word_names <- c(multi_word_names, "Voldemort", "Dobby")
character_freq_by_film <- split(character_freq_table, character_freq_table$movie)


character_names_list <- lapply(character_freq_by_film, function(df) tolower(df$character))
character_names_list

unique_movies <- unique(character_freq_table$movie)
unique_movies <- unique_movies[-length(unique_movies)]
for(i in unique_movies){
x<-intersect(character_names_list[[i]],tolower(multi_word_names))}
x #characters present in all the film (i hope)

z<-intersect(x,tolower(top_20$Name))
z #characters that are both top20 and present in all films

head(hpscripts$dialog)
top_4 <- top_20[1:4, ]

# Filter dialogs for the top 4 characters
hpscripts_top_4 <- hpscripts %>% filter(character %in% top_4$Name)

# Tokenize the dialogs for the top 4 characters
tokenized_words <- hpscripts_top_4 %>%
  unnest_tokens(word, dialog)

# Remove stop words
data("stop_words")
tokenized_words <- tokenized_words %>%
  anti_join(stop_words, by = "word")

# Count word frequencies by character
word_frequencies <- tokenized_words %>%
  group_by(character, word) %>%
  summarise(freq = n(), .groups = 'drop') %>%
  arrange(character, desc(freq))

# Identify the top 10 most frequently said words for each of the top 4 characters
top_10_words_per_character <- word_frequencies %>%
  group_by(character) %>%
  slice_max(order_by = freq, n = 250) %>%
  ungroup()

# Print the result
print(top_10_words_per_character)


for (char in unique(top_10_words_per_character$character)) {
  char_data <- top_10_words_per_character %>% filter(character == char)
  
  # Create word cloud
  png(paste0("HP_Scripts_dataset/img/wordcloud_", gsub(" ", "_", tolower(char)), ".png"), width = 800, height = 800)
  par(mar = c(5, 4, 4, 2) + 0.1)  # Adjust the margin for space at the bottom
  wordcloud(words = char_data$word, freq = char_data$freq, 
            scale = c(10, 0.9), 
            colors = brewer.pal(8, "Dark2"), 
            random.order = FALSE, 
            rot.per = 0.35)
  
  # Add the character name at the bottom
  mtext(char, side = 1, line = 3, cex = 1.5)
  
  dev.off()
}

result <- data.frame(
  Name = x)
result
result1 <- data.frame(
  Name = tolower(top_20$Name))
result1
new_table <- data.frame(
  Character_present = character(35),   # Initialize with empty strings
  Character_top20 = character(35)   # Initialize with empty strings
)

# Fill the data frame with names from result$Name and result1$Name
new_table$Character_present[1:nrow(result)] <- result$Name
new_table$Character_top20[1:nrow(result1)] <- result1$Name

# Print the new table
print(new_table) #difference between characters who stayed and talk the most
#NOT ORDER


