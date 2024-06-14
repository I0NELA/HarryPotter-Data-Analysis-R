library(readr)
library(stringr)
library(dplyr)
characters <- read.csv("HP_Scripts_dataset/datasets/Characters.csv", header = TRUE, row.names = NULL)
head(characters)
# Specify the path to the UTF-16 file
file_path <- ("HP_Scripts_dataset/datasets/combined.csv")
# Read the UTF-16 encoded file
hpscripts <- read_csv(file_path, locale = locale(encoding = "UTF-16"))
# View the data
head(hpscripts)
head(hpscripts$character)
hpscripts$character
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
top_20  

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
length(freq_df)
freq_ <- freq_df[2:5,]
freq_$Name
pie(freq_$frequency, labels =freq_$Name, main="Houses at Hogwarts") #clearly a preference over the houses 

character_freq <- hpscripts %>%
  group_by(movie, character) %>%
  summarise(Frequency = n()) %>%
  arrange(movie, desc(Frequency))

# Print the resulting dataframe
head(character_freq)

# Create a summary table for better readability
character_freq_table <- as.data.frame(character_freq)
unique(character_freq_table$character) #all the names one time (should be)

character_freq_by_film <- split(character_freq_table, character_freq_table$movie)
character_freq_by_film

character_names_list <- lapply(character_freq_by_film, function(df) tolower(df$character))
character_names_list

unique_movies <- unique(character_freq_table$movie)
unique_movies <- unique_movies[-length(unique_movies)]
for(i in unique_movies){
x<-intersect(character_names_list[[i]],tolower(unique(character_freq_table$character)))}
x #characters present in all the film (i hope)

z<-intersect(x,tolower(top_20$Name))
z #characters that are both top20 and present in all films

result <- data.frame(
  Name = x)
result
result1 <- data.frame(
  Name = tolower(top_20$Name))
result1
new_table <- data.frame(
  Character_present = character(40),   # Initialize with empty strings
  Character_top20 = character(40)   # Initialize with empty strings
)

# Fill the data frame with names from result$Name and result1$Name
new_table$Character_present[1:nrow(result)] <- result$Name
new_table$Character_top20[1:nrow(result1)] <- result1$Name

# Print the new table
print(new_table) #difference between characters who stayed and talk the most
#NOT ORDER