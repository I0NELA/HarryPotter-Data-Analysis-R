###
## Download the images of the characters from the URLs in the shortversioncharacters.csv file:
## The images will be saved in the "characters-images" folder of the 01_tidy_data folder
###

# Load the necessary libraries
library(tidyverse)

# Load the data
shortversioncharacters <- read_csv("00_raw_data/hp-scripts-kaggle-dataset/shortversioncharacters.csv")

# Create the folder (if not already exists) to save the images
dir.create("01_tidy_data/characters-images", showWarnings = FALSE)

# Download the images
for (i in 1:nrow(shortversioncharacters)) {
  download.file(
      shortversioncharacters$image[i],
      destfile = paste0("01_tidy_data/characters-images/", shortversioncharacters$name[i], ".jpg")
      )
}
