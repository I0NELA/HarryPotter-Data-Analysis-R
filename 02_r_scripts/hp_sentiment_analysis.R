##
## Load the libraries
source("02_r_scripts/utilities.R")
library(readr)
library(tm)
library(SnowballC)
library(dplyr)
library(ggplot2)
#install.packages("syuzhet")
library(syuzhet)

##
##  STEP 01 - Load the data ###

base_path <- "01_tidy_data/Harry-Potter-Movies-Dataset-Kaggle/"
hp_df <- read_csv(paste0(base_path, "hp-scripts-combined.csv"))
summary(hp_df)
head(hp_df, 10)



sentiment <- get_nrc_sentiment(hp_df$dialog)
td <- data.frame(t(sentiment))
td[,1:5]

# compute columns sums across rows for each level of a grouping variable
td = data.frame(rowSums(td[-1]))
td

names(td)[1] <- "count"
td

combined_df <- cbind("sentiment" = rownames(td), td) 
combined_df

rownames(combined_df) <- NULL
combined_df

td_emotions <- combined_df[1:10, ]
td_polarity <- combined_df[11:12, ]


## Plot emotions

require("ggplot2")

ggplot(td_emotions, aes(x = sentiment, y = count, fill = sentiment)) +
    geom_bar(stat = "identity") +
    labs(
        title = "Emotions in Harry Potter Dialogs",
        x = "Emotion",
        y = "Count"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())



## Plot polarity

require("ggplot2")

ggplot(td_polarity, aes(x = sentiment, y = count, fill = sentiment)) +
    geom_bar(stat = "identity") +
    labs(
        title = "Polarity in Harry Potter Dialogs",
        x = "Polarity",
        y = "Count"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())
