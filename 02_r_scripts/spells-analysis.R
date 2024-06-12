###
## Count how many times each spell is used in the scripts:
###

# Load the necessary libraries
library(readr)

# Load the Spells dataset:
# Name;Incantation;Type;Effect;Light
spells <- read_csv2("00_raw_data/hp-scripts-kaggle-dataset/Spells.csv", col_types = cols(Name = col_character(), Incantation = col_character(), Type = col_character(), Effect = col_character(), Light = col_character()))
print(colnames(spells))

# Load the scripts dataset:
script1 <- read_csv2("00_raw_data/hp-scripts-kaggle-dataset/Harry-Potter-1-script.csv", col_types = cols(Character = col_character(), Sentence = col_character()))
script2 <- read_csv2("00_raw_data/hp-scripts-kaggle-dataset/Harry-Potter-2-script.csv", col_types = cols(Character = col_character(), Sentence = col_character()))
script3 <- read_csv2("00_raw_data/hp-scripts-kaggle-dataset/Harry-Potter-3-script.csv", col_types = cols(Character = col_character(), Sentence = col_character()))
scripts <- list(script1, script2, script3)

print(colnames(scripts[[1]]))

# Extract list of spell incantations from the Incantation column of the Spells dataset
# and filter out the "Unknown" AND "<NA>" incantations:
spells_incantations <- spells$Incantation[!is.na(spells$Incantation) & spells$Incantation != "Unknown"]
print(spells_incantations)
