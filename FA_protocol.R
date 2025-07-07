library(data.table)
library(readxl)
library(writexl)

# Read .xlsx dataset with header
dataset <- read_xlsx("Morfometrie_2025_revised.xlsx", col_names = TRUE)

# Convert to data.table
dataset_dt <- as.data.table(dataset)

### FA protocol ###

library(lmer)
