library(data.table)

# Read all rows as character to process headers manually
raw <- fread("C:/Users/Dominik/Desktop/Test1-Dominik-2025-06-15/labeled-data/output/image_predictions_DLC_Resnet50_Test1Jun15shuffle1_snapshot_010.csv", header = FALSE)

# Extract header info
colnames <- paste(raw[2], raw[3], raw[4], sep = "_")
colnames[1] <- "Image"

# Drop the header rows and set proper names
df <- raw[-c(1:4)]
setnames(df, colnames)
