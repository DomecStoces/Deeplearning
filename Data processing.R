library(data.table)

# Read all rows as character to process headers manually
raw <- fread("C:/Users/Dominik/Desktop/Test1-Dominik-2025-06-15/labeled-data/output/image_predictions_DLC_Resnet50_Test1Jun15shuffle1_snapshot_010.csv", header = FALSE)

# Extract header info
colnames <- paste(raw[2], raw[3], raw[4], sep = "_")
colnames[1] <- "Image"

# Drop the header rows and set proper names
df <- raw[-c(1:4)]
setnames(df, colnames)

# Convert needed columns to numeric
df[, (2:ncol(df)) := lapply(.SD, as.numeric), .SDcols = 2:ncol(df)]

names(df)

# Calculate distances
# Compute antennomere distances
df[, a2_length := sqrt((animal_La2_2_x - animal_La2_1_x)^2 + (animal_La2_2_y - animal_La2_1_y)^2)]
df[, a3_length := sqrt((animal_La3_1_x - animal_La2_2_x)^2 + (animal_La3_1_y - animal_La2_2_y)^2)]
df[, a4_length := sqrt((animal_La3_2_x - animal_La3_1_x)^2 + (animal_La3_2_y - animal_La3_1_y)^2)]

# Conversion from pixels to mm
pixels_per_mm <- 720.363 / 1.5  
mm_per_pixel <- 1 / pixels_per_mm

# Add mm distances
df[, a2_mm := a2_length * mm_per_pixel]
df[, a3_mm := a3_length * mm_per_pixel]
df[, a4_mm := a4_length * mm_per_pixel]

fwrite(df[, .(Image, a2_length, a2_mm, a3_length, a3_mm, a4_length, a4_mm)],
       "C:/Users/Dominik/Desktop/Test1-Dominik-2025-06-15/labeled-data/output/antennomere_lengths_with_mm.csv")

