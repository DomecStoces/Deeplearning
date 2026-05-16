library(data.table)

# Read CSV with no header to process the 3-row header manually
raw <- fread("C:/Users/Dominik/Desktop/Test3-Dominik-2025-07-12/CollectedData_Dominik.csv", header = FALSE)

# Build new column names from rows 2–4
header1 <- as.character(unlist(raw[1, ]))
header2 <- as.character(unlist(raw[2, ]))
header3 <- as.character(unlist(raw[3, ]))

# Combine headers into meaningful names
new_names <- paste(header2, header3, sep = "_")
new_names[1:3] <- c("source", "type", "Image")  # Rename the first 3 manually

# Assign new column names and drop header rows
df <- raw[-c(1:3)]
setnames(df, new_names)

# Convert appropriate columns to numeric
df[, (4:ncol(df)) := lapply(.SD, as.numeric), .SDcols = 4:ncol(df)]

# Calculate distances between body parts (Euclidean distance formula)
df[, La2_length := sqrt((La2_2_x - La2_1_x)^2 + (La2_2_y - La2_1_y)^2)]
df[, La3_length := sqrt((La3_1_x - La2_2_x)^2 + (La3_1_y - La2_2_y)^2)]
df[, La4_length := sqrt((La3_2_x - La3_1_x)^2 + (La3_2_y - La3_1_y)^2)]

df[, Ra2_length := sqrt((Ra2_2_x - Ra2_1_x)^2 + (Ra2_2_y - Ra2_1_y)^2)]
df[, Ra3_length := sqrt((Ra3_1_x - Ra2_2_x)^2 + (Ra3_1_y - Ra2_2_y)^2)]
df[, Ra4_length := sqrt((Ra3_2_x - Ra3_1_x)^2 + (Ra3_2_y - Ra3_1_y)^2)]

df[, Body_length := sqrt((Elytra_base_x - Elytra_tip_x)^2 + (Elytra_base_y - Elytra_tip_y)^2)]

# Convert pixel measurements to mm Ophonus cribricollis
pixels_per_mm <- 756.771 / 1.5
mm_per_pixel <- 1 / pixels_per_mm


df[, La2_mm := La2_length * mm_per_pixel]
df[, La3_mm := La3_length * mm_per_pixel]
df[, La4_mm := La4_length * mm_per_pixel]
df[, Ra2_mm := Ra2_length * mm_per_pixel]
df[, Ra3_mm := Ra3_length * mm_per_pixel]
df[, Ra4_mm := Ra4_length * mm_per_pixel]
df[, Body_mm := Body_length * mm_per_pixel]

# Export selected results
fwrite(df[, .(Image,
              La2_length,
              La3_length,
              La4_length,
              Ra2_length,
              Ra3_length,
              Ra4_length)],
       "C:/Users/Dominik/Desktop/Test3-Dominik-2025-07-12/body_parts.csv")



