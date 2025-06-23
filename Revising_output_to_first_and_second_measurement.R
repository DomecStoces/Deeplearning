# Load necessary library
library(data.table)

# Read the original CSV with pixel and mm measurements
df <- fread("your_input.csv")  # replace with your actual file path

# Extract the mm values (if you prefer pixel values, modify accordingly)
output_df <- data.table(
  ID = 1:nrow(df),
  Locality.number = NA_character_,
  Type = NA_character_,
  Trap = NA_integer_,
  Year = 2023,
  Month = NA_integer_,
  Species = "Ophonus cribricollis",
  Color = NA_character_,
  Sex = NA_character_,
  Wing.m. = NA_character_,
  Ticks = NA_integer_,
  Scale.size..mm. = 1.5,
  `a1 L` = df$La2_mm,
  `a2 L` = df$La3_mm,
  `a3 L` = df$La4_mm,
  `a1 R` = df$Ra2_mm,
  `a2 R` = df$Ra3_mm,
  `a3 R` = df$Ra4_mm,
  Body.size = df$Body_mm
)

# Create second row with blanks for each ID (e.g., for second round of measurements)
blank_df <- copy(output_df)
blank_df[, c("Locality.number", "Type", "Trap", "Month", "Color", "Sex", "Wing.m.", "Ticks", 
             "a1 L", "a2 L", "a3 L", "a1 R", "a2 R", "a3 R", "Body.size") := NA]

# Combine original and blank rows
final_df <- rbindlist(list(output_df, blank_df), use.names = TRUE)

# Sort by ID to ensure paired rows
setorder(final_df, ID)

# Write to CSV
fwrite(final_df, "converted_output.csv")