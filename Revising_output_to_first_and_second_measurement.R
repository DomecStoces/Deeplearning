library(data.table)

# Step 1: Read the data
df <- fread("C:/Users/Dominik/Desktop/Test2-Dominik-2025-06-21/body_parts.csv")

# Step 1.5: Remove any pre-existing blank rows (rows where all fields are NA or empty)
df <- df[!apply(df, 1, function(row) all(is.na(row) | trimws(row) == ""))]

# Step 2: Create a blank row with NA values
blank_row <- as.data.table(as.list(rep(NA, ncol(df))))
setnames(blank_row, names(df))

# Step 3: Interleave each row with a blank row
rows_with_blanks <- rbindlist(
  lapply(1:nrow(df), function(i) rbind(df[i], blank_row)),
  use.names = TRUE, fill = TRUE
)

# Step 4: Save to CSV
fwrite(rows_with_blanks, "body_parts_blank.csv")
