library(data.table)

# Step 1: Read original dataset with blanks interleaved
orig <- fread("Ophonus_cribr_miss.csv")

# Step 2: Read second-round measurements
second <- fread("C:/Users/Dominik/Desktop/Test2-Dominik-2025-06-21/labeled-data/second_measurements.csv")

# Step 3: Remove existing blank rows from original (to find positions)
orig_non_blank <- orig[!apply(orig, 1, function(row) all(is.na(row) | trimws(as.character(row)) == ""))]

# Step 4: For each original non-blank row, record the row index of the blank row that follows
blank_indices <- which(apply(orig, 1, function(row) all(is.na(row) | trimws(as.character(row)) == "")))

# Step 5: Map images to those blank row indices
image_to_blank_idx <- setNames(blank_indices, orig[blank_indices - 1, Image])

# Step 6: For each image in the second measurement, write the row into corresponding blank
for (i in 1:nrow(second)) {
  img <- second[i, Image]
  if (!is.na(image_to_blank_idx[img])) {
    orig[image_to_blank_idx[img]] <- second[i]
  } else {
    warning(paste("No matching blank row for image:", img))
  }
}

# Step 7: Export the combined dataset
fwrite(orig, "Ophonus_cribr_combined.csv")