# Install packages.
# install.packages("readxl")

# Load packages.
library(readxl) # used for reading "xlsx / xls" files.

# Load the dataset.
ZAS_Original <- read_excel("Z_Alizadeh_Sani_Dataset.xlsx")

# Check whether there is any na or null value in the dataset.
# The results indicate there is no special value in the dataset.
sum(is.na(ZAS_Original))
sum(is.null(ZAS_Original))

# Check column names and types.
# Create a text file to store feature information.
feature_info_out_file = file("Feature_Info_Yiqiu.txt", "w")
for (col_index in 1:ncol(ZAS_Original)) {
  col = ZAS_Original[, col_index]
  # Feature name.
  col_name = names(col)
  # Feature type.
  col_type = sapply(col, class)
  # Unique values of each feature (in vectors).
  col_unique_val = c(unique(col))
  # Convert unique value's vectors to strings.
  for (val in col_unique_val) {
    col_unique_info = paste(toString(val), sep = " ")
  }
  col_unique_info = gsub(",", "", col_unique_info)
  # Combine all information above to one string (for each feature).
  col_info = paste(col_name, col_type, col_unique_info, sep = ", ")
  # Write the info string to the text file.
  writeLines(col_info, feature_info_out_file)
}
# Close the text file.
close(feature_info_out_file)

