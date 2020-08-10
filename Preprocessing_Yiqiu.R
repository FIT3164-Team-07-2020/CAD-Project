# Clear the environment.
rm(list=ls())

# Install packages.
# install.packages("readxl")



# Load packages.
library(readxl) # used for reading "xlsx / xls" files.



# Load the dataset.
ZAS_Original = read_excel("Z_Alizadeh_Sani_Dataset.xlsx")



# The results indicate there is no special value in the dataset.
sum(is.na(ZAS_Original))
sum(is.null(ZAS_Original))



# The function num_deature_desc_func is used to compute the descriptive 
# statistics for numerical features.
num_deature_desc_func = function(feature_incolumn, feature_desc_df) {
  # Convert the input dataframe column to vector for further computation.
  feature_invector = unlist(feature_incolumn)
  min = summary(feature_invector)[1]
  q1 = summary(feature_invector)[2]
  med = summary(feature_invector)[3]
  mean = summary(feature_invector)[4]
  q3 = summary(feature_invector)[5]
  max = summary(feature_invector)[6]
  var = var(feature_invector)
  # Combine all descriptive statistics into a vector and return the vector.
  desc_vector = c(min, q1, med, mean, q3, max, var)
  return(desc_vector)
}	



# Create a dataframe storing descriptive statistics of all numerical features.
num_feature_desc =  data.frame(Minimum = double(),
                               Q1 = double(),
                               Median = double(),
                               Mean = double(),
                               Q3 = double(),
                               Maximum = double(),
                               Variance = double())



# Check features' names, types, all possible values and descriptive statistics
# (only applicable to numerical features).

# Create a text file to store these information.
# Each line of the file will have the format of:
# feature name, feature type, (feature's unique values), 
# (feature's descriptive statistics if the feature is numerical)
feature_info_out_file = file("Feature_Info_Yiqiu.txt", "w")

# For each feature.
for (col_index in 1:ncol(ZAS_Original)) {
  col = ZAS_Original[, col_index]
  # Feature name.
  col_name = names(col)
  # Feature type.
  col_type = sapply(col, class)
  # If the current feature is numerical, compute and add its descriptive 
  # statistics into the dataframe num_feature_desc.
  if (col_type == "numeric") {
    desc_vector = num_deature_desc_func(col, num_feature_desc)
    desc_row = as.data.frame(t(desc_vector))
    row.names(desc_row) = names(col)
    colnames(desc_row) = c("Minimum", "Q1", "Median", "Mean", "Q3", "Maximum", 
                           "Variance")
    num_feature_desc = rbind(num_feature_desc, desc_row)
  }
  # Convert the descriptive statistics vector to strings surrounded with
  # parentheses.
  col_desc_info = paste(toString(desc_vector))
  col_desc_info = paste("(", col_desc_info, ")", sep = "")
  # Unique values of each feature (in vectors).
  col_unique_info = c(unique(col))
  col_unique_info = unlist(col_unique_info)
  # Convert the unique value's vector to strings surrounded with parentheses.
  col_unique_info = paste(toString(col_unique_info))
  col_unique_info = paste("(", col_unique_info, ")", sep = "")
  # Combine all information above to one string (for each feature).
  col_info = paste(col_name, col_type, col_unique_info, col_desc_info,
                   sep = ", ")
  # Write the info string to the text file.
  writeLines(col_info, feature_info_out_file)
}

# Close the text file.
close(feature_info_out_file)

# Write the descriptive statistics of numerical features into a new csv file.
write.csv(num_feature_desc, file="Num_feature_desc_stit_Yiqiu.csv")

# To keep the environment relatively clean, delete variables that are useless
# in further development.
remove("col", "desc_row", "col_desc_info", "col_index", "col_info", "col_name",
       "col_type", "col_unique_info", "desc_vector", "feature_info_out_file")
