# The code in this R script is exactly the same as Section 1: Data Preparation
# and Understanding. The aim of creating this file is to more easily copy and
# paste preprocessing codes to later scripts if necessary.

# Load necessary package(s).
library(caret) # This library is used to pre-process the data.

# Clear the work space before run the code.
rm(list = ls())

# Read the csv-formatted dataset as a dataframe.
ZAS_Original = read.csv("Z_Alizadeh_Sani_Dataset.csv")
ZAS = ZAS_Original

# Show the first 5 lines of the dataset to see what it looks like.
head(ZAS)

# Check the number of rows (patients) and columns (features).
dim(ZAS)

# Remove predictor features with zero / nearly zero variances.
# The reason is that the features have extremely unbalanced value distribution
# which don't significantly help predicting CAD.
# Find predictor features with zero / nearly-zero variances and record their 
# column indexes.
nzv_feature_indexes = nearZeroVar(ZAS[, !(colnames(ZAS) %in% c("Cad"))])
# Print selected features information.
print(paste0("Number of predictor features with zero or near-zero variances: ",
             length(nzv_feature_indexes)))
print(paste0("Following predictor features have zero or near-zero variances and
             should be removed: ",
             toString(colnames(ZAS)[nzv_feature_indexes])))
# Remove predictor features with zero / nearly-zero variances.
ZAS = ZAS[, -nzv_feature_indexes]

# Convert all "factor" variables into "character" type.
# This makes it easier to convert them into numeric values.
for (feature_index in 1:ncol(ZAS)) {
  # Get column name
  feature_name = colnames(ZAS)[feature_index]
   #Convert object to "character" if it is "factor"
  if ((class(ZAS[,feature_name]) == "factor")) {
    ZAS[,feature_name] = as.character(ZAS[,feature_name])
  }
}

# Convert all categorical features with only "N" / "Y" values to numerical 
# binary (that is, to convert "N" to 0 and "Y" to 1).
for (feature_index in 1:ncol(ZAS)) {
  # Get the name of the feature.
  feature_name = colnames(ZAS)[feature_index]
  # For each feature.
  feature = ZAS[, feature_index]
  # Extract unique values of each feature (in vectors).
  unique_val = unlist(c(unique(feature)))
  # If the feature has only "N" and "Y" categorical values, convert "N" to
  # 0 and "Y" to 1:
  if ((length(unique_val) == 2) &
      ("N" %in% unique_val) &
      ("Y" %in% unique_val)) {
    ZAS[ZAS[, feature_name] == "N", feature_name] = 0
    ZAS[ZAS[, feature_name] == "Y", feature_name] = 1
  }
}

# Convert values of Sex ("Male" / "Fmale") and Cath ("Normal" / "Cad") features
# to binary (that is to: for Sex, convert "Fmale" to 0 and "Male" to 1; 
# for Cath, convert "Normal" to 1 and "Cad" to 0).
# Convert Sex to binary. Male is 1 and Female is 0.
ZAS$Sex[ZAS$Sex == "Male"] = 1
ZAS$Sex[ZAS$Sex == "Fmale"] = 0
# Convert Cath to binary. Normal is 0 and Cad is 1.
ZAS$Cath[ZAS$Cath == "Normal"] = 0
ZAS$Cath[ZAS$Cath == "Cad"] = 1

# Convert values of features  (Function Class, Region RWMA and VHD) with more
# than two categories to dummy.
# Function Class.
ZAS$Function.Class = as.factor(ZAS$Function.Class)
contrasts(ZAS$Function.Class) = contr.treatment(4)
# Region RWMA.
ZAS$Region.RWMA = as.factor(ZAS$Region.RWMA)
contrasts(ZAS$Region.RWMA) = contr.treatment(5)
# Note that BBB is removed before because of its near-zero variance.
# VHD.
ZAS$VHD = as.factor(ZAS$VHD)
contrasts(ZAS$VHD) = contr.treatment(4)

# All categorical variables are in characters or converted to numeric, 
# this is just for convenience of pre-processing. 
# Now we want them to be factors.
for (feature_index in 1:ncol(ZAS)) {
  # Get the name of the feature.
  feature_name = colnames(ZAS)[feature_index]
  # For each feature.
  feature = ZAS[, feature_index]
  # Extract unique values of each feature (in vectors).
  unique_val = unlist(c(unique(feature)))
  # If the feature has only 0 and 1 numerical values:
  if ((length(unique_val) == 2) &
      (0 %in% unique_val) &
      (1 %in% unique_val)) {
    # Convert the feature from "character" to "numeric" object type.
    ZAS[,feature_name] = as.factor(ZAS[,feature_name])
  }
}

# Check the current type of each feature to ensure they are in expected:
# there should be 34 categorical features (which should be all factor typed)
# and 21 quantitative features (which should be either numeric or integer type).
types = sapply(ZAS, class)
print(paste0("The number of factor typed features: ",
             length(grep("factor", types))))
print(paste0("The number of numeric / integer typed features: ", 
             length(grep("numeric", types)) + length(grep("integer", types))))
print(types)

# Split the dataset into train and test sets (in 70%-30% ratio).
# Set the seed to make all teammates have the same random dataset.
set.seed(3164)
# Get 70% of the dataset to be training dataset.
training.rows = sample(1:nrow(ZAS), 0.7*nrow(ZAS))
ZAS.train1 = ZAS[training.rows,]
# The rest of rows are testing dataset.
ZAS.test1 = ZAS[-training.rows,]

# Set the seed to make all teammates have the same random dataset.
set.seed(070707)
# Get 70% of the dataset to be training dataset.
training.rows = sample(1:nrow(ZAS), 0.7*nrow(ZAS))
ZAS.train2 = ZAS[training.rows,]
# The rest of rows are testing dataset.
ZAS.test2 = ZAS[-training.rows,]

# Set the seed to make all teammates have the same random dataset.
set.seed(31643164)
# Get 70% of the dataset to be training dataset.
training.rows = sample(1:nrow(ZAS), 0.7*nrow(ZAS))
ZAS.train3 = ZAS[training.rows,]
# The rest of rows are testing dataset.
ZAS.test3 = ZAS[-training.rows,]

# Set the seed to make all teammates have the same random dataset.
set.seed(13653)
# Get 70% of the dataset to be training dataset.
training.rows = sample(1:nrow(ZAS), 0.7*nrow(ZAS))
ZAS.train4 = ZAS[training.rows,]
# The rest of rows are testing dataset.
ZAS.test4 = ZAS[-training.rows,]

# Set the seed to make all teammates have the same random dataset.
set.seed(1654221)
# Get 70% of the dataset to be training dataset.
training.rows = sample(1:nrow(ZAS), 0.7*nrow(ZAS))
ZAS.train5 = ZAS[training.rows,]
# The rest of rows are testing dataset.
ZAS.test5 = ZAS[-training.rows,]

# Feature selection from ensemble models.
# Cut 1 threshold.
cut_1 = c("Q.Wave", "Obesity", "Systolic.Murmur", "LVH", "FH")
ZAS.compressed1 = ZAS[, !(colnames(ZAS) %in% cut_1)]
types1 = sapply(ZAS.compressed1, class)
print(paste0("The number of factor typed features for 1st compressed version: ",
             length(grep("factor", types1))))
print(paste0("The number of numeric / integer typed features for 1st compressed version: ", 
             length(grep("numeric", types1)) + length(grep("integer", types1))))
print(types1)
# write.csv(ZAS.compressed1, "ZAS.compressed1.csv")

# Cut 2 threshold.
cut_2 = c("Q.Wave", "Obesity", "Systolic.Murmur", "LVH", "FH", "Sex", "DLP",
          "Function.Class", "St.Depression", "DM", "Current.Smoker", "Dyspnea", 
          "VHD", "Nonanginal", "HDL", "Neut")
ZAS.compressed2 = ZAS[, !(colnames(ZAS) %in% cut_2)]
types2 = sapply(ZAS.compressed2, class)
print(paste0("The number of factor typed features for 2nd compressed version: ",
             length(grep("factor", types2))))
print(paste0("The number of numeric / integer typed features for 2nd compressed version: ", 
             length(grep("numeric", types2)) + length(grep("integer", types2))))
print(types2)
# write.csv(ZAS.compressed2, "ZAS.compressed2.csv")