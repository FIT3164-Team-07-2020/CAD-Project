# The code in this R script contains part of Section 1: Data Preparation and
# Understanding & Section 3: Feature selection from ensemble models.. The aim of
# creating this file is to more easily copy and paste preprocessing codes to 
# later scripts if necessary.

# This script contains the final optimised boosting model for the CAD project.
# Model AUC: 0.93103
# Model Accuracy: 92.31%

# Load necessary package(s).
library(caret) # This library is used to pre-process the data.
library(ipred)
library(data.table)
library(mltools)
library(ROCR)
library(adabag)
# Clear the work space before run the code.
rm(list = ls())

# Read the csv-formatted dataset as a dataframe.
ZAS_Original = read.csv("Z_Alizadeh_Sani_Dataset.csv")
ZAS = ZAS_Original

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

#Convert all to factor first so the variables are the same class.
ZAS$Function.Class = as.factor(ZAS$Function.Class)
ZAS$Region.RWMA = as.factor(ZAS$Region.RWMA)
ZAS$VHD = as.factor(ZAS$VHD)

#Converts ZAS to a data.table, which is just a convenient format for performing one hot encoding with the mltools package.
ZAS = data.table(ZAS)

#One hot encoding using mltools package.
ZAS = one_hot(ZAS, cols = c('Function.Class', 'Region.RWMA', 'VHD'))

#Convert back to data frame
ZAS = data.frame(ZAS)

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

# Feature selection from ensemble models.
# Cut 1 threshold.
cut_1 = c("Q.Wave", "Obesity", "Systolic.Murmur", "LVH", "FH")
ZAS.compressed1 = ZAS[, !(colnames(ZAS) %in% cut_1)]
print(dim(ZAS.compressed1))
# write.csv(ZAS.compressed1, "ZAS.compressed1.csv")

# Cut 2 threshold.
cut_2 = c("Q.Wave", "Obesity", "Systolic.Murmur", "LVH", "FH", "Sex", "DLP", "Function.Class_0", "Function.Class_1", "Function.Class_2", "Function.Class_3", "St.Depression", "DM", "Current.Smoker", "Dyspnea", "VHD_mild", "VHD_Moderate", "VHD_N", "VHD_Severe", "Nonanginal", "HDL", "Neut")
ZAS.compressed2 = ZAS[, !(colnames(ZAS) %in% cut_2)]
print(dim(ZAS.compressed2))

# Create training and testing datasets
set.seed(3164)
# Get 70% of the dataset to be training dataset.
training.rows = sample(1:nrow(ZAS.compressed1), 0.7*nrow(ZAS.compressed1))
ZAS.train1 = ZAS.compressed1[training.rows,]
# The rest of rows are testing dataset.
ZAS.test1 = ZAS.compressed1[-training.rows,]

training.rows = sample(1:nrow(ZAS.compressed2), 0.7*nrow(ZAS.compressed2))
ZAS.train2 = ZAS.compressed2[training.rows,]
# The rest of rows are testing dataset.
ZAS.test2 = ZAS.compressed2[-training.rows,]

#Train the optimised boosting model
boost = adabag::boosting(Cath~., data = ZAS.train2, mfinal = 40)
boostpredict = predict.boosting(boost, newdata = ZAS.test2)
#Calculate accuracy of the model
n = sum(boostpredict$confusion)
x = diag(boostpredict$confusion)
accuracy = sum(x)/n
boostprob = ROCR::prediction(boostpredict$prob[,2], ZAS.test2$Cath)
boost_AUC = performance(boostprob, "auc")@y.values
print(paste0("Model AUC: ",as.numeric(boost_AUC)))
print(paste0("Model Accuracy: ", accuracy))