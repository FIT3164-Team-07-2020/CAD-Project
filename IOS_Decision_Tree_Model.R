setwd("/Users/xinhaoyan/desktop")
library(tree)
library(e1071)
library(adabag)
library(rpart)
library(randomForest)
library(ROCR)
library(neuralnet)
library(randomForest)

rm(list = ls())

ZAS_Original = read.csv("Z_Alizadeh_Sani_Dataset.csv")
ZAS = ZAS_Original


##################################### Pre-processing part #####################################
ZAS = ZAS[, !(colnames(ZAS) == "Exertional.CP")]


# Find predictor features with zero / nearly-zero variances.
# Record their column indexes.
nzv_feature_indexes = nearZeroVar(ZAS[, !(colnames(ZAS) %in% c("Cad"))])
# Print selected features information.
print(paste0("Number of predictor features with zero or near-zero variances: ",
             length(nzv_feature_indexes)))
print(paste0("Following predictor features have zero or near-zero variances and should be removed: ",
             toString(colnames(ZAS)[nzv_feature_indexes])))
# Remove predictor features with zero / nearly-zero variances.
ZAS = ZAS[, -nzv_feature_indexes]


for (feature_index in 1:ncol(ZAS)) {
  # Get column name
  feature_name = colnames(ZAS)[feature_index]
  #Convert object to "character" if it is "factor"
  if ((class(ZAS[,feature_name]) == "factor")) {
    ZAS[,feature_name] = as.character(ZAS[,feature_name])
  }
}

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

# Convert Sex to binary. Male is 1 and Female is 0.
ZAS$Sex[ZAS$Sex == "Male"] = 1
ZAS$Sex[ZAS$Sex == "Fmale"] = 0
# Convert Cath to binary. Normal is 0 and Cad is 1.
ZAS$Cath[ZAS$Cath == "Normal"] = 0
ZAS$Cath[ZAS$Cath == "Cad"] = 1

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


#Split the dataset into train and test sets (in 70%-30% ratio).
#set seed to make all teammates have the same random dataset
set.seed(13653)
#get 70% of the dataset to be training dataset
training.rows = sample(1:nrow(ZAS), 0.7*nrow(ZAS))
ZAS.train = ZAS[training.rows,]
#the rest of rows are testing dataset
ZAS.test = ZAS[-training.rows,]

##################################### Best tree based on the requirement #####################################
# Best model with a better AUC and lower complexity and simple features
r.fit=tree(Cath~., data=ZAS.train)
#when prune the tree with a size of 8, it will be the best tree fitting our requirement.
prune.rfit = prune.misclass(r.fit, best = 8)
plot(prune.rfit)
text(prune.rfit, pretty = 0)
