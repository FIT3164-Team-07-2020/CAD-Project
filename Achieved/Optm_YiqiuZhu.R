# Load necessary package(s).
library(neuralnet)
library(caret)
library(ROCR)
library(mltools)
library(data.table)

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

# Convert all to factor first so the variables are the same class.
ZAS$Function.Class = as.factor(ZAS$Function.Class)
ZAS$Region.RWMA = as.factor(ZAS$Region.RWMA)
ZAS$VHD = as.factor(ZAS$VHD)
# Converts ZAS to a data.table, which is just a convenient format for performing
# one hot encoding with the mltools package.
ZAS = data.table(ZAS)
# One hot encoding using mltools package.
ZAS = one_hot(ZAS, cols = c('Function.Class', 'Region.RWMA', 'VHD'))
# Convert back to data frame.
ZAS = data.frame(ZAS)

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

# Feature selection from ensemble models.
# Cut 1 threshold.
cut_1 = c("Q.Wave", "Obesity", "Systolic.Murmur", "LVH", "FH")
ZAS.compressed1 = ZAS[, !(colnames(ZAS) %in% cut_1)]
print(dim(ZAS.compressed1))

# Cut 2 threshold.
cut_2 = c("Q.Wave", "Obesity", "Systolic.Murmur", "LVH", "FH", "Sex", "DLP", "Function.Class_0", "Function.Class_1", "Function.Class_2", "Function.Class_3", "St.Depression", "DM", "Current.Smoker", "Dyspnea", "VHD_mild", "VHD_Moderate", "VHD_N", "VHD_Severe", "Nonanginal", "HDL", "Neut")
ZAS.compressed2 = ZAS[, !(colnames(ZAS) %in% cut_2)]
print(dim(ZAS.compressed2))





# Train and test sets with threshold 1 applied.
set.seed(316407)
training.rows1 = sample(1:nrow(ZAS.compressed1), 0.7*nrow(ZAS.compressed1))
train1 = ZAS.compressed1[training.rows1,]
test1 = ZAS.compressed1[-training.rows1,]

# Train and test sets with threshold 2 applied.
set.seed(316407)
training.rows2 = sample(1:nrow(ZAS.compressed2), 0.7*nrow(ZAS.compressed2))
train2 = ZAS.compressed2[training.rows2,]
test2 = ZAS.compressed2[-training.rows2,]

# Since the neuralnet() function can only deal with quantitative variables, all 
# qualitative variables need to be converted into dummy variables.
# Threshold 1.
train1_dummy = model.matrix(~., data = train1)[,-1]
test1_dummy = model.matrix(~., data = test1)[,-1]
# Threshold 2.
train2_dummy = model.matrix(~., data = train2)[,-1]
test2_dummy = model.matrix(~., data = test2)[,-1]

# According to thumb of rules from below article, set different numbers of
# hidden layers and neurons.
# https://www.heatonresearch.com/2017/06/01/hidden-layers.html
# Number of hidden layers.
n_hlayers = 3
# Number of neurons for threshold 1.
input_size1 = ncol(train1)
n_hneurons1 = as.integer(c(0.5*input_size1, 2/3*input_size1,
                          input_size1, 2*input_size1))
# Number of neurons for threshold 2.
input_size2 = ncol(train2)
n_hneurons2 = as.integer(c(0.5*input_size2, 2/3*input_size2,
                           input_size2, 2*input_size2))

# For each combination of hidden layers & neurons, train the neural network and 
# evaluate the ROC / AUC.
# Threshold 1.
for (hlayer in 1:n_hlayers) {
  for (hneuron_index in 1:length(n_hneurons1)) {
    # Generate the tuned parameters for this iteration.
    hiddens = rep(n_hneurons1[hneuron_index], times = hlayer)
    # To reduce the randomness of the training, set the random seed.
    set.seed(316407)
    # Train the neural network.
    nnet = neuralnet(Cath1~., train1_dummy, hidden = hiddens, 
                     linear.output = FALSE)
    # Feed the test data into trained model to predict.
    test1_no_cad = test1_dummy[, !(colnames(test1_dummy) %in% c("Cath1"))]
    fitted_values = round(predict(nnet, test1_no_cad))
    pred = ROCR::prediction(as.numeric(fitted_values), test1$Cath)
    # Calculate the AUC and confusion matrix.
    AUC = performance(pred, 'auc')@y.values
    conf_matrix = table(test1$Cath, fitted_values,
                        dnn = c("Actual", "Predicted"))
    # Print the result.
    print("Threshold1: ")
    print(paste0("Number of hidden layers: ",
                 hlayer, 
                 "; Number of neurons in each hidden layer: ", 
                 n_hneurons1[hneuron_index]))
    print(paste0("AUC: ", AUC))
    print(conf_matrix)
  }
}
# Threshold 2.
for (hlayer in 1:n_hlayers) {
  for (hneuron_index in 1:length(n_hneurons2)) {
    # Generate the tuned parameters for this iteration.
    hiddens = rep(n_hneurons2[hneuron_index], times = hlayer)
    # To reduce the randomness of the training, set the random seed.
    set.seed(316407)
    # Train the neural network.
    nnet = neuralnet(Cath1~., train2_dummy, hidden = hiddens, 
                     linear.output = FALSE)
    # Feed the test data into trained model to predict.
    test2_no_cad = test2_dummy[, !(colnames(test2_dummy) %in% c("Cath1"))]
    fitted_values = round(predict(nnet, test2_no_cad))
    pred = ROCR::prediction(as.numeric(fitted_values), test2$Cath)
    # Calculate the AUC and confusion matrix.
    AUC = performance(pred, 'auc')@y.values
    conf_matrix = table(test2$Cath, fitted_values,
                        dnn = c("Actual", "Predicted"))
    # Print the result.
    print("Threshold2: ")
    print(paste0("Number of hidden layers: ",
                 hlayer, 
                 "; Number of neurons in each hidden layer: ", 
                 n_hneurons2[hneuron_index]))
    print(paste0("AUC: ", AUC))
    print(conf_matrix)
  }
}
