library(ROCR) #AUC
library(kknn) #KNN
library(randomForest) #RF
library(e1071) #SVM
library(caret) #filter feature selection



rm(list = ls())
ZAS_Original = read.csv("Z_Alizadeh_Sani_Dataset.csv")
ZAS = ZAS_Original
# Find predictor features with zero / nearly-zero variances.
# Record their column indexes.
nzv_feature_indexes = nearZeroVar(ZAS[, !(colnames(ZAS) %in% c("Cath"))])
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
types = sapply(ZAS, class)
print(paste0("The number of factor typed features: ",
             length(grep("factor", types))))
print(paste0("The number of numeric / integer typed features: ", 
             length(grep("numeric", types)) + length(grep("integer", types))))
print(types)
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



# K-nearest neighbor model (KNN):
# Create vectors containing AUC and accuracy of each round's model.
KNN_AUCs = c()
KNN_accuracies = c()
for(index in 1:5) {
  # Iterate through each train-test sets combination.
  train = get(paste0("ZAS.train", index))
  test = get(paste0("ZAS.test", index))
  # Train the model and get the fitted values.
  KNN = kknn(Cath~., train, test)
  fitted_values = fitted(KNN)
  # Calculate and store the AUC.
  pred = prediction(as.numeric(fitted_values), test$Cath)
  AUC = performance(pred, 'auc')@y.values
  KNN_AUCs = append(KNN_AUCs, as.numeric(AUC))
  # Calculate and store the accuracy.
  conf_matrix = table(test$Cath, fitted_values, dnn = c("Actual", "Predicted"))
  accuracy = (conf_matrix[1] + conf_matrix[4]) / sum(conf_matrix)
  KNN_accuracies = append(KNN_accuracies, accuracy)
}
# Calculate and print the average AUC and accuracy of 5-round KNN models.
print(paste0("Average AUC for KNN: ", mean(KNN_AUCs)))
print(paste0("Average accuracy of KNN: ", mean(KNN_accuracies)))



# Random forest model (RF):
# Create vectors containing AUC and accuracy of each round's model.
RF_AUCs = c()
RF_accuracies = c()
RF_importance = as.data.frame((matrix(ncol = 0, nrow = 40)))
for(index in 1:5) {
  # Iterate through each train-test sets combination.
  train = get(paste0("ZAS.train", index))
  test = get(paste0("ZAS.test", index))
  # Train the model and get the fitted values.
  RF = randomForest(Cath~., data = train)
  test_no_cath = test[, !(colnames(test) %in% c("Cath"))]
  fitted_values = predict(RF, test_no_cath)
  # Calculate and store the AUC.
  pred = prediction(as.numeric(fitted_values), test$Cath)
  AUC = performance(pred, 'auc')@y.values
  RF_AUCs = append(RF_AUCs, as.numeric(AUC))
  # Calculate and store the accuracy.
  conf_matrix = table(test$Cath, fitted_values, dnn = c("Actual", "Predicted"))
  accuracy = (conf_matrix[1] + conf_matrix[4]) / sum(conf_matrix)
  RF_accuracies = append(RF_accuracies, accuracy)
  # Calculate and store the features importance in each round.
  current_RF_imp = as.data.frame(RF$importance)
  RF_importance = cbind(RF_importance, current_RF_imp)
  names(RF_importance)[length(names(RF_importance))] = paste0("Round_", index)
}
# Calculate, store and show each feature's average importance.
RF_importance = as.data.frame(sort(apply(RF_importance, 1, mean),
                                   decreasing = TRUE))
colnames(RF_importance) = "Average Importance"
print(RF_importance)
write.csv(RF_importance, "RandomForest.csv")
# Calculate and print the average AUC and accuracy of 5-round KNN models.
print(paste0("Average AUC for RF: ", mean(RF_AUCs)))
print(paste0("Average accuracy of RF: ", mean(RF_accuracies)))



# Support Vector Machine (SVM):
# Create vectors containing AUC and accuracy of each round's model.
SVM_AUCs = c()
SVM_accuracies = c()
for(index in 1:5) {
  # Iterate through each train-test sets combination.
  train = get(paste0("ZAS.train", index))
  test = get(paste0("ZAS.test", index))
  # Train the model and get the fitted values.
  SVM = svm(Cath~., data = train)
  test_no_cath = test[, !(colnames(test) %in% c("Cath"))]
  fitted_values = predict(SVM, test_no_cath)
  # Calculate and store the AUC of each round's model.
  pred = prediction(as.numeric(fitted_values), test$Cath)
  AUC = performance(pred, 'auc')@y.values
  SVM_AUCs = append(SVM_AUCs, as.numeric(AUC))
  # Calculate and store the accuracy of each round's model.
  conf_matrix = table(test$Cath, fitted_values, dnn = c("Actual", "Predicted"))
  accuracy = (conf_matrix[1] + conf_matrix[4]) / sum(conf_matrix)
  SVM_accuracies = append(SVM_accuracies, accuracy)
}
# Calculate and print the average AUC and accuracy of 5-round KNN models.
print(paste0("Average AUC for SVM: ", mean(SVM_AUCs)))
print(paste0("Average accuracy of SVM: ", mean(SVM_accuracies)))
