library(ROCR) #AUC
library(kknn) #KNN
library(randomForest) #RF
library(e1071) #SVM



rm(list = ls())
ZAS_Original = read.csv("Z_Alizadeh_Sani_Dataset.csv")
ZAS = ZAS_Original
ZAS = ZAS[, !(colnames(ZAS) == "Exertional.CP")]
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
# BBB and VHD.
ZAS$BBB = as.factor(ZAS$BBB)
contrasts(ZAS$BBB) = contr.treatment(3)
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
KNN_AUCs = c()
KNN_accuracies = c()
for(index in 1:5) {
  # Iterate through each train-test sets combination.
  train = get(paste0("ZAS.train", index))
  test = get(paste0("ZAS.test", index))
  # Train the model and get the fitted values.
  KNN = kknn(Cath~., train, test)
  fitted_values = fitted(KNN)
  # Calculate and store the AUC of each round's model.
  pred = prediction(as.numeric(fitted_values), test$Cath)
  AUC = performance(pred, 'auc')@y.values
  KNN_AUCs = append(KNN_AUCs, as.numeric(AUC))
  # Calculate and store the accuracy of each round's model.
  conf_matrix = table(test$Cath, fitted_values, dnn = c("Actual", "Predicted"))
  accuracy = (conf_matrix[1] + conf_matrix[4]) / sum(conf_matrix)
  KNN_accuracies = append(KNN_accuracies, accuracy)
}
# Calculate and print the average AUC and accuracy of 5-round KNN models.
print(paste0("Average AUC for KNN: ", mean(KNN_AUCs)))
print(paste0("Average accuracy of KNN: ", mean(KNN_accuracies)))



# Random forest model (RF):
RF_AUCs = c()
RF_accuracies = c()
for(index in 1:5) {
  # Iterate through each train-test sets combination.
  train = get(paste0("ZAS.train", index))
  test = get(paste0("ZAS.test", index))
  # Train the model and get the fitted values.
  RF = randomForest(Cath~., data = train)
  test_no_cad = test[, !(colnames(test) %in% c("Cad"))]
  fitted_values = predict(RF, test_no_cad)
  # Calculate and store the AUC of each round's model.
  pred = prediction(as.numeric(fitted_values), test$Cath)
  AUC = performance(pred, 'auc')@y.values
  RF_AUCs = append(RF_AUCs, as.numeric(AUC))
  # Calculate and store the accuracy of each round's model.
  conf_matrix = table(test$Cath, fitted_values, dnn = c("Actual", "Predicted"))
  accuracy = (conf_matrix[1] + conf_matrix[4]) / sum(conf_matrix)
  RF_accuracies = append(RF_accuracies, accuracy)
}
# Calculate and print the average AUC and accuracy of 5-round KNN models.
print(paste0("Average AUC for RF: ", mean(RF_AUCs)))
print(paste0("Average accuracy of RF: ", mean(RF_accuracies)))



# Support Vector Machine:
SVM_AUCs = c()
SVM_accuracies = c()
for(index in 1:5) {
  # Iterate through each train-test sets combination.
  train = get(paste0("ZAS.train", index))
  test = get(paste0("ZAS.test", index))
  # Train the model and get the fitted values.
  SVM = svm(Cath~., data = train)
  test_no_cad = test[, !(colnames(test) %in% c("Cad"))]
  fitted_values = predict(SVM, test_no_cad)
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



# Need to be talked about:
# - How will the feature selection be integrated into model training?
# - Is it possible to build final outcome as an interface which can be connected
#   to both mobile app / website?
# - How will the app / dataset read inputs from user?
# - Make GitHub repository public / available to the teaching team.
