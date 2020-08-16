setwd("/Users/xinhaoyan/desktop")
rm(list = ls())

#read the dataset to be data frame
ZAS.dataset = read.csv("Z-Alizadeh sani dataset.csv")

#find the description
str(ZAS.dataset)


#DLP
ZAS.dataset$DLP = as.numeric(ZAS.dataset$DLP)-1
ZAS.dataset$DLP = as.factor(ZAS.dataset$DLP)
#Weak Peripheral Pulse
ZAS.dataset$Weak.Peripheral.Pulse = as.numeric(ZAS.dataset$Weak.Peripheral.Pulse)-1
ZAS.dataset$Weak.Peripheral.Pulse = as.factor(ZAS.dataset$Weak.Peripheral.Pulse)
#Lung rales
ZAS.dataset$Lung.rales = as.numeric(ZAS.dataset$Lung.rales)-1
ZAS.dataset$Lung.rales = as.factor(ZAS.dataset$Lung.rales)
#Systolic Murmur
ZAS.dataset$Systolic.Murmur = as.numeric(ZAS.dataset$Systolic.Murmur)-1
ZAS.dataset$Systolic.Murmur = as.factor(ZAS.dataset$Systolic.Murmur)
#Diastolic Murmur
ZAS.dataset$Diastolic.Murmur = as.numeric(ZAS.dataset$Diastolic.Murmur)-1
ZAS.dataset$Diastolic.Murmur = as.factor(ZAS.dataset$Diastolic.Murmur)
#Dyspnea
ZAS.dataset$Dyspnea = as.numeric(ZAS.dataset$Dyspnea)-1
ZAS.dataset$Dyspnea = as.factor(ZAS.dataset$Dyspnea)
#Atypical
ZAS.dataset$Atypical = as.numeric(ZAS.dataset$Atypical)-1
ZAS.dataset$Atypical = as.factor(ZAS.dataset$Atypical)

#Function Class
ZAS.dataset$Function.Class = as.factor(ZAS.dataset$Function.Class)
contrasts(ZAS.dataset$Function.Class) = contr.treatment(4)
contrasts(ZAS.dataset$Function.Class)
#Region RWMA
#transfer the data type from integer to factor
ZAS.dataset$Region.RWMA = as.factor(ZAS.dataset$Region.RWMA)
contrasts(ZAS.dataset$Region.RWMA) = contr.treatment(5)
#print the matrix
contrasts(ZAS.dataset$Region.RWMA)


#set seed to make all teammates have the same random dataset
set.seed(3164)

#get 70% of the dataset to be training dataset
training.rows = sample(1:nrow(ZAS.dataset), 0.7*nrow(ZAS.dataset))
ZAS.train = ZAS.dataset[training.rows,]

#the rest of rows are testing dataset
ZAS.test = ZAS.dataset[-training.rows,]