setwd("/Users/xinhaoyan/desktop")
rm(list = ls())

#read the dataset to be data frame
ZAS.dataset = read.csv("Z-Alizadeh sani dataset.csv")

#find the description
str(ZAS.dataset)


#DLP
ZAS.dataset$DLP = as.numeric(ZAS.dataset$DLP)-1
#Weak Peripheral Pulse
ZAS.dataset$Weak.Peripheral.Pulse = as.numeric(ZAS.dataset$Weak.Peripheral.Pulse)-1
#Lung rales
ZAS.dataset$Lung.rales = as.numeric(ZAS.dataset$Lung.rales)-1
#Systolic Murmur
ZAS.dataset$Systolic.Murmur = as.numeric(ZAS.dataset$Systolic.Murmur)-1
#Diastolic Murmur
ZAS.dataset$Diastolic.Murmur = as.numeric(ZAS.dataset$Diastolic.Murmur)-1
#Dyspnea
ZAS.dataset$Dyspnea = as.numeric(ZAS.dataset$Dyspnea)-1
#Atypical
ZAS.dataset$Atypical = as.numeric(ZAS.dataset$Atypical)-1


#set seed to make all teammates have the same random dataset
set.seed(3164)

#get 70% of the dataset to be training dataset
training.rows = sample(1:nrow(ZAS.dataset), 0.7*nrow(ZAS.dataset))
ZAS.train = ZAS.dataset[training.rows,]

#the rest of rows are testing dataset
ZAS.test = ZAS.dataset[-training.rows,]