setwd("/Users/xinhaoyan/desktop")
rm(list = ls())

#read the dataset to be data frame
ZAS.dataset = read.csv("Z-Alizadeh sani dataset.csv")

#find the description
str(ZAS.dataset)

#set seed to make all teammates have the same random dataset
set.seed(3164)

#get 70% of the dataset to be training dataset
training.rows = sample(1:nrow(ZAS.dataset), 0.7*nrow(ZAS.dataset))
ZAS.train = ZAS.dataset[training.rows,]

#the rest of rows are testing dataset
ZAS.test = ZAS.dataset[-training.rows,]



