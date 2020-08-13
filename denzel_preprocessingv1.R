#Load library packages

library(readxl) #Used to read xlsx dataset into r
library(summarytools) #Used to create the descriptive statistics table
library(stringr) #Primarily for str_replace_all function to remove all spaces in column names

#Clear the environment
rm(list = ls())

#Load the dataset xlsx file into R
cad = read_excel("Z_Alizadeh_Sani_Dataset.xlsx")

#Print the first 5 entries in the dataset to understand what the data looks like.
head(cad)

#Shows that there are 303 rows and 56 variables in this dataset.
dim(cad)

#Prints a table containing a summary of descriptive statistics
view(dfSummary(cad))

#Returns the number of null values in dataset; in this case there are zero.
sum(is.na(cad))

#Now we want to view the number of categorical variables in the dataset
#The dfSummary function already shows this but we can double check using sapply
#There are 21 categorical variables and 35 numerical.
sapply(cad, class)

#Many column names were found to have spaces, which would prevent them from being referenced in functions
#This code replaces the spaces in a column name with a "_" 
names(cad) = str_replace_all(names(cad), c(" " = "_"))

#Creating 80% training and 20% testing data sets
set.seed(28790502) #Just used student ID as seed, should decide on a common seed to use
train.row = sample(1:nrow(cad), 0.8*nrow(cad))
cad.train = cad[train.row,]
cad.test = cad[-train.row,]