library(MASS)
library(visreg)
library(brglm)
library(car)
library(mgcv)


#This sets working directory, CWD in python
setwd("C:\\Users\\ryanm\\OneDrive\\Documents\\")

#Reading in Dataframes from CSV files
insurt_df <- read.csv(file = "insurance_t.csv", header = TRUE)
insurv_df <- read.csv(file = 'insurance_v.csv',header = TRUE)

#calculating percentage of customers that purchased products

#finding position of column 'INS' in R data frame
pos_ins<- grep('INS',colnames(insurt_df))

#calculating percentage of customers who bought product sum(INS)/length(INS)
percentage <- sum(insurt_df[,pos_ins])/length(insurt_df[,pos_ins])


#------------------EDA of predictor variables------------------


#Credit Score

#Calculating Mean of Column
CRSCORE_avg = mean(insurt_df[,'CRSCORE'],na.rm = TRUE)

#Finding means of all variables in dataframe
averages <-sapply(insurt_df, mean, na.rm = TRUE)
sds <- sapply(insurt_df, sd,na.rm = TRUE)
medians <- sapply(insurt_df,median,na.rm = TRUE)

#Retrieving particular values
print(averages['ACCTAGE'])