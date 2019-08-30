#Install Packages
install.packages(c("corrplot", "haven", "tidyverse", "car", "plotly"))

#Load Packages 
library(haven)
library(tidyverse)
library(car)
library(plotly)
library(corrplot)

#Import Insurance Data (this function is a part of the haven library)
insurance_t <- read_sas('/Users/mehak/Desktop/MSA/FALL2020/LogisticRegression/Homework1_LR/insurance_t.sas7bdat')

#Explore Data
#Names() give you the names of all the variables in your dataset
names(insurance_t)
#Summary() gives you descriptive statistics for all of your variables 
summary(insurance_t)


#If you want a specific metric for all the variables such as standard deviation, you can use sapply(data, measure)
sapply(insurance_t, sd)

#Outliers and Influential Observations 

#Check Assumptions: Multicollinearity 
correlations <- cor(Smarket[,1:8])
corrplot(correlations, method="circle")

#

#Check Assumptions: Linearity of Continuous Vars (BoxTidwell)
#boxTidwell(y ~ x1 + x2, data = insurance)

e^1.2
