#Install Packages
install.packages(c("corrplot", "haven", "tidyverse", "car", "plotly", 'mctest', 'faraway'))
install.packages("MuMIn")

#Load Packages 
library(haven)
library(tidyverse)
library(car)
library(plotly)
library(corrplot)
library(magrittr)
library(mctest)
library(faraway)
library(MuMIn)
library(MASS)

##########################################
#                                        #
#               Phase 1                  #
# Variable Understanding and Assumptions #
#                                        #
#           Mehak Shamdasani             #
#                                        #
##########################################


#Import Insurance Training Data (from sas7bdat)
insurance_t <- read_sas('/Users/mehak/Desktop/MSA/FALL2020/LogisticRegression/Homework1_LR/insurance_t.sas7bdat')

#Explore Data
#Names() give you the names of all the variables in your dataset
names(insurance_t)

#Head() allows you to view the first fives rows of your data
head(insurance_t)

#Summary() gives you descriptive statistics for all of your variables 
summary(insurance_t)


#Applies the sd function to all columns of insurance_t
sapply(insurance_t, sd)

#Cleaning Data
#Check the type of our variables
sapply(insurance_t, class)

#Create a list of the column names of categorical variables 
fctCol <- c('DDA', 'DIRDEP','SAV', 'ATM', 'CD', 'IRA', 'LOC', 'INV',
            'ILS', 'MM', 'MTG', 'CC', 'SDB', 'HMOWN', 'MOVED', 'INAREA',
            'INS', 'BRANCH', 'RES')
#Below is a way to check the levels of these vars before conversion
sapply(insurance_t[fctCol], unique)

#converting categorical variables to fct
insurance_t<- insurance_t %>%
  mutate_at(fctCol, as.factor)

#Check to see if this worked (should now be type 'factor')
sapply(insurance_t, class)

#Should be 0 and 1
levels(insurance_t$DDA) 
  

#Outliers, Influential Observations, Missing Data
#Option 1
sapply(insurance_t, function(x) sum(is.na(x)))
#Option 2
colSums(is.na(insurance_t))

#Check Assumptions: Multicollinearity 

#Select all Continuous variables 
ins_num <- insurance_t %>%
  select_if(., is.numeric)

#Get Pairwise Correlations
correlations <- cor(ins_num, use = 'complete.obs')

#(adapted from sthd.com)
#Format Correlations
flatten <- function(correl) {
  ut <- upper.tri(correl)
  data.frame(
    row = rownames(correl)[row(correl)[ut]],
    column = rownames(correl)[col(correl)[ut]],
    cor  =(correl)[ut]
  )}
correlations<- flatten(correlations)

#Get correlations above 0.7
correlations %>%
  filter(cor >0.7)
#POS and POSAMT - both describe point of sale interactions so this makes sense
#MTBBAL and CCBAL - Mortgage and Credit Card Balances 


#Get Correlation with all other variables
#Extract only eigenvalues (one for each predictor)
eigen(cor(ins_num, use = 'complete.obs'))$values

#Condition Number: Calculated two ways 
#A number above 100 indicates that there is a Multicollinearity problem
#Option1:
max(eigen(cor(ins_num, use = 'complete.obs'))$values)/min(eigen(cor(ins_num, use = 'complete.obs'))$values)
#Option2:
kappa(cor(ins_num, use = 'complete.obs'), exact = TRUE)

#Get VIF 
logit.model <- glm(INS ~., data = insurance_t, family = binomial(link = "logit"))
ins_vif <- car::vif(logit.model, y.name = fctCol)
#ILS, ILSBAL, MM, MMBAL, MTGBAL, CCBAL have VIFs above 10 


#Chi-Square Test for Categorical Vars
#Filter only factor variables 
ins_fct <- insurance_t %>%
  select_if(., is.factor)%>%
  select(DDA:INAREA, BRANCH, RES, INS) %>%
  drop_na()

colnames(ins_fct)



  




