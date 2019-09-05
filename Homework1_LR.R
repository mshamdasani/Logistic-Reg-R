#Install Packages
install.packages(c("corrplot", "haven", "tidyverse", "car", "plotly", 'mctest', 'faraway'))
install.packages("MuMIn")

#Load Packages 
library(haven)
library(tidyverse)
library(car)
library(plotly)
library(lubridate)
library(magrittr)

##########################################
#                                        #
#               Phase 1                  #
# Variable Understanding and Assumptions #
#                                        #
#           Mehak Shamdasani             #
#                                        #
##########################################


# Load data -----------------------------------------------------------------------------
f
insurance_t <- read_sas('path to insurance_t.sas7bdat')

# Explore data --------------------------------------------------------------------------
#Names() give you the names of all the variables in your dataset
names(insurance_t)

#Head() allows you to view the first fives rows of your data
head(insurance_t)

#Summary() gives you descriptive statistics for all of your variables 
summary(insurance_t)

#Applies the sd function to all columns of insurance_t
sapply(insurance_t, sd)


# Clean data --------------------------------------------------------------------------
#Check whether variable types are correct
sapply(insurance_t, class)
 
#label Categorical Variables as Factors
#Create a list of the column names of categorical variables 
fctCol <- c('DDA', 'DIRDEP','SAV', 'ATM', 'CD', 'IRA', 'LOC', 'INV',
            'ILS', 'MM', 'MTG', 'CC', 'SDB', 'HMOWN', 'MOVED', 'INAREA', 'NSF',
            'INS', 'BRANCH', 'RES')
 
#check the levels of these vars before conversion
#sapply(insurance_t[fctCol], unique)
 
#convert the columns in fctCol to factor types
insurance_t<- insurance_t %>%
  mutate_at(fctCol, as.factor)
 
#check to see if your variables are correctly labeled as 'factor
sapply(insurance_t, class)


# Check for Missing Data --------------------------------------------------------------------------
#number of missing values for each variable
mv <- lapply(insurance_t, function(x) sum(is.na(x)))
mv <- mv[mv>1075]
for (x in 1:length(mv)){
  y<- c(y, mv[[x]])
}
xname<-c("Total Income", "Homeowner","Length of Residence" , "Value of Home", "Age")
mv <-data.frame("variable" = xname, "missingVal" = y) 
xnames<- c('Age', "Credit Card Holder", "Credit Card Balance", "Number of Credit Card Purchase", "Homeowner", "Value of Home", "Total Income", "Investment Account Holder"
           , "Investment Account Balance", "Length of Residence (Years)", "Number of Telephone Banking Interactions", "Number of Point of Sale Interactions",
            'Total Amt for Point of Sale')


#Bar Chart of Missing Values
missing_val <- plot_ly(mv, x = ~variable, y = ~missingVal, type = 'bar', color = I("blue"), name = 'Highest Number of Missing Values') %>%
  layout(xaxis = list(title = "Variable"), yaxis = list(title = 'Count'))
missing_val

# Multicollinearity Check--------------------------------------------------------------------------

#Get Pairwise Correlations
#create a subset with only continuous variables
ins_num <- insurance_t %>%
  select_if(., is.numeric)

#correlation matrix
correlations <- cor(ins_num, use = 'complete.obs')

#(adapted from sthd.com)
#format correlations
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
#POS, POSAMT, MTBBAL, and CCBAL

# Get Correlation with all other Variables
#extract only eigenvalues (one for each predictor)
eigen(cor(ins_num, use = 'complete.obs'))$values
 
#Condition Number
#ratio of the max eigenvalue to the min eigenvalue (>100 indicates multicollinearity)
max(eigen(cor(ins_num, use = 'complete.obs'))$values)/min(eigen(cor(ins_num, use = 'complete.obs'))$values)

# VIF(>10 indicates collinearity)
logit.model <- glm(INS ~., data = insurance_t, family = binomial(link = "logit"))
ins_vif <- car::vif(logit.model, y.name = fctCol)
#ILS, ILSBAL, MM, MMBAL, MTGBAL, CCBAL 
 

# Odds Ratio --------------------------------------------------------------------------
#create a subset of binrary predictor variables 
ins_fct <- insurance_t %>%
  select_if(., is.factor)%>%
  select(DDA:INAREA, NSF, INS) %>%
  drop_na()
 
#subset colnames of binary predictors
col<- colnames(ins_fct)
col <- col[1:17]

#odds ratio function
odds_ratio <- function(variable) {
  logit.model <- glm(ins_fct$INS ~ ins_fct[[variable]], family = binomial(link = "logit"))
  or <- exp(cbind(coef(logit.model), confint(logit.model)))
  return(or[2])
}

#compute OR of binary predictors
out <- lapply(col, odds_ratio)

#convert output to data.frame
out <- unlist(out,recursive=FALSE)
or <- data.frame("var" = col, 'OR' = out)

#export to CSV
write_csv(or, file.path(file.dir, "oddsratio.csv"))

 
# Calculate Significance for Continuous Variables--------------------------------------------------------------------------
fctCols <- c('DDA', 'DIRDEP','SAV', 'ATM', 'CD', 'IRA', 'LOC', 'INV',
             'ILS', 'MM', 'MTG', 'CC', 'SDB', 'HMOWN', 'MOVED', 'INAREA', 'NSF',
             'INS', 'BRANCH', 'RES', 'CCPURC', 'MMCRED', 'CASHBK' )

#create a list of the continuous variables
cont_var <- setdiff(colnames(insurance_t), fctCols)

#function to extract p-value for model
get_significance <- function(variable){
  res <- "INS"
  pred <- as.character(variable)
  logit.model<- glm(as.formula(paste0(res, "~", pred)), data=insurance_t, family = binomial(link = "logit"))
  return(coef(summary(logit.model))[2,'Pr(>|z|)'])
}

#compute p-values for list of continuous variables
pval<- lapply(cont_var, get_significance)
pval <- unlist(pval,recursive=FALSE)
#create a dataframe of significant continuous variables
cont <- data.frame("Variable" = cont_var, 'PValues' = pval)
cont<- filter(cont, PValues< 0.002)

#export to CSV
write_csv(o, file.path(file.dir, "significantvars.csv"))
 
