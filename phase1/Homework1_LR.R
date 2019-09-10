#Load Packages 
library(haven)
library(tidyverse)
library(car)
library(plotly)
library(lubridate)
library(magrittr)
library(broom)
library(mgcv)
library(corrplot)
library(faraway)
library(mctest)

##########################################
#                                        #
#               Phase 1                  #
# Variable Understanding and Assumptions #
#                                        #
#           Mehak Shamdasani             #
#                                        #
##########################################


# Load data -----------------------------------------------------------------------------
insurance_t <- read_sas('insurance_t.sas7bdat')

# Explore data --------------------------------------------------------------------------
#Names() give you the names of all the variables in your dataset
names(insurance_t)

#Head() allows you to view the first fives rows of your data
head(insurance_t)

#Summary() gives you descriptive statistics for all of your variables 
#summary(insurance_t)

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
col_of_interest<- mv[mv> 1000] 
col_of_interest<- col_of_interest[col_of_interest <1100]
mv <- mv[mv>1075]
y<-c()
for (x in 1:length(mv)){
  y<- c(y, mv[[x]])
}
xname<-c("Total Income", "Homeowner","Length of Residence" , "Value of Home", "Age")
mv <-data.frame("variable" = xname, "missingVal" = y) 
xnames<- c('Age', "Credit Card Holder", "Credit Card Balance", "Number of Credit Card Purchase", "Homeowner", "Value of Home", "Total Income", "Investment Account Holder"
           , "Investment Account Balance", "Length of Residence (Years)", "Number of Telephone Banking Interactions", "Number of Point of Sale Interactions",
            'Total Amt for Point of Sale')


#Bar Chart of Missing Values
t <- list(
  family = "sans serif bold",
  size = 16,
  color = 'black')
missing_val <- plot_ly(mv, x = ~variable, y = ~missingVal, type = 'bar', color = I("blue")) %>%
  layout(xaxis = list(title = "<B>Variable</B>", font = t), yaxis = list(title = '<B>Count of Missing Values</B>', font=t))
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
insurance_t[cont_var]

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

#export to CSV
write_csv(cont, file.path(file.dir, "significantvars.csv"))

#Linearity for Variables--------------------------------------------------------------------------
fctCols <- c('DDA', 'DIRDEP','SAV', 'ATM', 'CD', 'IRA', 'LOC', 'INV',
             'ILS', 'MM', 'MTG', 'CC', 'SDB', 'HMOWN', 'MOVED', 'INAREA', 'NSF',
             'INS', 'BRANCH', 'RES', 'CCPURC', 'MMCRED', 'CASHBK' )

#create a list of the continuous variables
cont_var <- setdiff(colnames(insurance_t), fctCols)
contn<- insurance_t[cont_var]
c <- insurance_t$INS
contn<- cbind(contn, c)

#GAM for continuous variables
fit.gam <- gam(c ~ s(ACCTAGE) + s(AGE) + s(ATMAMT) + s(CCBAL) + s(CDBAL) + s(CHECKS) + s(CRSCORE) + s(DDABAL) + +s(DEP) + s(DEPAMT) + s(HMVAL) + s(INCOME) + s(INVBAL) + s(ILSBAL)+ s(IRABAL)+ s(LOCBAL) +s(LORES)+s(MMBAL)+ s(MTGBAL) +s(NSFAMT)+s(POS)+s(POSAMT) +s(PHONE) +s(SAVBAL)+s(TELLER),
               data=contn, family=binomial(link = 'logit'), method = 'REML')
results <- summary(fit.gam)

#extract p-vlaues for predictors
la_table<- data.frame(variables = rownames(results$s.table), pvalues= results$s.pv)
#export p-values to csv 
write_csv(la_table, file.path(file.dir, "linearitytable.csv"))
