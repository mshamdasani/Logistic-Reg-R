install.packages(c('caret', 'leaps', 'rms'))
install.packages("SignifReg")
library(tidyverse)
library(caret)
library(leaps)
library(haven)
library(MASS)
library(dummies)
library(forcats)
library(stringr)

# Load data -----------------------------------------------------------------------------
insurance_t <- read_sas('/Users/mehak/Desktop/MSA/FALL2020/LogisticRegression/Homework1_LR/insurance_t.sas7bdat')

# Clean data --------------------------------------------------------------------------
#Check whether variable types are correct


#label Categorical Variables as Factors
#Create a list of the column names of categorical variables 
fctCol <- c('DDA', 'DIRDEP','SAV', 'ATM', 'CD', 'IRA', 'LOC', 'INV',
            'ILS', 'MM', 'MTG', 'CC', 'SDB', 'HMOWN', 'MOVED', 'INAREA', 'NSF',
            'INS', 'BRANCH', 'RES')

#convert the columns in fctCol to factor types and replace NA with missing
rep_na <- function(x) (factor(ifelse(is.na(x), 'Missing', paste(x))))
insurance_t<- insurance_t %>%
  mutate_at(fctCol, rep_na)

#check to see if your variables are correctly labeled as 'factor
sapply(insurance_t, class)
sapply(insurance_t, levels)

#create dummies for all factors in the data frame:
df <- data.frame(insurance_t)
insurance_t.new <- dummy.data.frame(df, sep = ".")

#create separate matrix of predictors excluding reference dummies
names(insurance_t.new)
predictvar <- (names(insurance_t.new) %in% c("INS.1",
                                             "INS.0",
                                             "DDA.0",
                                             "DIRDEP.0",
                                             "NSF.0",
                                             "SAV.0",
                                             "ATM.0",
                                             "CD.0",
                                             "IRA.0",
                                             "LOC.0",
                                             "INV.0",
                                             "ILS.0",
                                             "MM.0",
                                             "MTG.0",
                                             "CC.0",
                                             "SDB.0",
                                             "HMOWN.0",
                                             "MOVED.0",
                                             "INAREA.0",
                                             "BRANCH.19"))
predmatrix <- as.matrix(insurance_t.new)

# Backward Selection  ---------------------------------------------------------------------
#With regular data NA replaced
col<- str_c((colnames(insurance_t)[-46]), sep="+", collapse='+')
bw.model <-glm(paste('INS', col, sep='~'), data=insurance_t, family=binomial(link='logit'))

#logistic regression model (Brittany)
logit.model.w <- glm(INS.1 ~ predmatrix[,!predictvar], data=insurance_t.new, family = binomial(link = "logit") )
bw.models<- step(logit.model.w, alpha=0.002, direction='backward', criterion = "p-value")

