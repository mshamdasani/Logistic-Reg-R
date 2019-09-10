#reading in data:
library(haven)
library(dplyr)
install.packages("tidyverse")
install.packages("dummies")
library(dummies)
library(tidyverse)
library(forcats)

#read in dataset and create list of factors
insurance_t <-read_sas("C:\\Users\\17046\\Documents\\MSA 20\\Logistic Reg\\Homework2_LR\\insurance_t_bin.sas7bdat")

fctCol <- c('DDA', 'DIRDEP','SAV', 'ATM', 'CD', 'IRA', 'LOC', 'INV',
            'ILS', 'MM', 'MTG', 'CC', 'SDB', 'HMOWN', 'MOVED', 'INAREA', 'NSF',
            'INS', 'BRANCH', 'RES')

names(insurance_t)

df <- data.frame(insurance_t)

#turn categorical variables in the dataset into factors
insurance_t <- insurance_t %>%
  mutate_at(fctCol, as.factor)

#create dummies for all factors in the data frame:

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

#logistic regression model 
logit.model.w <- glm(INS.1 ~ predmatrix[,!predictvar], data=insurance_t.new, family = binomial(link = "logit") )
summary(logit.model.w)

#try to exclude reference categoriese to get fuller picture 
# of separation 