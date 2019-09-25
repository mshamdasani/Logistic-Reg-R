###############################
#                             #
#     Logistic Regression:    #
#           Phase 3           #
#                             #
#        Ryan Ankersen        #
#                             #
###############################







library(MASS)
library(car)
library(DescTools)
library(ggplot2)
library(ROCR)
library(InformationValue)
library(haven)


#Creating dataframes of validation and training data sets
file_train <- "C:\\Users\\ryanm\\OneDrive\\Documents\\Logistic_Regression\\HMW3\\Data\\insurance_t_bin.sas7bdat"
file_val <- "C:\\Users\\ryanm\\OneDrive\\Documents\\Logistic_Regression\\HMW3\\Data\\insurance_v_bin.sas7bdat"
training <- read_sas(file_train)
val <- read_sas(file_val)


#Recreating model from phase 2 

names <- names(df)
df[, names] <- lapply(df[,names], factor)
# Change missing levels to missing category
df <- df %>% mutate_if(is.factor, fct_explicit_na, na_level= "M")
# Create logistic regression model with all variables
logit.model.w <- glm(INS ~., data=df, family = binomial(link = "logit") )
summary(logit.model.w)
# Creates full model
full.model <- glm(INS ~ DDA+CASHBK+DIRDEP+NSF+SAV+ATM+CD+IRA+LOC+INV+ILS+MM+MMCRED+MTG+CC+CCPURC
                  +SDB+HMOWN+MOVED+INAREA+BRANCH+RES+ACCTAGE_Bin+DEPAMT_Bin+CHECKS_Bin+NSFAMT_Bin
                  +PHONE_Bin+TELLER_Bin+SAVBAL_Bin+ATMAMT_Bin+POS_Bin+POSAMT_Bin+CDBAL_Bin+IRABAL_Bin
                  +DDABAL_Bin+LOCBAL_Bin+INVBAL_Bin+ILSBAL_Bin+MTGBAL_Bin+CCBAL_Bin+INCOME_Bin+LORES_Bin
                  +HMVAL_Bin+AGE_Bin+CRSCORE_Bin, data=df,
                  family=binomial(link = "logit"))
# Creates empty model
empty.model <- glm(INS ~ 1, data=df,
                   family= binomial(link = "logit"))
# Creates model from backward selection
back.model <- step(full.model, direction = "backward", k=log(8495)
