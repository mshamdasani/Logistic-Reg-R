#reading in data:
library(haven)
library(dplyr)
library(dummies)
library(tidyverse)
library(forcats)
library(Hmisc)
library(glmulti)
library(jtools)
library(ggplot2)
library(oddsratio)
options("jtools-digits" = 8)

#read in dataset and create list of factors
insurance_t <-read_sas("C:\\Users\\17046\\Documents\\MSA 20\\Logistic Reg\\Homework2_LR\\insurance_t_bin.sas7bdat")

df <- data.frame(insurance_t)

#turn variables in the dataset into factors
names <- names(df) 
df[, names] <- lapply(df[,names], factor)

df = df %>% mutate_if(is.factor, fct_explicit_na, na_level= "M")
names(df)

#plotting variable distributions 
ggplot() +
  geom_histogram(aes(df$MMBAL_Bin), stat="count")
ggplot() +
  geom_histogram(aes(df$BRANCH), stat="count")
ggplot() +
  geom_histogram(aes(df$CASHBK), stat="count")

#creating a logit model with new data to check separation
logit.model.w <- glm(INS ~., data=df, family = binomial(link = "logit") )
summary(logit.model.w)

#collapsing bins into one another for MMBAL_Bin and CASHBK_BIN
levels(df$CASHBK) <- list("0" = c("0","2"), "1" = c("1"))
levels(df$CASHBK)

#ODDS RATIOS
or_glm(data=df, model = logit.model.w)
exp(
  cbind(coef(logit.model.w), confint(logit.model.w))
  )

#creating all the different models

full.model <- glm(INS ~DDA+CASHBK+DIRDEP+NSF+SAV+ATM+CD+IRA+LOC+INV+ILS+MM+MMCRED+MTG+CC+CCPURC+SDB+HMOWN+MOVED+INAREA+BRANCH+RES+ACCTAGE_Bin+DEPAMT_Bin+CHECKS_Bin+NSFAMT_Bin+PHONE_Bin+TELLER_Bin+SAVBAL_Bin+ATMAMT_Bin+POS_Bin+POSAMT_Bin+CDBAL_Bin+IRABAL_Bin+DDABAL_Bin+LOCBAL_Bin+INVBAL_Bin+ILSBAL_Bin+MTGBAL_Bin+CCBAL_Bin+INCOME_Bin+LORES_Bin+HMVAL_Bin+AGE_Bin+CRSCORE_Bin,
                  data=df,
                  family=binomial(link = "logit"))

empty.model <- glm(INS ~ 1, data=df,
                   family= binomial(link = "logit"))

#backwards model with AIC
back.model <- step(full.model, direction = "backward")

#backwards binary log model with BIC
back.model2 <- step(full.model, direction = "backward", k=log(8495))

summ(back.model)
summ(back.model2)
summary(back.model2)

#ODDS RATIOS
or_glm(data=df, model = back.model2)
exp(
  cbind(coef(back.model2), confint(back.model2))
)

#testing for interactions and forward selection
forward <- glm(INS ~ (NSF+MTG+ILS+INV+IRA+DDA+TELLER_Bin+CC+ATMAMT_Bin+CHECKS_Bin+MM+CDBAL_Bin+DDABAL_Bin)^2,
              data=insurance_t, family=binomial(link = "logit"))

main.model<- glm(INS ~ NSF+MTG+ILS+INV+IRA+DDA+TELLER_Bin+CC+ATMAMT_Bin+CHECKS_Bin+MM+CDBAL_Bin+DDABAL_Bin,
               data=df, family=binomial(link = "logit"))

forward.model <- step(main.model, 
                  scope = list(lower=formula(main.model), upper=formula(forward)), 
                  direction = "forward")

summ(forward.model)
