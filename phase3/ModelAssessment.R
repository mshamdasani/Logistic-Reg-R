library(MASS)
library(car)
library(DescTools)
library(ggplot2)
library(ROCR)
library(InformationValue)
library(haven)
library(forcats)
library(tidyverse)
#Import Training and Validation Set------------------------------------------------------------------------
insurance_t <-read_sas('/Users/mehak/Desktop/MSA/FALL2020/LogisticRegression/Homework1_LR/Logistic-Reg-R/phase3/insurance_t_bin.sas7bdat')
insurance_v<- read_sas('/Users/mehak/Desktop/MSA/FALL2020/LogisticRegression/Homework1_LR/Logistic-Reg-R/phase3/insurance_v_bin.sas7bdat')

#Recreate Factor Columns------------------------------------------------------------------------
#Will use for forward selection 
insurance_t <- data.frame(insurance_t)
insurance_v <- data.frame(insurance_v)

#Will use for backward selection 
insurance_t2<- data.frame(insurance_t)
insurance_v2 <- data.frame(insurance_v)

names <- names(insurance_t)
insurance_t[, names] <- lapply(insurance_t[,names], factor)

names <- names(insurance_t2)
insurance_t2[, names] <- lapply(insurance_t2[,names], factor)

names <- names(insurance_v)
insurance_v[, names] <- lapply(insurance_v[,names], factor)

names <- names(insurance_v2)
insurance_v2[, names] <- lapply(insurance_v2[,names], factor)

levels(insurance_t$CASHBK) <- list("0" = c("0","2"), "1" = c("1"))
levels(insurance_t2$CASHBK) <- list("0" = c("0","2"), "1" = c("1"))
levels(insurance_v$CASHBK) <- list("0" = c("0","2"), "1" = c("1"))
levels(insurance_v2$CASHBK) <- list("0" = c("0","2"), "1" = c("1"))

insurance_t = insurance_t %>% mutate_if(is.factor, fct_explicit_na, na_level= "M")
insurance_t2 = insurance_t2 %>% mutate_if(is.factor, fct_explicit_na, na_level= "M")
insurance_v = insurance_v %>% mutate_if(is.factor, fct_explicit_na, na_level= "M")
insurance_v2 = insurance_v2 %>% mutate_if(is.factor, fct_explicit_na, na_level= "M")




#Final Model for INS----------------------------------------------------------------------------------------
#Backward Selection
logit.model <-  glm(INS ~NSF + MTG + CC + SAVBAL_Bin + CDBAL_Bin +  MM+ CHECKS_Bin+ TELLER_Bin+ DDABAL_Bin+ ATMAMT_Bin+IRA +INV +ILS + DDA,
                              data=na.omit(insurance_t2),
                              family=binomial(link = "logit"))
full.model <- glm(INS ~DDA+CASHBK+DIRDEP+NSF+SAV+ATM+CD+IRA+LOC+INV+ILS+MM+MMCRED+MTG+CC+CCPURC+SDB+HMOWN+MOVED+INAREA+BRANCH+RES+ACCTAGE_Bin+DEPAMT_Bin+CHECKS_Bin+NSFAMT_Bin+PHONE_Bin+TELLER_Bin+SAVBAL_Bin+ATMAMT_Bin+POS_Bin+POSAMT_Bin+CDBAL_Bin+IRABAL_Bin+DDABAL_Bin+LOCBAL_Bin+INVBAL_Bin+ILSBAL_Bin+MTGBAL_Bin+CCBAL_Bin+INCOME_Bin+LORES_Bin+HMVAL_Bin+AGE_Bin+CRSCORE_Bin,
                  data=insurance_t,
                  family=binomial(link = "logit"))

back.model <- step(full.model, alpha=0.0001, direction = "backward")
back.model2 <- step(full.model, direction = "backward", k=log(8495))


#Forward Selection including Interactions
logit.forward.model <-  glm(INS ~  NSF + MTG + ILS + INV + IRA + DDA + TELLER_Bin + CC + ATMAMT_Bin + 
                              CHECKS_Bin + MM + CDBAL_Bin + DDABAL_Bin + CC*DDABAL_Bin + 
                              ATMAMT_Bin*DDABAL_Bin + IRA*DDA + DDA*CDBAL_Bin + TELLER_Bin*CDBAL_Bin + 
                              IRA*CDBAL_Bin + ILS*CC + IRA*TELLER_Bin + ATMAMT_Bin*CHECKS_Bin + 
                              MM*DDABAL_Bin + DDA*MM + MM*CDBAL_Bin + MTG*ILS + NSF*ILS + 
                              NSF*MTG + ILS*MM, data=na.omit(insurance_t),family=binomial(link = "logit"))


#Probability Metrics on Training Data for Backward Selection -----------------------------------------------------------------------
insurance_t2$p_hat <- predict(logit.model, type = "response")
#Concordance percentage
Concordance(insurance_t2$INS, insurance_t2$p_hat)
#0.7968226

#Discrimination slope (coefficient of discrimination and histogram)
p1 <- insurance_t2$p_hat[insurance_t2$INS == 1]
p0 <- insurance_t2$p_hat[insurance_t2$INS == 0]
coef_discrim <- mean(p1) - mean(p0)

ggplot(insurance_t2, aes(p_hat, fill = INS)) +
  geom_density(alpha = 0.7) +
  scale_fill_grey() +
  labs(x = "Predicted Probability",
       fill = "Outcome",
       title = paste("Coefficient of Discrimination = ",
                     round(coef_discrim, 3), sep = ""))

#Probability Metrics on Training Data for Forward Selection -----------------------------------------------------------------------
insurance_t$p_hat <- predict(logit.forward.model, type = "response")
#Concordance percentage
Concordance(insurance_t$INS, insurance_t2$p_hat)
#0.7968226

#Discrimination slope (coefficient of discrimination and histogram)
p1 <- insurance_t$p_hat[insurance_t$INS == 1]
p0 <- insurance_t$p_hat[insurance_t$INS == 0]
coef_discrim <- mean(p1) - mean(p0)

ggplot(insurance_t, aes(p_hat, fill = INS)) +
  geom_density(alpha = 0.7) +
  scale_fill_grey() +
  labs(x = "Predicted Probability",
       fill = "Outcome",
       title = paste("Coefficient of Discrimination = ",
                     round(coef_discrim, 3), sep = ""))

#Classificatiopn Metrics on Training Data for Backward Selection--------------------------------------------------------------------
#ROC curve
plotROC(insurance_t2$INS, insurance_t2$p_hat)
AUROC(insurance_t2$INS, insurance_t2$p_hat)

pred <- prediction(fitted(logit.model), factor(insurance_t2$INS))
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, lwd = 3, colorize = TRUE, colorkey = TRUE,
     colorize.palette = rev(gray.colors(256)))
abline(a = 0, b = 1, lty = 3)

#K-S Statistic
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
KS <- max(perf@y.values[[1]] - perf@x.values[[1]])
cutoffAtKS <- unlist(perf@alpha.values)[which.max(perf@y.values[[1]] - perf@x.values[[1]])]
print(c(KS, cutoffAtKS))

plot(x = unlist(perf@alpha.values), y = (1-unlist(perf@y.values)),
     type = "l", main = "K-S Plot (EDF)",
     xlab = 'Cut-off',
     ylab = "Proportion",
     col = "red")
lines(x = unlist(perf@alpha.values), y = (1-unlist(perf@x.values)), col = "blue")

#Classification Metrics on Training Data for Forward Selection  --------------------------------------------------------------------
#ROC curve
insurance_t$p_hat <- predict(logit.forward.model, type = "response")
plotROC(insurance_t$INS, insurance_t$p_hat)
AUROC(insurance_t$INS, insurance_t$p_hat)

pred <- prediction(fitted(logit.forward.model), factor(insurance_t$INS))
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, lwd = 3, colorize = TRUE, colorkey = TRUE,
     colorize.palette = rev(gray.colors(256)))
abline(a = 0, b = 1, lty = 3)

#K-S Statistic
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
KS <- max(perf@y.values[[1]] - perf@x.values[[1]])
cutoffAtKS <- unlist(perf@alpha.values)[which.max(perf@y.values[[1]] - perf@x.values[[1]])]
print(c(KS, cutoffAtKS))

plot(x = unlist(perf@alpha.values), y = (1-unlist(perf@y.values)),
     type = "l", main = "K-S Plot (EDF)",
     xlab = 'Cut-off',
     ylab = "Proportion",
     col = "red")
lines(x = unlist(perf@alpha.values), y = (1-unlist(perf@x.values)), col = "blue")

#Classification Metrics on Validation Data for Backward Selection ------------------------------------------------------------------
# confusion matrix
insurance_v2$p_hat <- predict(logit.model, newdata = insurance_v2, type = "response")
confusionMatrix(insurance_v2$INS, insurance_v2$p_hat, threshold = 0.5)

#Confusion Matrix with more Statistics
caret::confusionMatrix(data = as.factor(as.numeric(insurance_v2$p_hat >  0.3125111)), reference= insurance_v2$INS)


#Accuracy
acc <- NULL
cutoff<- NULL
for(i in 1:49){
  cutoff = c(cutoff, i/50)
  acc = c(acc, caret::confusionMatrix(data = as.factor(as.numeric(insurance_v2$p_hat >i/50)), reference= insurance_v2$INS)$overall[['Accuracy']])
}
ctable <- data.frame(cutoff, acc)


#lift
#fitted(logit.model)
pred <- prediction(insurance_v2$p_hat, insurance_v2$INS)
perf <- performance(pred, measure = "lift", x.measure = "rpp")
plot(perf, lwd = 3, colorize = TRUE, colorkey = TRUE,
     colorize.palette = rev(gray.colors(256)),
     main = "Lift Chart for Validation Data")
abline(h = 1, lty = 3)


#Classification Metrics on Validation Data On Interaction Model ------------------------------------------------------------------
# confusion matrix
insurance_v$p_hat <- predict(logit.forward.model, newdata = insurance_v, type = "response")
confusionMatrix(insurance_v$INS, insurance_v$p_hat, threshold = 0.5)

#Confusion Matrix with more Statistics
caret::confusionMatrix(data = as.factor(as.numeric(insurance_v$p_hat >   0.3298625)), reference= insurance_v$INS)


#Accuracy
acc <- NULL
cutoff<- NULL
for(i in 1:49){
  cutoff = c(cutoff, i/50)
  acc = c(acc, caret::confusionMatrix(data = as.factor(as.numeric(insurance_v$p_hat >i/50)), reference= insurance_v$INS)$overall[['Accuracy']])
}
ctable <- data.frame(cutoff, acc)


#lift
#fitted(logit.forward.model)
pred <- prediction(insurance_v$p_hat, insurance_v$INS)
perf <- performance(pred, measure = "lift", x.measure = "rpp")
plot(perf, lwd = 3, colorize = TRUE, colorkey = TRUE,
     colorize.palette = rev(gray.colors(256)),
     main = "Lift Chart for Validation Data")
abline(h = 1, lty = 3)