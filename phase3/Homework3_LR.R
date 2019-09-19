library(MASS)
library(car)
library(DescTools)
library(ggplot2)
library(ROCR)
library(InformationValue)
library(haven)

#Import Training and Validation Set------------------------------------------------------------------------
insurance_t <-read_sas('/Users/mehak/Desktop/MSA/FALL2020/LogisticRegression/Homework1_LR/Logistic-Reg-R/phase3/insurance_t_bin.sas7bdat')
insurance_v<- read_sas('/Users/mehak/Desktop/MSA/FALL2020/LogisticRegression/Homework1_LR/Logistic-Reg-R/phase3/insurance_v_bin.sas7bdat')

#Recreate Factor Columns------------------------------------------------------------------------
insurance_t <- data.frame(insurance_t)
insurance_v <- data.frame(insurance_v)

names <- names(insurance_t)
insurance_t[, names] <- lapply(insurance_t[,names], factor)

names <- names(insurance_v)
insurance_v[, names] <- lapply(insurance_v[,names], factor)

insurance_t = insurance_t %>% mutate_if(is.factor, fct_explicit_na, na_level= "M")
insurance_v = insurance_v %>% mutate_if(is.factor, fct_explicit_na, na_level= "M")


#Final Model for INS----------------------------------------------------------------------------------------
logit.model <-  glm(INS ~NSF + MTG + CC + SAVBAL_Bin + CDBAL_Bin +  MM+ CHECKS_Bin+ TELLER_Bin+ DDABAL_Bin+ ATMAMT_Bin+IRA +INV +ILS + DDA,
                              data=na.omit(insurance_t),
                              family=binomial(link = "logit"))

logit.forward.model <-  glm(INS ~  MTG + CC  + CDBAL_Bin +  MM+ TELLER_Bin+ DDABAL_Bin+IRA +INV +ILS + DDA+IRA*DDA,
                    data=na.omit(insurance_t),
                    family=binomial(link = "logit"))
#Probability Metrics on Training Data-----------------------------------------------------------------------
insurance_t$p_hat <- predict(logit.model, type = "response")
#Concordance percentage
Concordance(insurance_t$INS, insurance_t$p_hat)
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
#Classification Metrics on Training Data--------------------------------------------------------------------
#ROC curve
plotROC(insurance_t$INS, insurance_t$p_hat)
AUROC(insurance_t$INS, insurance_t$p_hat)

pred <- prediction(fitted(logit.model), factor(insurance_t$INS))
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

#Classification Metrics on Validation Data------------------------------------------------------------------
# confusion matrix
insurance_v$p_hat <- predict(logit.model, newdata = insurance_v, type = "response")
confusionMatrix(insurance_v$INS, insurance_v$p_hat, threshold = 0.5)

#Confusion Matrix with more Statistics
caret::confusionMatrix(data = as.factor(as.numeric(insurance_v$p_hat >  0.3125111)), reference= insurance_v$INS)


#Accuracy
acc <- NULL
cutoff<- NULL
for(i in 1:49){
  cutoff = c(cutoff, i/50)
  acc = c(acc, caret::confusionMatrix(data = as.factor(as.numeric(insurance_v$p_hat >i/50)), reference= insurance_v$INS)$overall[['Accuracy']])
}
ctable <- data.frame(cutoff, acc)


#lift
pred <- prediction(fitted(logit.model), insurance_v$INS)
perf <- performance(pred, measure = "lift", x.measure = "rpp")
plot(perf, lwd = 3, colorize = TRUE, colorkey = TRUE,
     colorize.palette = rev(gray.colors(256)),
     main = "Lift Chart for Validation Data")
abline(h = 1, lty = 3)