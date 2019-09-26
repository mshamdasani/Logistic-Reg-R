
library(DescTools)
library(ggplot2)
library(ROCR)
library(InformationValue)
library(haven)
library(dplyr)
library(dummies)
library(tidyverse)
library(forcats)
library(Hmisc)
library(glmulti)
library(jtools)
options("jtools-digits" = 8)
library(MASS)
library(car)
library(rJava)
library(givitiR)
library(pROC)
library(reshape2)
# Re-use final model from Phase 2 (Assuming backward selection)

#------------------------------------------------- Read in SAS table--------------------------
ins_t <-read_sas("C:\\Users\\ryanm\\OneDrive\\Documents\\Logistic_Regression\\HMW3\\Data\\insurance_t_bin.sas7bdat")
ins_v<- read_sas("C:\\Users\\ryanm\\OneDrive\\Documents\\Logistic_Regression\\HMW3\\Data\\insurance_v_bin.sas7bdat")
df_t <- data.frame(ins_t)
df_v <- data.frame(ins_v)

names <- names(df_t)
df_t[, names] <- lapply(df_t[,names], factor)
df_v[, names] <- lapply(df_v[,names],factor)

# Change missing levels to missing category

df_t <- df_t %>% mutate_if(is.factor, fct_explicit_na, na_level= "M")
df_v <- df_v %>% mutate_if(is.factor, fct_explicit_na, na_level= "M")

# Create logistic regression model with all variables

#-------------------------------Recreate model from Phase 2-----------------------------------
back.model <- glm(formula = INS ~ DDA + NSF + IRA + INV + ILS + MM + MTG + 
             CC + CHECKS_Bin + TELLER_Bin + SAVBAL_Bin + ATMAMT_Bin + 
             CDBAL_Bin + DDABAL_Bin, family = binomial(link = "logit"), 
           data = df_t)
AIC(back.model)


pred <- prediction(fitted(back.model), factor(df_t$INS))
#Creating pred probs with back-model
df_t$p_hat <- predict(back.model,type = 'response')


# Concordance for backward selection model
Concordance(df_t$INS, df_t$p_hat)


# Discrimination slope (predicted probabilities)
# Coefficients of discrimination and histogram visuals
p1 <- df_t$p_hat[ins_t$INS ==1]
p0 <- df_t$p_hat[ins_t$INS == 0]


#-----------------------------------------------------------Coef Discrim & Plot----------------------------------------------------------------------------------------
coef_discrim <- mean(p1) - mean(p0)

ggplot(df_t, aes(p_hat, fill = INS)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c('#FC4E07','#00AFBB'),labels = c('Not Purchased', 'Purchased')) +
  geom_segment(aes(x = mean(p0),xend = mean(p1), y = 2.5, yend = 2.5))+
  geom_segment(aes(x = mean(p0),xend = mean(p0), y = 2.3, yend = 2.7))+
  geom_segment(aes(x = mean(p1),xend = mean(p1), y = 2.3, yend = 2.7))+
  annotate(geom = 'Text', x = 0.375, y = 2.8, label = 'D = 0.24')+
  labs(x = "Predicted Probability",
       y = 'Density',
       title = 'Predicted Probabilities Distributions',
       subtitle = 'Levels of INS')+
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust= 0.5),legend.title=element_blank())



#-----------------------------------------------------------ROC Curve Plot--------------------------------------------------------------------------------------------------------
# creates ROC curve
# ROC Curve - ROCR Package #
rocobj <- roc(df_t$INS, df_t$p_hat)
str(rocobj)
ggroc(rocobj,legacy.axes = TRUE, color = '#00AFBB')+
  geom_segment(x = 0, xend = 1, y = 0, yend = 1)+
  labs(x = 'FPR',
       y = 'TPR',
       title = 'ROC Curve',
       subtitle = 'Backward Selection Model')+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust= 0.5),
        legend.title=element_blank())






#---------------------------------------------------------- K-S Statistics-------------------------------------------------------------------------------------
#Getting performance stats
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

#Extracting information to generate cumulative dist for event and non event
cut_off <- unlist(perf@alpha.values)
prob_0 <- 1-unlist(perf@y.values)
prob_1 <- 1-unlist(perf@x.values)




#Creating dataframe for plotting purposes
ksd <- data.frame( 'index' = cut_off, 'one' = prob_1, 'zero' = prob_0, 'diff' = prob_1-prob_0 )

#Finding optimal KS stat cutoff
ksd_optimal <- ksd$index[ksd$diff==max(ksd$diff)]

#Generating KS stat
ks_stat <- ks_stat(df_t$INS, df_t$p_hat)
ks_stat_test <- ks.test(ks_stat)

ksd_sub <- ksd[,c(1,2,3)]
ksd_sub = melt(ksd_sub,id = c('index'))

ggplot(ksd_sub)+ geom_line(aes(x = index, y = value,color = variable))+
  scale_colour_manual(values = c('#00AFBB','#FC4E07'),labels = c('Event', 'Non-Event'))+
  geom_segment(x =ksd_optimal, xend  = ksd_optimal, y = -0.1, yend = 1.1, linetype = 2)+
  geom_segment(x = ksd_optimal, xend = 0.365, y = 0.125,yend = 0.125, arrow = arrow(length = unit(0.1, 'cm')) )+
  annotate(geom = 'Text', x  = ksd_optimal +0.25, y= 0.135, label = 'Maximized KS Threshold'  )+
  
  labs(title =  'K-S Plot (EDF)',
       subtitle = 'KS Maximized at  0.31 Threshold',
       y = "Proportion",
       x = "Cut-off") + theme_bw(base_size = 15)+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(size = 10,hjust= 0.5),
        legend.title=element_blank(),
        panel.background = element_rect(fill = 'grey90'),
        panel.border = element_rect(color = 'white'),
        panel.grid = element_line(color = 'white'))


#--------------------------------Creating predicted probs on Validation Data set----------------------------------------------
back.model_v <- glm(formula = INS ~ DDA + NSF + IRA + INV + ILS + MM + MTG + 
                    CC + CHECKS_Bin + TELLER_Bin + SAVBAL_Bin + ATMAMT_Bin + 
                    CDBAL_Bin + DDABAL_Bin, family = binomial(link = "logit"), 
                  data = df_v)
df_v$p_hat <- predict(back.model_v,type = 'response')


#Creating Confusion Matrix
conf_matrix <-confusionMatrix(df_v$INS,df_v$p_hat, threshold = ksd_optimal)

#Calculating Accuracy
acc <- (conf_matrix[1,1] + conf_matrix[2,2])/ (conf_matrix[1,1] + conf_matrix[2,2]+conf_matrix[1,2]+conf_matrix[2,1])

#Calculating prediction info for validation data set
pred_v <- prediction(fitted(back.model_v), factor(df_v$INS))

#Generating Lift charts
lift <- performance(pred_v, measure = "lift", x.measure = "rpp")
print(lift)
x_lift <- unlist(lift@x.values)
y_lift <- unlist(lift@y.values)

#Creating lift Dataframe for ggplot purposes
lift_df <- data.frame('x'= x_lift, 'y' = y_lift)

#mutating depth to percentages
lift_df <- mutate(lift_df, x = x*100)

#Creating lift chart
lift_df %>% ggplot(aes(x = x, y = y))+
  geom_line()+
  geom_segment(x = 20, xend = 20, y = 0, yend = 1.89, linetype= 2, color = 'red')+

  labs(x = 'Depth (%) ',
       y = 'Lift',
       title = 'Lift Chart for Validation Data')+
  
    theme(plot.title = element_text(hjust = 0.5)
    )
