import pandas as pd
import numpy as np
import math
import path
import scipy.stats


#Creating dataframes for both validation and training set using pandas.read_csv method.
training_df = pd.read_csv('C:\\Users\\ryanm\\OneDrive\\Documents\\Logistic_hw\\insurance_t.csv')
valid_df = pd.read_csv('C:\\Users\\ryanm\\OneDrive\\Documents\\Logistic_hw\\insurance_v.csv')

variable = []
data_type = []


for element in list(training_df):
    col = training_df[str(element)]
    
    #Creating List of unique values for each column. 
    unique_vals =training_df[element].unique()

    #This snippet of code will fail if a string is passed through, which will help tell us if the column is only character data
    try:
        unique_vals = unique_vals[np.logical_not(np.isnan(unique_vals))]
        
        #If there is only two unique values in the array, then the data is binary
        if len(unique_vals)==2:
            training_df[str(element)].astype('bool')
            print(str(element) + ' is binary,')
            data_type.append('binary')
            variable.append(element)
        else:
            print(str(element) + ' is contiuous.')
    #if the try statement fails, then the data is nominal.
    except:
        print(str(element)+' is nominal,')



odds_ratios = []
vars_list = []
chi_statistic = []
pvals =[]
for var in training_df.columns:
    cross_tab = pd.crosstab(training_df[str(var)],training_df['INS'])
    if len(cross_tab)==2:
        inv_1 = cross_tab[1][0]
        inv_0 = cross_tab[0][0]
        var_0 = cross_tab[0][1]
        var_1 = cross_tab[1][1]
        odds_0 = inv_1/inv_0
        odds_1 = var_1/var_0
        ratio = odds_1/odds_0
        odds_ratios.append(ratio)
        vars_list.append(var)
        
        ct11= cross_tab[0][0]
        ct12 = cross_tab[1][0]
        ct21 = cross_tab[0][1]
        ct22 = cross_tab[1][1]
        row1_tot = ct11+ct12
        row2_tot = ct21+ct22
        col1_tot = ct11+ct21
        col2_tot = ct12+ct22
        n = row1_tot+row2_tot
        obs = [ct11,ct12,ct21,ct22]
    
    
    
        exp11 = (row1_tot*col1_tot)/n
        exp12 = (row1_tot*col2_tot)/n
        exp21 = (row2_tot*col1_tot)/n
        exp22 = (row2_tot*col2_tot)/n
        exp = [exp11,exp12,exp21,exp22]
    
        stats = scipy.stats.chisquare(obs,f_exp = exp)
        chi = stats[0]
        p = stats[1]
        chi_statistic.append(chi)
        pvals.append(p)
        
    else:
        if len(training_df[str(var)].unique())< 10:
            print(str(var) +' has ' +str(len(training_df[str(var)].unique())) +'   categories')
        continue

odds_dict = {'variable': vars_list,'chisq': chi_statistic,'pvalue': pvals,'Odds Ratio': odds_ratios}
odds_df = pd.DataFrame(odds_dict)

odds_df.to_csv('C:\\Users\\ryanm\\OneDrive\\Documents\\Logistic_hw\\odds-chisq-binaryvars.csv')



    

    



