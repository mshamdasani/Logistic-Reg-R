import pandas as pd
import numpy as np
import math
import path


#Creating dataframes for both validation and training set using pandas.read_csv method.
training_df = pd.read_csv('C:\\Users\\ryanm\\OneDrive\\Documents\\Logistic_hw\\insurance_t.csv')
valid_df = pd.read_csv('C:\\Users\\ryanm\\OneDrive\\Documents\\Logistic_hw\\insurance_t.csv')

#Creating New Dataframe of only continous variables
floats_df = training_df.select_dtypes(include = 'float')

#Casting target variable to binary variable
training_df.astype({'INV': 'bool'}).dtypes

#Snippet of code that creates list of unique values for the column INV, and then removes any nans
#This was a test before creating a loop 
vals = np.array(training_df.INV.unique())
vals = vals[np.logical_not(np.isnan(vals))]


#Creating for loop that goes through all variables and determines the data type

#initialzing lists 
variable = []
data_type = []
test = []

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
        else:
            print(str(element) + ' is contiuous.')
            data_type.append('continuous')
    #if the try statement fails, then the data is nominal.
    except:
        print(str(element)+' is nominal,')
        data_type.append('nominal')
    variable.append(element)
    test.append('')
    
#creating dictionary of variables, and associated data types
var_dict = {'variable': variable,'data_type': data_type,'test':test}
    
variable_types = pd.DataFrame(var_dict)
variable_types.to_csv('C:\\Users\\ryanm\\OneDrive\\Documents\\Logistic_hw\\variable_types.csv')


training_df.RES.value_counts()
