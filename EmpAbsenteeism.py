

import os
import numpy as np
import pandas as pd


os.chdir("/Users/anchal/Documents/Edwisor/Project_2")

df = pd.read_excel('Absenteeism_at_work_Project.xls')


df.shape

df.head(10)

df.describe()

type(df)


# ## Missing Value Analysis

miss_df = pd.DataFrame(df.isnull().sum())

#Reset index
miss_df = miss_df.reset_index()

#Rename variable
miss_df = miss_df.rename(columns = {'index': 'Variables', 0: 'Missing_percentage'})

#Calculate percentage
miss_df['Missing_percentage'] = (miss_df['Missing_percentage']/len(df))*100

#descending order
miss_df = miss_df.sort_values('Missing_percentage', ascending = False).reset_index(drop = True)

#save output results 
#miss_df.to_csv("Miising_perc.csv", index = False)

miss_df


#replace -1 with NA to impute
for i in range(0, df.shape[1]):
    df.iloc[:,i] = df.iloc[:,i].replace(-1, np.nan) 

#Apply KNN imputation algorithm
from fancyimpute import KNN  
df = pd.DataFrame(KNN(k = 3).complete(df), columns = df.columns)

#Generating correlation matrix
corr = df.corr()

#Plot using seaborn library
import seaborn as sns
sns.heatmap(corr,vmin=-1,vmax=1)


## Correlation Analysis

#Generating correlation matrix
corr = df.corr()

#Plot using seaborn library
import seaborn as sns
sns.heatmap(corr,vmin=-1,vmax=1)


# ## Normalization

#Nomalisation
for i in df.columns:
    print(i)
    df[i] = (df[i] - min(df[i]))/(max(df[i]) - min(df[i]))

df.head()


# ## Decision Tree

from sklearn.cross_validation import train_test_split
from sklearn.tree import DecisionTreeRegressor

set.seed(123)
train, test = train_test_split(df, test_size=0.2)


set.seed(123)
fit_DT = DecisionTreeRegressor(max_depth=6).fit(train.iloc[:,0:20], train.iloc[:,20])

#Apply model on test data
predictions_DT = fit_DT.predict(test.iloc[:,0:20])

def rmse(predictions, targets):
    return np.sqrt(((predictions_DT - test.iloc[:,20]) ** 2).mean())

rmse( predictions_DT, test.iloc[:,20])


## Linear Regression

#Import libraries for LR
import statsmodels.api as sm
from sklearn.cross_validation import train_test_split


#Split train and test
train, test = train_test_split(df, test_size=0.2)
# Train the model using the training sets
set.seed(123)
model = sm.OLS(train.iloc[:,20], train.iloc[:,0:20]).fit()

# Print out the statistics
model.summary()


# make the predictions by the model
predictions_LR = model.predict(test.iloc[:,0:20])

def rmse(predictions, targets):
    return np.sqrt(((predictions_LR - test.iloc[:,20]) ** 2).mean())

rmse( predictions_LR, test.iloc[:,20])

