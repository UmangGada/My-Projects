# -*- coding: utf-8 -*-
"""
Created on Fri Aug 27 16:31:43 2021

@author: umang
Title: TESS Objects of Interest Classification and Prediction using AdaBoostClassifier with RandomForestClassifier

This is the edited file for this project with only the final code retained.
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from astropy.coordinates import SkyCoord
from sklearn.model_selection import train_test_split
from sklearn.metrics import confusion_matrix
from sklearn import metrics
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import AdaBoostClassifier

# To view all the columns' data
pd.set_option("display.max_columns", None)

sns.set(style="white")
sns.set(style="whitegrid", color_codes=True)

# Creating excel file for dataset
# CTOI_df.to_excel("Project_data_CTOI.xlsx", sheet_name = 'Sheet_name_1')

# Collecting data and looking at a single row
url="https://exofop.ipac.caltech.edu/tess/download_toi.php?sort=toi&output=pipe"
TOI_df=pd.read_csv(url, delimiter='|', index_col=1)
urlc="https://exofop.ipac.caltech.edu/tess/download_ctoi.php?sort=ctoi&output=pipe"
CTOI_df = pd.read_csv(urlc, delimiter = '|', index_col=1)
print("Number of TOIs:", len(TOI_df))
print(TOI_df.loc[664.01])
print("Number of CTOIs:", len(CTOI_df))
print(CTOI_df.loc[17361.01])

TOI_df['TFOPWG Disposition'].value_counts(dropna=False)

# Filling empty cells in response variable extrapolating from a related column
TOI_df['TFOPWG Disposition'] = TOI_df['TFOPWG Disposition'].fillna(TOI_df['TESS Disposition'])

# Count per category
sns.countplot(x='TFOPWG Disposition', data=TOI_df, palette = 'hls')
plt.show()

# Renaming columns in CTOI to match their names from TOI
CTOI_df = CTOI_df.rename(columns={"Transit Epoch (BJD)": "Epoch (BJD)", "Transit Epoch (BJD) err": "Epoch (BJD) err", "Period (days) Error": "Period (days) err", "Depth mmag": "Depth (mmag)", "Depth mmag Error": "Depth (mmag) err", "Depth ppm": "Depth (ppm)", "Depth ppm Error": "Depth (ppm) err", "Duration (hrs)": "Duration (hours)", "Duration (hrs) Error": "Duration (hours) err", "Radius (R_Earth)": "Planet Radius (R_Earth)", "Radius (R_Earth) Error": "Planet Radius (R_Earth) err", "Insolation (Earth Flux)": "Planet Insolation (Earth Flux)", "Equilibrium Temp (K)": "Planet Equil Temp (K)"})

# reducing the dataset to contain common columns between the 2 datasets
print(TOI_df.columns.intersection(CTOI_df.columns))
reduced_TOI_retained = list(TOI_df.columns.intersection(CTOI_df.columns))
reduced_TOI_df = TOI_df[reduced_TOI_retained]
reduced_CTOI_df = CTOI_df[reduced_TOI_retained]

# dropping highly correlated columns and columns with subjective info
reduced_TOI_df = reduced_TOI_df.drop(columns = ['SG1A', 'SG1B', 'SG2', 'SG3', 'SG4', 'SG5', 'Stellar log(g) (cm/s^2)', 'Depth (mmag)', 'Depth (mmag) err', 'Depth (ppm) err', 'Stellar Distance (pc) err', 'Planet Equil Temp (K)'])
reduced_CTOI_df = reduced_CTOI_df.drop(columns = ['SG1A', 'SG1B', 'SG2', 'SG3', 'SG4', 'SG5', 'Stellar log(g) (cm/s^2)','Depth (mmag)', 'Depth (mmag) err','Depth (ppm) err', 'Stellar Distance (pc) err', 'Planet Equil Temp (K)'])


# Generating correlation plots
TOI_df_corr = reduced_TOI_df.corr()
CTOI_df_corr = reduced_CTOI_df.corr()

# Visualizing correlation plot as a heatmap
mask = np.zeros_like(TOI_df_corr)
mask[np.triu_indices_from(mask)] = True
with sns.axes_style("white"):
    f, ax = plt.subplots(figsize=(20, 20))
    ax = sns.heatmap(TOI_df_corr, annot=True, fmt='.2f', mask=mask, vmax=1, square=True)
plt.show()
mask2 = np.zeros_like(CTOI_df_corr)
mask2[np.triu_indices_from(mask2)] = True
with sns.axes_style("white"):
    f, ax = plt.subplots(figsize=(20, 20))
    try:
        ax = sns.heatmap(CTOI_df_corr, annot=True, fmt='.2f', mask=mask2, vmax=1, square=True)
    except ValueError:
        pass
plt.show()

# Converting celestial co-ordinates in usable format
RA_DEC = SkyCoord(reduced_TOI_df['RA'], reduced_TOI_df['Dec'], unit='hourangle, deg')
reduced_TOI_df['RA'] = pd.DataFrame(RA_DEC.ra).to_numpy()
reduced_TOI_df['Dec'] = pd.DataFrame(RA_DEC.dec).to_numpy()

#print(TOI_df.groupby('TFOPWG Disposition').mean())

# Checking null values in numerical columns in TOI and imputing them using group mean
# Note: The group means are pretty close to each other when compared to their variance, an overall mean would probably do the same job
reduced_TOI_df.isnull().sum()
reduced_TOI_df.fillna(reduced_TOI_df.groupby('TFOPWG Disposition').transform('mean'), inplace=True)
Final_TOI_df = reduced_TOI_df[reduced_TOI_df['TFOPWG Disposition'].notna()]
Final_TOI_df = Final_TOI_df.drop(['TIC ID'], axis=1)

# Imputing cells for CTOI and creating a final dataframe with only numerical variables
reduced_CTOI_df.isnull().sum()
reduced_CTOI_df.fillna(reduced_CTOI_df.mean(), inplace=True)
Final_CTOI_df = reduced_CTOI_df.drop(['TIC ID', 'TFOPWG Disposition'], axis=1)

# Creating training and test set. Here test set acts as a validation set for CTOI.    
x_train, x_test, y_train, y_test = train_test_split(Final_TOI_df.loc[:, Final_TOI_df.columns != 'TFOPWG Disposition'], Final_TOI_df['TFOPWG Disposition'], test_size=0.25)


# Final Model: Adaboost
# Result: This gives us a decent result of 66% weighted avg F-1 score.
# I personally would want a better result and so I cannot fully trust the predictions of this model.   

base = RandomForestClassifier(max_depth=5, n_estimators=10, max_features=1)
adaclf = AdaBoostClassifier(base_estimator=base)
adaclf.fit(x_train,y_train)
preds = adaclf.predict(x_test)
confusion_matrix(y_test,preds)
print('F1 Score:', metrics.f1_score(y_test,preds, average='weighted'))
class_report=metrics.classification_report(y_test, preds)
print(class_report)

# Predictions for CTOI

preds_CTOI = adaclf.predict(Final_CTOI_df)
preddf = pd.DataFrame(preds_CTOI, columns = ['output'])

# Creating an excel file for predictions
preddf.to_excel("Final_CTOI_df_predictions.xlsx", sheet_name = 'Sheet_name_1')
