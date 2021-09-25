# ---
# jupyter:
#   jupytext:
#     formats: ipynb,py:light
#     text_representation:
#       extension: .py
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.6.0
#   kernelspec:
#     display_name: Python 3
#     language: python
#     name: python3
# ---

import pandas as pd
import numpy as np
from catboost import CatBoostClassifier
from sklearn import metrics
from sklearn.tree import DecisionTreeClassifier
#from sklearn.linear_model import LogisticRegression
#from sklearn.ensemble import RandomForestClassifier
#from sklearn.ensemble import GradientBoostingClassifier, AdaBoostClassifier
from sklearn.model_selection import train_test_split
from sklearn.pipeline import Pipeline
from sklearn.model_selection import GridSearchCV, RandomizedSearchCV
#from sklearn.utils.fixes import loguniform
#from sklearn.base import BaseEstimator, ClassifierMixin
#from sklearn.base import clone
#from sklearn.metrics import fbeta_score, accuracy_score, make_scorer

df_panel = pd.read_csv(r"Q:\Meine Bibliotheken\Research\SME_Corona\02_Data\01_PanelData\df_panel_final.txt", 
                       sep = '\t', 
                       encoding='utf-8', 
                       dtype={'kreis': 'object'})

df_panel = pd.read_csv(r"Q:\Meine Bibliotheken\Research\SME_Corona\02_Data\01_PanelData\df_panel_estimation2.txt", 
                       sep = '\t', 
                       encoding='utf-8', 
                       dtype={'kreis': 'object'})

# Datatypes of variables
df_panel.dtypes

# Seperate data by date of Creditreform reassessment (treat_final == 0 -> prior to 01.04.2020, treat_final == 1 -> after 01.04.2020,)
df_panel = df_panel.drop(labels = ['treat', 'crefo', 'jahr', 'bonitaet', 'q_p_bonitaet'], axis = 1, inplace=False)
df_panel0 = df_panel.loc[df_panel.treat_final==0,:].drop('treat_final', axis=1)
df_panel1 = df_panel.loc[df_panel.treat_final==1,:].drop('treat_final', axis=1)

df_panel0.shape, df_panel1.shape 

df_panel0.insolv.value_counts(), df_panel1.insolv.value_counts()

# +
#df_panel0[['r_p_bonitaet', 'wz', 'kreis', 'refo', 'ctype', 'csize']] = df_panel0[['r_p_bonitaet', 'wz', 'kreis', 'refo', 'ctype', 'csize']].astype('string')
# -

df_panel0.dtypes

# Features and Targets
X0 = df_panel0.drop(labels = ['insolv', 'grddat'], axis=1)
y0 = df_panel0.insolv

# + [markdown] heading_collapsed=true
# # Cost-Sensitive Learning 

# + hidden=true
# Train(-dev)-test split
X_train0, X_test0, y_train0, y_test0 = train_test_split(X0, y0, test_size = 0.2, random_state=333, stratify = y0)

# + hidden=true
y0.value_counts(normalize=False)

# + hidden=true
y_train0.value_counts(normalize=False)

# + hidden=true
y_test0.value_counts(normalize=False)

# + hidden=true
1422767/2247

# + hidden=true
model = CatBoostClassifier(
    random_seed=333,
    iterations=100,
    depth=10,
    learning_rate=0.01,
    loss_function='Logloss',
    verbose=True, 
    scale_pos_weight= 633)

# + hidden=true
df_panel

# + hidden=true
cat_features = np.array(['r_p_bonitaet', 'wz', 'kreis', 'refo', 'ctype', 'csize'])

# + hidden=true
model.fit(X_train0, y_train0, cat_features = cat_features, plot=True)

# + hidden=true
y_train_pred0 = model.predict(X_train0)

# + hidden=true
print(metrics.classification_report(y_train0, y_train_pred0))

# + hidden=true
y_test_pred0 = model.predict(X_test0)

# + hidden=true
print(metrics.classification_report(y_test0, y_test_pred0))

# + hidden=true
model.get_feature_importance(prettified=True)

# + hidden=true
from sklearn.metrics import precision_recall_curve
from sklearn.metrics import auc
import matplotlib.pyplot as plt

# predict probabilities
yhat = model.predict_proba(X_train0)

# retrieve the probabilities for the positive class
yhat_positive = yhat[:, 1]

# calculate the no skill line as the proportion of the positive class
no_skill = len(y0[y0==1]) / len(y0)
# plot the no skill precision-recall curve
plt.plot([0, 1], [no_skill, no_skill], linestyle='--', label='No Skill')

# calculate inputs for the PR curve
precision, recall, thresholds = precision_recall_curve(y_train0, yhat_positive)

# plot PR curve
plt.plot(recall, precision, marker='.', label='CatBoost')
# axis labels
plt.xlabel('Recall')
plt.ylabel('Precision')
# show the legend
plt.legend()
plt.show()

# calculate and print PR AUC
auc_pr = auc(recall, precision)
print('AUC PR: %.3f' % auc_pr)

# + hidden=true
#import the required library
from numpy import argmax

# Calculate F-Scores and find the index of ideal score
fscore = (2 * precision * recall) / (precision + recall)
ix = argmax(fscore)
best_thresh = thresholds[ix]
print('Best Threshold: %f' % (best_thresh))

# + hidden=true
pd.Series({'prob_insolv': yhat_positive}).hist()

# + hidden=true
yhat_positive = list(yhat_positive)

# + hidden=true
yhat_positive

# + hidden=true
y_train_pred0 = []
for i in range(0, len(yhat_positive)):
    if yhat_positive[i] > best_thresh:
        y_train_pred0.append(1)
    else: 
        y_train_pred0.append(0)

# + hidden=true
sum(y_train_pred0)

# + hidden=true
print(metrics.classification_report(y_train0, y_train_pred0))
# -

# # Undersampling 

# ## Random Undersampling 

# Train(-dev)-test split
X_train0, X_test0, y_train0, y_test0 = train_test_split(X0, y0, test_size = 0.2, random_state=333, stratify = y0)

from imblearn.under_sampling import NearMiss, RandomUnderSampler

maj_fac = 4

rus = RandomUnderSampler(sampling_strategy=1/maj_fac, random_state=333, replacement=False)

X_res, y_res = rus.fit_resample(X_train0, y_train0)

y_res.sum(), y_res.shape[0]-y_res.sum()

model = CatBoostClassifier(
    random_seed=333,
    iterations=100,
    depth=10,
    learning_rate=0.01,
    loss_function='Logloss',
    verbose=True, 
    scale_pos_weight= maj_fac)

cat_features = np.array(['r_p_bonitaet', 'wz', 'kreis', 'refo', 'ctype', 'csize'])

model.fit(X_res, y_res, plot=True)

y_train_pred0 = model.predict(X_res)
print(metrics.classification_report(y_res, y_train_pred0))

y_test_pred0 = model.predict(X_test0)
print(metrics.classification_report(y_test0, y_test_pred0))

# ## Near Miss

# Train(-dev)-test split
X_train0, X_test0, y_train0, y_test0 = train_test_split(X0, y0, test_size = 0.2, random_state=333, stratify = y0)

from imblearn.under_sampling import NearMiss, RandomUnderSampler

maj_fac = 2

nm = NearMiss(sampling_strategy='auto')

X_res, y_res = rus.fit_resample(X_train0, y_train0)

y_res.sum(), y_res.shape[0]-y_res.sum()

model = CatBoostClassifier(
    random_seed=333,
    iterations=100,
    depth=10,
    learning_rate=0.01,
    loss_function='Logloss',
    verbose=True, 
    scale_pos_weight= maj_fac)

cat_features = np.array(['r_p_bonitaet', 'wz', 'kreis', 'refo', 'ctype', 'csize'])

model.fit(X_res, y_res, plot=True)

y_train_pred0 = model.predict(X_res)
print(metrics.classification_report(y_res, y_train_pred0))

y_test_pred0 = model.predict(X_test0)
print(metrics.classification_report(y_test0, y_test_pred0))
