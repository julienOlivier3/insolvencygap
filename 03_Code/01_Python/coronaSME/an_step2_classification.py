# -*- coding: utf-8 -*-
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

# +
import pandas as pd
import numpy as np
import re
import pickle
import string
import operator

import nltk
nltk.data.path.append(r'Q:\Meine Bibliotheken\Research\04_Data\03_NLTK')

from sklearn.ensemble import GradientBoostingClassifier
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.base import BaseEstimator, ClassifierMixin


from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
from tqdm import tqdm


# + [markdown] heading_collapsed=true
# # Playground 

# + hidden=true
def text_standardization(input_data):
        '''
        lowercase, delete html tags, delte whitespaces, delete numbers, delete punctuation
    
        '''
    
        # lowercasing
        clean_text = input_data.lower()
        # delete html tags
        clean_text = re.sub(r'\[->.*?<-\]', ' ', clean_text)
        # delete leading and trailing whitespaces
        clean_text = clean_text.strip()
        clean_text = re.sub(r' +', ' ', clean_text)
        # delete numbers not part of a word
        clean_text = re.sub(r'\b\d+\b', '', clean_text)
        # delete punctuation
        clean_text = re.sub("[%s]" % re.escape(string.punctuation), "", clean_text)
        return clean_text


# + hidden=true
class OrdinalClassifier(BaseEstimator, ClassifierMixin):
    
    def __init__(self, learning_rate=0.1, n_estimators=100, min_samples_split=2, min_samples_leaf=1, max_depth=3, sample_weights=None):
        self.learning_rate = learning_rate
        self.n_estimators = n_estimators
        self.min_samples_split = min_samples_split
        self.min_samples_leaf = min_samples_leaf
        self.max_depth = max_depth
        self.sample_weights = sample_weights
        self.clfs = {}
        
        # fitted parameters, initialized to None
        #self.params_ = None
  
        
            
    def fit(self, X, y):
        self.unique_class = np.sort(np.unique(y))
        if self.unique_class.shape[0] > 2:
            for i in range(self.unique_class.shape[0]-1):
                # for each k - 1 ordinal value we fit a binary classification problem
                binary_y = (y > self.unique_class[i]).astype(np.uint8)
                clf = GradientBoostingClassifier(learning_rate=self.learning_rate, 
                                                 n_estimators=self.n_estimators,
                                                 min_samples_split = self.min_samples_split,
                                                 min_samples_leaf = self.min_samples_leaf,
                                                 max_depth = self.max_depth,
                                                 random_state = 333)
                clf.fit(X, binary_y, sample_weight=self.sample_weights)
                self.clfs[i] = clf
    
    def predict_proba(self, X):
        clfs_predict = {k:self.clfs[k].predict_proba(X) for k in self.clfs}
        predicted = []
        for i,y in enumerate(self.unique_class):
            if i == 0:
                # V1 = 1 - Pr(y > V1)
                predicted.append(1 - clfs_predict[y][:,1])
            elif y in clfs_predict:
                # Vi = Pr(y > Vi-1) - Pr(y > Vi)
                 predicted.append(clfs_predict[y-1][:,1] - clfs_predict[y][:,1])
            else:
                # Vk = Pr(y > Vk-1)
                predicted.append(clfs_predict[y-1][:,1])
        return np.vstack(predicted).T
    
    def predict(self, X):
        return np.argmax(self.predict_proba(X), axis=1)
    
    def get_params(self, deep=True):
    # suppose this estimator has parameters 'learning_rate', ...
        return {"learning_rate": self.learning_rate, 
                "n_estimators": self.n_estimators, 
                "min_samples_split": self.min_samples_split,
                "min_samples_leaf": self.min_samples_leaf,
                "max_depth": self.max_depth,
                "sample_weights": self.sample_weights}

    def set_params(self, **parameters):
        for parameter, value in parameters.items():
            setattr(self, parameter, value)
        return self


# + hidden=true
def greta(X_text, proba=False):

    # Vectorization
    vectorizer = pd.read_pickle(r"Q:\Meine Bibliotheken\Research\Green_startups\05_Model\vectorizer.pkl")
        
    # Model
    classifier = pd.read_pickle(r"Q:\Meine Bibliotheken\Research\Green_startups\05_Model\model.pkl")
    
    # Classifications
    X_vec = vectorizer.transform(X_text)
    y_pred = classifier.predict(X_vec)
    y_pred_proba = classifier.predict_proba(X_vec)

    return y_pred, y_pred_proba


# + hidden=true
def greta_simplified(X_text, proba=False):

    # Vectorization
    vectorizer = pd.read_pickle(r"Q:\Meine Bibliotheken\Research\Green_startups\05_Model\vectorizer_simplified.pkl")
        
    # Model
    classifier = pd.read_pickle(r"Q:\Meine Bibliotheken\Research\Green_startups\05_Model\model_simplified.pkl")
    
    # Classifications
    X_vec = vectorizer.transform(X_text)
    y_pred = classifier.predict(X_vec)
    y_pred_proba = classifier.predict_proba(X_vec)

    return y_pred, y_pred_proba


# + hidden=true
greta(['Grüne Technologien sind ein entscheidender Treiber für eine Dekarbonisierung unseres Wirtschaftens.'])

# + hidden=true
greta_simplified(['Grüne Technologien sind ein entscheidender Treiber für eine Dekarbonisierung unseres Wirtschaftens.'])

# + hidden=true
greta(['Grüne Technologien wie Wasserstoff, Photovoltaikanlagen aber auch Smart-Metering sind eine wichtige Komponente unseres Geschäftsmodells'])

# + hidden=true
greta_simplified(['Grüne Technologien wie Wasserstoff, Photovoltaikanlagen aber auch Smart-Metering sind eine wichtige Komponente unseres Geschäftsmodells'])

# + hidden=true
greta(['Wir bauen Braunkohle ab und benötigen viel Energie in unserem Produktionsprozess. Energieintensive Müllverbrennung zeichnet uns aus.'])
# -

# # Setup Database

engine = create_engine('sqlite:///MUPcrawling.db', echo=False)  #create DB engine


def getWebsiteTexts(crefo):
    with engine.connect() as con: 
        rs = con.execute(f'SELECT text_clean FROM ARGUS WHERE ID={crefo}') 
        df = pd.DataFrame(rs.fetchall())
        df.columns = rs.keys()
    return df


