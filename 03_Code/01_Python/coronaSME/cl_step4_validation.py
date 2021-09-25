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


# -

# # Define Functions 

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


def greta_add(X_text, proba=False):

    # Vectorization
    vectorizer = pd.read_pickle(r"Q:\Meine Bibliotheken\Research\Green_startups\05_Model\vectorizer_add.pkl")
        
    # Model
    classifier = pd.read_pickle(r"Q:\Meine Bibliotheken\Research\Green_startups\05_Model\model_add.pkl")
    
    # Classifications
    X_vec = vectorizer.transform(X_text)
    y_pred = classifier.predict(X_vec)
    y_pred_proba = classifier.predict_proba(X_vec)

    return y_pred, y_pred_proba


def greta_tf(X_text, proba=False):

    # Vectorization
    vectorizer = pd.read_pickle(r"Q:\Meine Bibliotheken\Research\Green_startups\05_Model\vectorizer_tf.pkl")
        
    # Model
    classifier = pd.read_pickle(r"Q:\Meine Bibliotheken\Research\Green_startups\05_Model\model_tf.pkl")
    
    # Classifications
    X_vec = vectorizer.transform(X_text)
    y_pred = classifier.predict(X_vec)
    y_pred_proba = classifier.predict_proba(X_vec)

    return y_pred, y_pred_proba


# +
# Alternative target 1
def map_green(x1, x2, x3):
    pr_gr0 = x2+x3
    pr_0 = x1
    
    if pr_gr0 > pr_0:
        return 1
    else:
        return 0

map_green = np.vectorize(map_green)

# + [markdown] heading_collapsed=true
# # Start-ups I

# + [markdown] hidden=true
# EIT Climate-KIC Accelerator

# + [markdown] hidden=true
# ## Webdata 

# + [markdown] hidden=true
# ### German and English

# + hidden=true
# Read scraped webtexts
df_web = []

for i in range(1, 12):
    
    df_web.append(pd.read_csv(r"Q:\Meine Bibliotheken\Research\Green_startups\05_Model\02_Validation\01_StartUps\01_Scraping\01_CurrentPortfolio\ARGUS_chunk_p" + str(i) + ".csv"
                 ,sep="\t"
                 ,encoding="utf-8"))


# + hidden=true
# Concatenate list elements
df_web = pd.concat(df_web)

# + hidden=true
# Distribution of scraped webpages
df_web.ID.value_counts().sort_index()

# + hidden=true
df_web.shape

# + hidden=true
# Drop rows with NaN in text column
df_web = df_web.loc[df_web.text.notnull(),:]

# + hidden=true
df_web.shape

# + hidden=true
# Change dtype of ID column
df_web.loc[:,'ID'] = df_web.ID.astype(int)

# + hidden=true
# Aggregate webdata by crefo
df_web = df_web.groupby('ID').agg(
    dict(
        text = lambda x: ' '.join(x),
        links = lambda x: list(x)
        )
)

# + hidden=true
df_web.shape

# + hidden=true
df_web.head()

# + hidden=true
X_text = df_web.text

# + hidden=true
y_pred, y_pred_proba = greta_tf(X_text)

# + hidden=true
y_pred_proba

# + hidden=true
df_web.loc[:,'y_pred'] = y_pred
df_web.loc[:,'y_pred_gr0'] = map_green(y_pred_proba[:,0], y_pred_proba[:,1], y_pred_proba[:,2])

# + hidden=true



# + [markdown] heading_collapsed=true hidden=true
# ### German

# + hidden=true
df_web_de_en = pd.read_excel(r"Q:\Meine Bibliotheken\Research\Green_startups\05_Model\02_Validation\01_StartUps\EIT_Climate-KIC_Accelerator_startups.xlsx", 
             sheet_name="DE-EN texts")

# + hidden=true
# Drop start-ups for which scraping did not work
df_web_de_en = df_web_de_en.loc[df_web_de_en.text_de.notnull(),:]

# + hidden=true
X_text = df_web_de_en.text_de

# + hidden=true
y_pred, y_pred_proba = greta(X_text)

# + hidden=true
y_pred_proba

# + hidden=true
df_web.loc[:,'y_pred_de'] = y_pred
df_web.loc[:,'y_pred_gr0_de'] = map_green(y_pred_proba[:,0], y_pred_proba[:,1], y_pred_proba[:,2])

# + [markdown] heading_collapsed=true hidden=true
# ### English

# + hidden=true
X_text = df_web_de_en.text_en

# + hidden=true
y_pred, y_pred_proba = greta(X_text)

# + hidden=true
y_pred_proba

# + hidden=true
df_web.loc[:,'y_pred_en'] = y_pred
df_web.loc[:,'y_pred_gr0_en'] = map_green(y_pred_proba[:,0], y_pred_proba[:,1], y_pred_proba[:,2])

# + hidden=true
df_web

# + hidden=true
df_web.to_excel(r'Q:\Meine Bibliotheken\Research\Green_startups\05_Model\02_Validation\01_StartUps\label_results.xlsx'
                    , index=True)

# + [markdown] heading_collapsed=true hidden=true
# ## Company Descriptions 

# + hidden=true
df_eit = pd.read_excel(r"Q:\Meine Bibliotheken\Research\Green_startups\05_Model\02_Validation\01_StartUps\EIT_Climate-KIC_Accelerator_startups.xlsx")

# + [markdown] heading_collapsed=true hidden=true
# ### German Descriptions

# + hidden=true
X_text = df_eit.C_DESC_de

# + hidden=true
y_pred, y_pred_proba = greta(X_text)

# + hidden=true
y_pred_proba

# + hidden=true
# Append columns with predicted classes
df_eit.loc[:,'y_pred'] = y_pred
# Append columns with identifier for Pr(y>0)
df_eit.loc[:,'y_pred_gr0'] = map_green(y_pred_proba[:,0], y_pred_proba[:,1], y_pred_proba[:,2])

# + [markdown] hidden=true
# ### English Descriptions

# + hidden=true
X_text = df_eit.C_DESC_en

# + hidden=true
y_pred, y_pred_proba = greta(X_text)

# + hidden=true
y_pred_proba

# + hidden=true
df_eit.loc[:,'y_pred_en'] = y_pred
df_eit.loc[:,'y_pred_gr0_en'] = map_green(y_pred_proba[:,0], y_pred_proba[:,1], y_pred_proba[:,2])

# + hidden=true
# Look at resulting predictions
df_eit
# They are worse compared to predictions based on webdata. Not surprising since classifier has been trained on webdata

# + [markdown] heading_collapsed=true
# # Start-ups II

# + [markdown] hidden=true
# EIT Climate-KIC Accelerator Alumnis

# + [markdown] heading_collapsed=true hidden=true
# ## Prepare Scraping

# + hidden=true
df_argus = pd.read_excel(r"Q:\Meine Bibliotheken\Research\Green_startups\05_Model\02_Validation\01_StartUps\EIT_Climate-KIC_Accelerator_startups_alumni.xlsx",
             sheet_name='Detailed')

# + hidden=true
# Keep relevant variables only
df_argus = df_argus.set_index('ID').loc[:,['WEBSITE']]

# + hidden=true
df_argus.WEBSITE = df_argus.WEBSITE.astype(str)

# + hidden=true
# Clean urls
df_argus = df_argus.WEBSITE.apply(lambda x: re.sub('^(.+)//', '', x)).apply(lambda x: re.sub('/(.*)$', '', x))
df_argus.loc[df_argus.apply(lambda x: operator.not_(bool(re.search(r'^www.',x))))] = df_argus.loc[df_argus.apply(lambda x: operator.not_(bool(re.search(r'^www.',x))))].apply(lambda x: 'www.' + x)
df_argus

# + hidden=true
df_argus.to_csv(r'Q:\Meine Bibliotheken\Research\Green_startups\05_Model\02_Validation\01_StartUps\01_Scraping\02_Alumni\df_argus.txt', 
                sep='\t', 
                encoding='utf-8')

# + [markdown] hidden=true
# ## Webdata 

# + [markdown] heading_collapsed=true hidden=true
# ### German and English

# + hidden=true
# Read scraped webtexts
df_web = []

for i in range(1, 5):
    
    df_web.append(pd.read_csv(r"Q:\Meine Bibliotheken\Research\Green_startups\05_Model\02_Validation\01_StartUps\01_Scraping\02_Alumni\ARGUS_chunk_p" + str(i) + ".csv"
                 ,sep="\t"
                 ,encoding="utf-8"))


# + hidden=true
# Concatenate list elements
df_web = pd.concat(df_web)

# + hidden=true
# Distribution of scraped webpages
df_web.ID.value_counts().sort_index()

# + hidden=true
df_web.shape

# + hidden=true
# Drop rows with NaN in text column
df_web = df_web.loc[df_web.text.notnull(),:]

# + hidden=true
df_web.shape

# + hidden=true
# Change dtype of ID column
df_web.loc[:,'ID'] = df_web.ID.astype(int)

# + hidden=true
# Aggregate webdata by crefo
df_web = df_web.groupby('ID').agg(
    dict(
        text = lambda x: ' '.join(x),
        links = lambda x: list(x)
        )
)

# + hidden=true
df_web.shape

# + hidden=true
df_web = df_web.loc[df_web.text.apply(lambda x: text_standardization(x)).apply(lambda x: len(nltk.word_tokenize(x))) > 100,:]

# + hidden=true
df_web.shape

# + hidden=true
df_web.head()

# + hidden=true
X_text = df_web.text

# + hidden=true
y_pred, y_pred_proba = greta_tf(X_text)

# + hidden=true
y_pred_proba


# + hidden=true
# Alternative target 1
def map_green(x1, x2, x3):
    pr_gr0 = x2+x3
    pr_0 = x1
    
    if pr_gr0 > pr_0:
        return 1
    else:
        return 0

map_green = np.vectorize(map_green)
map_green(y_pred_proba[:,0], y_pred_proba[:,1], y_pred_proba[:,2])

# + hidden=true
df_web.loc[:,'y_pred'] = y_pred
df_web.loc[:,'y_pred_gr0'] = map_green(y_pred_proba[:,0], y_pred_proba[:,1], y_pred_proba[:,2])

# + hidden=true
df_web

# + hidden=true
df_web.y_pred_gr0.sum()/df_web.shape[0]

# + hidden=true
df_web.to_excel(r'Q:\Meine Bibliotheken\Research\Green_startups\05_Model\02_Validation\01_StartUps\label_results_alumni.xlsx'
                    , index=True)

# + [markdown] heading_collapsed=true
# # Incumbents 

# + [markdown] hidden=true
# Curlie

# + [markdown] hidden=true
# ## Webdata 

# + hidden=true
# Read scraped webtexts
df_web = []

for i in range(1, 8):
    
    df_web.append(pd.read_csv(r"Q:\Meine Bibliotheken\Research\Green_startups\05_Model\02_Validation\02_Incumbents\01_Scraping\ARGUS_chunk_p" + str(i) + ".csv"
                 ,sep="\t"
                 ,encoding="utf-8"))


# + hidden=true
# Concatenate list elements
df_web = pd.concat(df_web)

# + hidden=true
df_web.ID.value_counts()

# + hidden=true
# Distribution of scraped webpages
df_web.ID.value_counts().sort_index()

# + hidden=true
df_web.shape

# + hidden=true
# Drop rows with NaN in text column
df_web = df_web.loc[df_web.text.notnull(),:]

# + hidden=true
df_web.shape

# + hidden=true
# Change dtype of ID column
df_web.loc[:,'ID'] = df_web.ID.astype(int)

# + hidden=true
# Aggregate webdata by crefo
df_web = df_web.groupby('ID').agg(
    dict(
        text = lambda x: ' '.join(x),
        links = lambda x: list(x)
        )
)

# + hidden=true
df_web.shape

# + hidden=true
df_web.head()

# + hidden=true
X_text = df_web.text

# + hidden=true
y_pred, y_pred_proba = greta(X_text)

# + hidden=true
y_pred_proba

# + hidden=true
df_web.loc[:,'y_pred'] = y_pred
df_web.loc[:,'y_pred_gr0'] = map_green(y_pred_proba[:,0], y_pred_proba[:,1], y_pred_proba[:,2])

# + hidden=true
df_web

# + hidden=true
df_web.to_excel(r'Q:\Meine Bibliotheken\Research\Green_startups\05_Model\02_Validation\02_Incumbents\label_results.xlsx'
                    , index=True)

# + [markdown] hidden=true
# ### With Simplified Model 

# + hidden=true
y_pred, y_pred_proba = greta_simplified(X_text)

# + hidden=true
y_pred_proba

# + hidden=true
df_web.loc[:,'y_pred'] = y_pred

# + hidden=true
df_web

# + [markdown] hidden=true
# ### With Samples in Training 

# + hidden=true
y_pred, y_pred_proba = greta_add(X_text)

# + hidden=true
y_pred_proba

# + hidden=true
df_web.loc[:,'y_pred'] = y_pred
df_web.loc[:,'y_pred_gr0'] = map_green(y_pred_proba[:,0], y_pred_proba[:,1], y_pred_proba[:,2])

# + hidden=true
df_web

# + [markdown] heading_collapsed=true
# # Non-green Firms I

# + [markdown] hidden=true
# Curlie

# + [markdown] hidden=true
# ## Webdata 

# + hidden=true
# Read scraped webtexts
df_web = []

for i in range(1, 8):
    
    df_web.append(pd.read_csv(r"Q:\Meine Bibliotheken\Research\Green_startups\05_Model\02_Validation\03_NonGreen\01_Scraping\ARGUS_chunk_p" + str(i) + ".csv"
                 ,sep="\t"
                 ,encoding="utf-8"))


# + hidden=true
# Concatenate list elements
df_web = pd.concat(df_web)

# + hidden=true
df_web.ID.value_counts()

# + hidden=true
# Distribution of scraped webpages
df_web.ID.value_counts().sort_index()

# + hidden=true
df_web.shape

# + hidden=true
# Drop rows with NaN in text column
df_web = df_web.loc[df_web.text.notnull(),:]

# + hidden=true
df_web.shape

# + hidden=true
# Change dtype of ID column
df_web.loc[:,'ID'] = df_web.ID.astype(int)

# + hidden=true
# Aggregate webdata by crefo
df_web = df_web.groupby('ID').agg(
    dict(
        text = lambda x: ' '.join(x),
        links = lambda x: list(x)
        )
)

# + hidden=true
df_web.shape

# + hidden=true
df_web.head()

# + hidden=true
X_text = df_web.text

# + hidden=true
y_pred, y_pred_proba = greta(X_text)

# + hidden=true
y_pred_proba

# + hidden=true
df_web.loc[:,'y_pred'] = y_pred
df_web.loc[:,'y_pred_gr0'] = map_green(y_pred_proba[:,0], y_pred_proba[:,1], y_pred_proba[:,2])

# + hidden=true
df_web

# + hidden=true
df_web.to_excel(r'Q:\Meine Bibliotheken\Research\Green_startups\05_Model\02_Validation\03_NonGreen\label_results.xlsx'
                    , index=True)
# -

# # Non-green Firms II

# Curlie

# ## Prepare Scraping

df_brown = pd.read_excel(r"Q:\Meine Bibliotheken\Research\Green_startups\02_Data\brown_labels\01_Curlie\curlie_energy_intensive_companies.xlsx",
             sheet_name='Python')

df_brown

# Keep relevant variables only
df_argus = df_brown.set_index('ID').loc[:,['URL']]

df_argus.URL = df_argus.URL.astype(str)

# Clean urls
df_argus = df_argus.URL.apply(lambda x: re.sub('^(.+)//', '', x)).apply(lambda x: re.sub('/(.*)$', '', x))
df_argus.loc[df_argus.apply(lambda x: operator.not_(bool(re.search(r'^www.',x))))] = df_argus.loc[df_argus.apply(lambda x: operator.not_(bool(re.search(r'^www.',x))))].apply(lambda x: 'www.' + x)
df_argus

df_argus.to_csv(r'Q:\Meine Bibliotheken\Research\Green_startups\05_Model\02_Validation\03_NonGreen\02_FirmsETS\01_Scraping\df_argus.txt', 
                sep='\t', 
                encoding='utf-8')

# ## Webdata 

# +
# Read scraped webtexts
df_web = []

for i in range(1, 6):
    
    df_web.append(pd.read_csv(r"Q:\Meine Bibliotheken\Research\Green_startups\05_Model\02_Validation\03_NonGreen\02_FirmsETS\01_Scraping\ARGUS_chunk_p" + str(i) + ".csv"
                 ,sep="\t"
                 ,encoding="utf-8"))
# -


# Concatenate list elements
df_web = pd.concat(df_web)

df_web.ID.value_counts()

# Distribution of scraped webpages
df_web.ID.value_counts().sort_index()

df_web.shape

# Drop rows with NaN in text column
df_web = df_web.loc[df_web.text.notnull(),:]

df_web.shape

# Change dtype of ID column
df_web.loc[:,'ID'] = df_web.ID.astype(int)

df_web.ID.value_counts()

# Aggregate webdata by crefo
df_web = df_web.groupby('ID').agg(
    dict(
        text = lambda x: ' '.join(x),
        links = lambda x: list(x)
        )
)

df_web.shape

df_web.head()

X_text = df_web.text

y_pred, y_pred_proba = greta(X_text)

y_pred_proba

df_web.loc[:,'y_pred'] = y_pred
df_web.loc[:,'y_pred_gr0'] = map_green(y_pred_proba[:,0], y_pred_proba[:,1], y_pred_proba[:,2])

df_web

df_web.to_excel(r'Q:\Meine Bibliotheken\Research\Green_startups\05_Model\02_Validation\03_NonGreen\02_FirmsETS\label_results.xlsx'
                    , index=True)


