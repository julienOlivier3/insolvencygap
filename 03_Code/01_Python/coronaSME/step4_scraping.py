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

from scraper_api import ScraperAPIClient
import shutil
import requests
import urllib.request
from lxml import etree
from lxml import html
import pandas as pd
import re
from tqdm import tqdm
from concurrent.futures import ThreadPoolExecutor, as_completed
import pickle
#from bs4 import BeautifulSoup

# Activate scraperapi account
#client = ScraperAPIClient('5ede3c912f093340f548c3dc1cd686c3') # julian-doerr@gmx.de
#client = ScraperAPIClient('65e480c6d3d2e26d05284eeeb717e595') # studiumtuebingen@gmail.com
client = ScraperAPIClient('a3c4ed07efb18309ff530edc603d07fd') # julian.doerr@zew.de

# Get account info
usage = client.account()
usage


# Define landing page
landing = r"https://www.firmenwissen.de/musterfirmenprofil.html?crefoId="

# Define html parser
htmlparser = etree.HTMLParser()


# # Functions

# Function for cleaning html
def clean_html(html):
    res = html.replace("\n", " ").strip()
    return(res)


def insolv_parser(crefo, html_request):
    
    status_code = html_request.status_code
    
    # If status code == 404, then crefo does not exist in database of firmenwissen -> return NA
    if (status_code == 404):
        dict_temp = {crefo: {'response': status_code,
                             'insolv': pd.NA,
                             'exitdat': pd.NA,
                             'insolv_text': pd.NA}}
        
        return(pd.DataFrame.from_dict(dict_temp, orient='index'))
    
    # If status code == 200, then crefo exists in database of firmenwissen -> return information regarding insolvency
    elif (status_code == 200):
        
        # Pass html text from request into tree object
        html_tree = etree.XML(html_request.text, htmlparser)
        
        # Go to tag with ID: Insolvenzverfahren
        insolv_temp = html_tree.xpath(_path = "//*[@id='Insolvenzverfahren']")
        
        if (insolv_temp == []):
            dict_temp = {crefo: {'response': status_code,
                'insolv': 0,
                'exitdat': pd.NA,
                'insolv_text': pd.NA}}
            
            return(pd.DataFrame.from_dict(dict_temp, orient='index'))
                
        else:
            # Go to tag including information in insolvency
            tag_insolv = html_tree.xpath(_path = "//*[@id='Insolvenzverfahren']")[0].getparent()
            # Get all text of that tag
            text_temp = list(tag_insolv.itertext())
            # Extract tags containing dates ...
            text_temp = [i for i in text_temp if re.findall(r"[\d]{1,2}.[\d]{1,2}.[\d]{4}", i)]                        
            # ... and clean it from html clutter
            text_insolv = [clean_html(i) for i in text_temp]
            # Extract tag containing information of date of insolvency filing
            exitdat_temp = [i for i in text_insolv if re.findall(r"[\d]{1,2}.[\d]{1,2}.[\d]{4}.{1,3}Insolvenzbekanntmachung", i)][0]
            # Extarct the date 
            exitdat = re.search(r"[\d]{1,2}.[\d]{1,2}.[\d]{4}", exitdat_temp).group(0)
            
            dict_temp = {crefo: {'response': status_code,
                'insolv': 1,
                'exitdat': exitdat,
                'insolv_text': text_insolv}}
            
            return(pd.DataFrame.from_dict(dict_temp, orient='index'))
    
    # If other status code, then there is some issue with scraping -> return some notification
    else:
        print(str(crefo) + ": " + str(status_code))
        
        dict_temp = {crefo: {'response': status_code,
                             'insolv': pd.NA,
                             'exitdat': pd.NA,
                             'insolv_text': pd.NA}}
            
        return(pd.DataFrame.from_dict(dict_temp, orient='index'))


def insolv_requester(crefo, landing, request_type, timeout):
    # Define path 
    full_path = landing+str(crefo)
    
    if (request_type=='local'): # Read html from local folder
        html_file = open(full_path, 'r', encoding='latin-1')
        html_request = html_file.read()
        status_code = None  
    
    elif (request_type=='url'): # Read html from url with own IP
        html_request = requests.get(full_path, timeout=timeout)
        
    else: # Read html from url with rotating proxy (via ScraperAPI)
        #client = ScraperAPIClient('5ede3c912f093340f548c3dc1cd686c3')
        html_request = client.get(full_path, timeout=timeout)
    
    return(html_request)


def insolv_scraper(crefo, landing=landing, request_type="url", timeout=60):
    html_request = insolv_requester(crefo = crefo, landing = landing, request_type = request_type, timeout = timeout)
    df_temp = insolv_parser(crefo = crefo, html_request = html_request)
    return(df_temp)

print("Finished defining functions!")
# # Data

# Read list of crefos
crefos = pd.read_csv(r"Q:\Meine Bibliotheken\Research\SME_Corona\02_Data\09_Insolvency\02_BadRating\crefos_badrating.txt", sep='\t')
crefos = crefos.crefo.to_list()

# Firms already scraped
#df_insolv = pd.read_csv(r"Q:\Meine Bibliotheken\Research\SME_Corona\02_Data\09_Insolvency\02_BadRating\df_insolv.txt", 
#                        sep = '\t', 
#                        index_col='crefo')
df_insolv = pd.read_pickle(r"Q:\Meine Bibliotheken\Research\SME_Corona\02_Data\09_Insolvency\02_BadRating\df_insolv.pkl")
#df_insolv = pd.read_pickle(r"Q:\Meine Bibliotheken\Research\SME_Corona\02_Data\09_Insolvency\02_Backup\02_BadRating\df_insolv_bu.pkl")

df_insolv.response.value_counts(dropna=False)

len(crefos)

crefos = [i for i in crefos if i not in df_insolv.index]

# +
#crefos = df_insolv.loc[df_insolv.response==429].index.to_list()
# -

len(crefos)


# +
#df_insolv = pd.DataFrame(columns= ['response', 'insolv', 'exitdat', 'insolv_text'])

# + active=""
# for i in tqdm(list_rescrape):
#     
#     crefo = i
#
#     
#     
#     html_request = insolv_requester(crefo = crefo, landing=landing)
#     df_temp = insolv_parser(crefo = crefo, html_request = html_request)
#     df_insolv = df_insolv.append(df_temp)

# + active=""
# for i in tqdm(range(0,900)):
#     
#     crefo = crefos[i]
#
#     html_request = insolv_requester(crefo = crefo, landing=landing, request_type = 'scraperapi')
#     df_temp = insolv_parser(crefo = crefo, html_request = html_request)
#     df_insolv = df_insolv.append(df_temp)
# -

concurrency = 5

CREFOs = crefos

# We can use a with statement to ensure threads are cleaned up promptly
with ThreadPoolExecutor(max_workers = concurrency) as executor:
    # Start the load operations and mark each future with its URL
    future_to_url = {executor.submit(insolv_scraper, crefo, landing, request_type = "scraperapi", timeout=60): crefo for crefo in CREFOs}
    for future in tqdm(as_completed(future_to_url)):
        crefo = future_to_url[future]
        try:
            df_insolv = df_insolv.append(future.result())
        except Exception as exc:
            print('%r generated an exception: %s' % (crefo, exc))
        
        
        if (df_insolv.shape[0] % 100 == 0):
            df_insolv.to_pickle(r"Q:\Meine Bibliotheken\Research\SME_Corona\02_Data\09_Insolvency\02_BadRating\01_Backup\df_insolv_bu.pkl")
            
        else:
            pass
print("finish!")


df_insolv.index.duplicated().sum()

# + active=""
# df_insolv = df_insolv.loc[~(df_insolv.response==429)]
# -

df_insolv

df_insolv.response.value_counts(dropna=False)

df_insolv.insolv.value_counts(dropna=False)

df_insolv.exitdat.value_counts()

df_insolv = df_insolv.loc[~((df_insolv.response == 403) | (df_insolv.response == 429)),:]

df_insolv.to_pickle(r"Q:\Meine Bibliotheken\Research\SME_Corona\02_Data\09_Insolvency\02_BadRating\df_insolv.pkl")

df_insolv.to_csv(r"Q:\Meine Bibliotheken\Research\SME_Corona\02_Data\09_Insolvency\02_BadRating\df_insolv.txt", sep = "\t")

# + [markdown] heading_collapsed=true
# # Working Bench 

# + hidden=true
# Get three samples
raw_html_dead = etree.parse(urllib.request.urlopen("https://www.firmenwissen.de/musterfirmenprofil.html?crefoId=2010001919"),
                            htmlparser)
raw_html_living = etree.parse(urllib.request.urlopen("https://www.firmenwissen.de/musterfirmenprofil.html?crefoId=2010000137"),
                            htmlparser)
#raw_html_dead = etree.parse(urllib.request.urlopen("https://www.firmenwissen.de/musterfirmenprofil.html?crefoId=2010000139"),
#                            htmlparser)

# + hidden=true
f = open("Q:/Meine Bibliotheken/Research/SME_Corona/02_Data/09_Insolvency/01_html/2010001919.txt", "w")
f.write(result)
f.close()

# + hidden=true
import time
import requests
from bs4 import BeautifulSoup
from concurrent.futures import ProcessPoolExecutor, ThreadPoolExecutor, as_completed


# + hidden=true
crefos

# + hidden=true
# We can use a with statement to ensure threads are cleaned up promptly
with ThreadPoolExecutor(max_workers=10) as executor:
    # Start the load operations and mark each future with its URL
    future_to_url = {executor.submit(insolv_scraper, crefo): crefo for crefo in CREFOs}
    for future in as_completed(future_to_url):
        df_insolv = df_insolv.append(future.result())

# + hidden=true
df_insolv

# + hidden=true
CREFOs = crefos[0:10]


def insolv_scraper(crefo):
    html_request = insolv_requester(crefo = crefo, landing=landing, request_type = 'url')
    df_temp = insolv_parser(crefo = crefo, html_request = html_request)
    return(df_temp)


# + hidden=true
df_insolv = pd.DataFrame(columns= ['response', 'insolv', 'exitdat', 'insolv_text'])

# + hidden=true
with ProcessPoolExecutor(max_workers=4) as executor:
    start = time.time()
    futures = [ executor.submit(insolv_scraper, crefo) for crefo in CREFOs ]
    results = []
    for result in as_completed(futures):
        results.append(result)
    end = time.time()
    print("Time Taken: {:.6f}s".format(end-start))
