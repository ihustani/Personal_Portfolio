# -*- coding: utf-8 -*-
"""
Created on Fri May  7 10:52:02 2021

@author: bfetes
"""

import nltk
import re
import csv
from csv import DictReader
import pandas as pd
import string

#### add new file path
nltk.data.path.append("H://Desktop/Working Directory/Data Science/Natural Language Processing/nltk")

#### Load Data ####
babynames = pd.read_csv('H:/Desktop/Working Directory/Data Science/Natural Language Processing/Group Project/Data/Baby Names.csv')
babynames = babynames['name']
babynames = babynames.tolist()

filteredMovies = pd.read_csv('H:/Desktop/Working Directory/Data Science/Natural Language Processing/Group Project/Data/Filtered_movies_plots.csv')

Movies1980 = filteredMovies[filteredMovies['Release Year'] >= 1980]

##### Cleaning ####

cleanMovies = Movies1980.filter(items = ['Release Year', 'Title', 'Plot'])

stopwords = nltk.corpus.stopwords.words('english')
wn = nltk.WordNetLemmatizer()
ps = nltk.PorterStemmer()

def lemmatizer(tokenized_text):
    text = [wn.lemmatize(word) for word in tokenized_text]
    return(text)

cleanMovies['plot_lower'] = cleanMovies['Plot'].apply(lambda x: x.lower())
cleanMovies['alpha_only'] = cleanMovies['plot_lower'].replace('[^a-zA-Z]', ' ', regex=True)
cleanMovies['tokens'] = cleanMovies['alpha_only'].apply(lambda x: nltk.word_tokenize(x))
cleanMovies['no_stops'] = cleanMovies['tokens'].apply(lambda x: [word for word in x if word not in stopwords])
cleanMovies['no_names'] = cleanMovies['no_stops'].apply(lambda x: [word for word in x if word not in babynames])
cleanMovies['plot_clean'] = cleanMovies['no_names'].apply(lambda x: lemmatizer(x))

#### Find Most Common Words ####

allWords = []

for wordList in cleanMovies['plot_clean']:
    allWords += wordList
    
word_list = nltk.FreqDist(allWords)
len(word_list)

len(word_list)*.02

most_common_words = word_list.most_common(1200)

most_common_filter = [word for (word, count) in most_common_words]

#### Data for K Means ####

def pre_process(text):
    text = "".join([word.lower() for word in text if word not in string.punctuation])
    tokens = nltk.word_tokenize(text)
    lemma_stops = [wn.lemmatize(word) for word in tokens if word not in stopwords]
    #lemma_stops = [ps.stem(word) for word in tokens if word not in stopwords]
    remove_names = [word for word in lemma_stops if word not in babynames]
    common_words = [word for word in remove_names if word in most_common_filter]
    return(common_words)

from sklearn.feature_extraction.text import CountVectorizer
vectorizer = CountVectorizer(analyzer = pre_process)

sparseMovies = vectorizer.fit_transform(Movies1980['Plot'])

vectorizedMovies = pd.DataFrame(sparseMovies.toarray(), columns = vectorizer.get_feature_names())
vectorizedMovies

from sklearn.cluster import KMeans

true_k = 100

km = KMeans(n_clusters=true_k)
modelMovies = km.fit(sparseMovies)

titles = Movies1980['Title'].tolist()
clusters = modelMovies.labels_.tolist()

Movies = {'title' : titles, 'cluster' : clusters}

MovieClusters = pd.DataFrame(Movies, index = [clusters])

MovieClusters['cluster'].value_counts()

print("Top terms per cluster:")
order_centroids = modelMovies.cluster_centers_.argsort()[:, ::-1]
terms = vectorizer.get_feature_names()
for i in range(true_k):
    print("Cluster %d:" % i),
    for ind in order_centroids[i, :10]:
        print(' %s' % terms[ind])
    print

