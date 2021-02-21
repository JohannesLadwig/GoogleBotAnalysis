import pandas as pd
import nltk
from nltk.stem import PorterStemmer
import ssl
import string
import json


data = pd.read_csv('Raw_Media_Headers.csv',header=0)



bigrams_path = 'all_partisan_bigrams_500.csv'
bigram_ideology_path = 'all_bigram_ideology.csv'
bigram = pd.read_csv(bigrams_path)
ideology = pd.read_csv(bigram_ideology_path)[['bigram_stem','a_p','b_p']]
bigram = bigram.loc[bigram['source']=='cr']
bigram = bigram.merge(ideology,how='left',left_on='phrase',right_on='bigram_stem')

bigram.drop_duplicates('phrase', inplace=True)
bigram.reset_index(inplace=True, drop=True)

all_bigrams = set(bigram['phrase'])
bigram_corpus = pd.DataFrame(columns=all_bigrams)

stopwords = set(nltk.stem.snowball.stopwords.words('english'))

ps = PorterStemmer()
nr_gram = []
for j in data.index:
    bigram_corpus.loc[j] = 0
    uncleaned_text = data.loc[j]['search_term']
    if not pd.isna(uncleaned_text):
        text = nltk.word_tokenize(uncleaned_text)
    else:
        text = []
    filtered_text_0 = [w.strip(string.punctuation + '—') for w in text if
                       w not in stopwords]
    filtered_text = [ps.stem(word=w.lower()) for w in filtered_text_0 if
                     w != '']
    text_bigrams = [' '.join(filtered_text[i: i + 2]) for i in
                    range(len(filtered_text) - 2)]

    for gram in text_bigrams:
        if gram in all_bigrams:
            bigram_corpus.loc[j][gram] += 1
    nr_gram.append(len(text_bigrams))

data = data[['source','political_orientation']]
data = data.assign(nr_bigram=nr_gram)

complete_data = data.join(bigram_corpus)


complete_data.to_csv('termpartisanship.csv')
bigram.to_csv('bigram_partisanship.csv')

