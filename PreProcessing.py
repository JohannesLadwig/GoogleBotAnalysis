import pandas as pd
import nltk
from nltk.stem import PorterStemmer
import ssl
import string
import json

# try:
#     _create_unverified_https_context = ssl._create_unverified_context
# except AttributeError:
#     pass
# else:
#     ssl._create_default_https_context = _create_unverified_https_context
#
# nltk.download()
stemmer = nltk.stem.snowball.SnowballStemmer('english')
stopwords = set(nltk.stem.snowball.stopwords.words('english'))
data = pd.read_csv('combined.csv')


all_bigrams = []
bigrams_by_row = []

for j in data.index:
    new_bigrams = set()
    uncleaned_text = data.loc[j]['text']
    if not pd.isna(uncleaned_text):
        text = nltk.word_tokenize(
            uncleaned_text.translate({ord(ch): None for ch in '0123456789'}))
    else:
        text = []
    filtered_text_0 = [w.strip(string.punctuation + '—') for w in text if
                       w not in stopwords]
    filtered_text = [stemmer.stem(w.lower()) for w in filtered_text_0 if
                     w != '']
    text_bigrams = [' '.join(filtered_text[i: i + 2]) for i in
                    range(len(filtered_text) - 2)]
    new_bigrams = new_bigrams.union(text_bigrams)
    uncleaned_title = data.loc[j]['title']
    if not pd.isna(uncleaned_title):
        title = nltk.word_tokenize(
            uncleaned_title.translate({ord(ch): None for ch in '0123456789'}))
    else:
        title = []
    filtered_title_0 = [w.strip(string.punctuation + '—') for w in title if
                        w not in stopwords]
    filtered_title = [stemmer.stem(w.lower()) for w in filtered_title_0 if
                      w != '']
    title_bigrams = [' '.join(filtered_title[i: i + 2]) for i in
                     range(len(filtered_title) - 2)]

    combined_bigrams = title_bigrams + text_bigrams
    all_bigrams += combined_bigrams
    bigrams_by_row.append(combined_bigrams)

data = data.assign(bigrams=bigrams_by_row)
all_bigrams = set(all_bigrams)
bigram_corpus = pd.DataFrame(columns=all_bigrams)

for i in data.index:
    result_bigrams = data.loc[i]['bigrams']
    bigram_corpus.loc[i] = 0
    for big in result_bigrams:
        bigram = big.strip()
        if bigram == '':
            continue
        bigram_corpus.loc[i][bigram] = bigram_corpus.loc[i][bigram] + 1

complete_data = data.join(bigram_corpus)


complete_data.to_csv('all_data.csv')


##################
##2.0


bigrams_path = 'all_partisan_bigrams_500.csv'
bigram = pd.read_csv(bigrams_path)
bigram = bigram.loc[bigram['source']=='cr']
bigram.drop_duplicates('phrase', inplace=True)
bigram.reset_index(inplace=True, drop=True)

ps = PorterStemmer()
all_bigrams = set(bigram['phrase'])

bigram_corpus = pd.DataFrame(columns=all_bigrams)

for j in data.index:
    uncleaned_text = data.loc[j]['text']
    if not pd.isna(uncleaned_text):
        text = nltk.word_tokenize(
            uncleaned_text.translate({ord(ch): None for ch in '0123456789'}))
    else:
        text = []
    filtered_text_0 = [w.strip(string.punctuation + '—') for w in text if
                       w not in stopwords]
    filtered_text = [ps.stem(word=w.lower()) for w in filtered_text_0 if
                     w != '']
    text_bigrams = [' '.join(filtered_text[i: i + 2]) for i in
                    range(len(filtered_text) - 2)]

    uncleaned_title = data.loc[j]['title']
    if not pd.isna(uncleaned_title):
        title = nltk.word_tokenize(
            uncleaned_title.translate({ord(ch): None for ch in '0123456789'}))
    else:
        title = []
    filtered_title_0 = [w.strip(string.punctuation + '—') for w in title if
                        w not in stopwords]
    filtered_title = [ps.stem(w.lower()) for w in filtered_title_0 if
                      w != '']
    title_bigrams = [' '.join(filtered_title[i: i + 2]) for i in
                     range(len(filtered_title) - 2)]

    combined_bigrams = title_bigrams + text_bigrams
    bigram_corpus.loc[j] = 0
    for gram in combined_bigrams:
        if gram in all_bigrams:
            bigram_corpus.loc[j][gram] += 1



complete_data = data.join(bigram_corpus)

complete_data.to_csv('Final_Political_Bigram_Corpus.csv')
