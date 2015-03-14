#!/usr/bin/env python3
from ebook.markov import MarkovGenerator, twitter_tokenize
from math import log
import os
from random import randint, random
import re
import sys
import time
from twython import Twython, TwythonError
from wordfilter.lib.wordfilter import Wordfilter

API_KEY = os.getenv('API_KEY')
API_SECRET = os.getenv('API_SECRET')
OAUTH_KEY = os.getenv('OAUTH_KEY')
OAUTH_SECRET = os.getenv('OAUTH_SECRET')

twitter = Twython(API_KEY, API_SECRET, OAUTH_KEY, OAUTH_SECRET)

# get rid of annoying trailing ands.
andRe = re.compile(r'\s(and|the|a).$', re.I)

generator = None
wordfilter = Wordfilter()

ngram = 3
min_length = 25
max_length = 130

min_interval = 240
average_interval = 8040

def loadModel():
  global generator
  with open('data/cleanCorpus.txt') as f:
    tweetCorpusString = f.read()
    tweetCorpus = tweetCorpusString.split('\n')
    generator = MarkovGenerator(tweetCorpus, max_length, ngram, tokenize_fun=twitter_tokenize)
    print("model loaded")

def generateTweet():
  generator.length = max_length
  tweet = generator.generate_words()

  # exclude undesired tweets
  if tweet == '.':
    return None
  if tweet.find('@') != -1:
    return None
  if wordfilter.blacklisted(tweet):
    return None

  # clean up random markov junk
  tweet = andRe.sub('.', tweet)

  return tweet

if __name__ == '__main__':
  loadModel()
  if generator is None:
    sys.exit("failure loading corpus")

  while True:
    # keep drawing sentences until this length is surpassed.
    draw_until_length = min_length  # randint(min_length, max_length)

    tweet = ""
    while len(tweet) <= draw_until_length:
      new_sentence = generateTweet()
      if new_sentence is not None:
        tweet += new_sentence + ' '

    # tweet, log and delay
    twitter.update_status(status=tweet)
    print(time.strftime('[%y-%m-%dT%H:%M:%S] {}').format(tweet))
    time.sleep(min_interval - log(random()) * (average_interval - min_interval))
