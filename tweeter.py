#!/usr/bin/env python3
from ebook.markov import MarkovGenerator, twitter_tokenize
from math import log
import os
from random import gauss, random
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

min_interval = 300
average_interval = 10800

def loadModel():
  global generator
  with open('data/cleanCorpus.txt') as f:
    tweetCorpusString = f.read()
    tweetCorpus = tweetCorpusString.split('\n')
    generator = MarkovGenerator(tweetCorpus, 0, tokenize_fun=twitter_tokenize)
    print("model loaded")

def generateTweet():
  # TODO: this works but i should use a real distribution (?)
  generator.length = 130 # min(20 + round(abs(gauss(0, 75))), 130)
  tweet = generator.generate_words()
  print(tweet)

  # exclude undesired tweets
  if tweet.find('@') != -1:
    return None
  if wordfilter.blacklisted(tweet):
    return None

  # clean up random markov junk
  tweet = andRe.sub('.', tweet)

  # return tweet
  return tweet

if __name__ == '__main__':
  loadModel()
  if generator is None:
    sys.exit("failure loading corpus")

  while True:
    tweet = generateTweet()

    # immediately generate a new tweet if we failed
    if tweet is None:
      continue

    # tweet, log and delay
    twitter.update_status(status=tweet)
    print(time.strftime('[%y-%m-%dT%H:%M:%S] {}').format(tweet))
    time.sleep(min_interval - log(random()) * (average_interval - min_interval))
