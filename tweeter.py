#!/usr/bin/env python3
from ebook.markov import MarkovGenerator, twitter_tokenize
import os
from random import random
import re
import time
from twython import Twython, TwythonError
from wordfilter.lib.wordfilter import Wordfilter

API_KEY = os.getenv('API_KEY')
API_SECRET = os.getenv('API_SECRET')
OAUTH_KEY = os.getenv('OAUTH_KEY')
OAUTH_SECRET = os.getenv('OAUTH_SECRET')

twitter = Twython(API_KEY, API_SECRET, OAUTH_KEY, OAUTH_SECRET)

# get rid of annoying trailing ands.
andRe = re.compile(r'\s(and|the).$', re.I)

generator = None
wordfilter = Wordfilter()

min_interval = 300
average_interval = 7200

def loadModel():
  with open('data/cleanCorpus.txt') as f:
    tweetCorpus = f.read()
    generator = MarkovGenerator(tweetCorpus, 0, tokenize_fun=twitter_tokenize)
  return mc

def publishTweet():
  # TODO: this works but i should use a real distribution (?)
  generator.length = min(25 + round(abs(random.gauss(0, 75))), 130)
  tweet = generator.generate_words()

  # exclude undesired tweets
  if tweet.find('@') != -1:
    return None
  if wordfilter.blacklisted(tweet):
    return None

  # clean up random markov junk
  tweet = andRe.sub('.', tweet)

  #tweet and log
  twitter.update_status(tweet)

  # return tweet
  return tweet

if __name__ == '__main__':
  loadModel()
  if generator is None:
    return

  while True:
    tweet = publishTweet()

    # immediately generate a new tweet if we failed
    if tweet is None:
      continue

    # log and delay
    print(time.strftime('[%y-%m-%dT%H:%M:%S] {}').format(tweet))
    time.sleep(min_interval - log(random())) * (average_interval - min_interval)
