#!/usr/bin/env python3
from ebook.markov import MarkovGenerator, twitter_tokenize
import random
import re

# get rid of annoying trailing ands.
andRe = re.compile(r'\s(and|the).$', re.I)

if __name__ == "__main__":
  with open('data/cleanCorpus.txt', mode='r') as fin:
    with open('data/markov-gamedev.txt', mode='a') as fout:
      tweetCorpus = fin.read()
      mc = MarkovGenerator(tweetCorpus, 90, tokenize_fun=twitter_tokenize)
      lengths = []
      for x in range(50):
        mc.length = min(25 + round(abs(random.gauss(0, 75))), 130)
        print(mc.length)
        lengths.append(mc.length)
        tweet = mc.generate_words()

        # exclude @ replies!
        if tweet.find('@') != -1:
          continue

        tweet = andRe.sub('.', tweet)

        fout.write(tweet)
        fout.write('\n')

      print('Average')
      print(sum(lengths) / len(lengths))
