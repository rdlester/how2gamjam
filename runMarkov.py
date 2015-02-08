#!/usr/bin/env python3
from markov import MarkovGenerator, twitter_tokenize

if __name__ == "__main__":
  with open('data/cleanCorpus-noHashtags.txt', mode='r') as fin:
    with open('data/pyGenTweets.txt', mode='a') as fout:
      tweetCorpus = fin.read()
      mc = MarkovGenerator(tweetCorpus, 90, tokenize_fun=twitter_tokenize)
      for x in range(50):
        fout.write(mc.generate_words())
        fout.write('\n')
