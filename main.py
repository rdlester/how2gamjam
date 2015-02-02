#!/usr/bin/env python3
import os
import re
import time
from twython import Twython, TwythonError

API_KEY = os.getenv('API_KEY')
API_SECRET = os.getenv('API_SECRET')
OAUTH_KEY = os.getenv('OAUTH_KEY')
OAUTH_SECRET = os.getenv('OAUTH_SECRET')

twitter = Twython(API_KEY, API_SECRET, OAUTH_KEY, OAUTH_SECRET)

urlRE = re.compile(r'https?:\/\/[^\s\r\n]*')

# wait 15 minutes between search queries to avoid getting duplicates
queryDelay = 15 * 60

def getCorpus():
  try:
    result = twitter.search(q='#gamedev',
        result_type='recent',
        count=100)
    with open('corpus.txt', mode='a') as f:
      for status in result['statuses']:
        # trim, remove internal newlines, remove links.
        text = status['text']
        text = text.strip()
        text = text.replace ('\r\n', ' ')
        text = text.replace('\r', ' ')
        text = text.replace('\n', ' ')
        text = text.replace('…', '')
        text = re.sub(urlRE, '', text)
        f.write(text)
        f.write('\n')
      print('Search saved.')
  except TwythonError as e:
    print(e)
    pass

if __name__ == '__main__':
  while True:
    getCorpus()
    time.sleep(queryDelay)
