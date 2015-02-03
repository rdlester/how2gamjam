#!/usr/bin/env python3
import os
import re
import sys
import time
from twython import Twython, TwythonError

API_KEY = os.getenv('API_KEY')
API_SECRET = os.getenv('API_SECRET')
OAUTH_KEY = os.getenv('OAUTH_KEY')
OAUTH_SECRET = os.getenv('OAUTH_SECRET')

twitter = Twython(API_KEY, API_SECRET, OAUTH_KEY, OAUTH_SECRET)

urlRE = re.compile(r'https?:\/\/[^\s\r\n]*')
RTRE = r'^RT @[^:]+:'

# rate limit is 180, get as close to that as possible
queryDelay = 6

saveFileName = 'pastCorpus.txt'

# scrapes upwards or downwards, excluding tweets.
def getCorpus(max_id, since_id):
  try:
    noMax = max_id < 0
    noSince = since_id < 0
    if noMax and noSince:
      result = twitter.search(q='#gamedev',
          result_type='recent',
          count=100)
    elif noMax:
      result = twitter.search(q='#gamedev',
          result_type='recent',
          count=100,
          since_id=since_id)
    elif noSince:
      result = twitter.search(q='#gamedev',
          result_type='recent',
          count=100,
          max_id=max_id)
    else:
      return (-1,-1)
    first = sys.maxsize
    last = -sys.maxsize - 1
    with open(saveFileName, mode='a') as f:
      for status in result['statuses']:
        # trim, remove internal newlines, remove links.
        text = filterStatusText(status['text'])
        if text is not None:
          f.write(text)
          f.write('\n')
          statusId = status['id']
          if statusId > last:
            last = statusId
          if statusId < first:
            first = statusId
      print('Search saved.')
    return (first, last)
  except TwythonError as e:
    print(e)
    return (-1, -1)

def filterStatusText(text):
  # exclude retweets
  if re.match(RTRE, text) is not None:
    return None
  text = text.strip()
  text = text.replace ('\r\n', ' ')
  text = text.replace('\r', ' ')
  text = text.replace('\n', ' ')
  text = text.replace('…', '')
  text = re.sub(urlRE, '', text)
  return text


# search backwards
if __name__ == '__main__':
  (first, last) = getCorpus(-1, -1)
  time.sleep(queryDelay)
  while True:
    (newFirst, _) = getCorpus(first, -1)
    # (_, newLast) = getCorpus(-1, last)
    if newFirst > 0 and newFirst < first:
      first = newFirst
    # if newLast > 0 and newLast > last:
      # last = newLast
    time.sleep(queryDelay)
