#!/usr/bin/env python3
import os
import re
import sys

corpusFileName = 'pastCorpus.txt'
cleanedCorpusName = 'cleanCorpus.txt'

# remove weirdly formatted manual RTs, note those kleene stars, no +
RTre = re.compile('^(RT\s*)?\w*:')

# drop hashtags that occur at end of line
# Q: drop these entirely? clear sign of obnoxious #relevant game tweets
hashtagre = re.compile(r'(\s*[\-\–]\s*)?(\#\w+\s+)+$')

def cleanCorpus():
  with open(corpusFileName, mode='r') as fraw:
    with open(cleanedCorpusName, mode='w') as fclean:
      for line in fraw:
        line = cleanLine(line)
        if line is None or line is '':
          continue
        fclean.write(line)
        fclean.write('\n')  # does line already contain \n?

def cleanLine(line):
  if RTre.match(line) is not None:
    return None
  line = hashtagre.sub('', line)
  line = line.replace('\n', '')
  line = line.strip()
  return line

if __name__ == '__main__':
  cleanCorpus()
