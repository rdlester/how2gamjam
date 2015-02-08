#!/usr/bin/env python3
import os
import re
import sys

corpusFileName = 'data/pastCorpus.txt'
cleanedCorpusName = 'data/cleanCorpus-noHashtags.txt'

# remove weirdly formatted manual RTs, note those kleene stars, no +
rtRe = re.compile('^(RT\s*)?\w*:')

# who is tweeting this junk
dstRe = re.compile(r'(\#DaylightSavingsTime)|(\#DaylightSavingTime)')

# drop hashtags that occur at end of line
# Q: drop these entirely? clear sign of obnoxious #relevant game tweets
startHashtagRe = re.compile(r'^(\#\w+\s+){2,}')
endHashtagRe = re.compile(r'(\s*[\-\–]\s*)?(\#\w+\s*)+$')

def cleanCorpus():
  with open(corpusFileName, mode='r') as fraw:
    with open(cleanedCorpusName, mode='w') as fclean:
      for line in fraw:
        line = cleanLine(line)
        if line is None or line is '':
          continue
        fclean.write(line)
        fclean.write('\n')  # does line already contain \n?

# returns cleaned line or None if line should be excluded.
def cleanLine(line):
  # exclude offenders
  if reCollide(rtRe, line) or reCollide(dstRe, line):
    return None

  #remove junk
  line = startHashtagRe.sub('', line)
  line = endHashtagRe.sub('', line)

  # fix formatting
  line = line.replace('\n', '')
  line = line.replace ('...', '')
  line = line.strip()

  return line

# returns True is `re` is matched in `line`
def reCollide(re, line):
  return (re.search(line) is not None)

if __name__ == '__main__':
  cleanCorpus()
