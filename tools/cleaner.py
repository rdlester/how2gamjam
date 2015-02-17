#!/usr/bin/env python3
import nltk
import os
import re
import sys

corpusFileName = 'data/corpus.txt'
cleanedCorpusName = 'data/cleanCorpus.txt'

# remove weirdly formatted manual RTs, note those kleene stars, no +
rtRe = re.compile('^(RT\s*)?\w*:', re.I)

# who is tweeting this junk
dstRe = re.compile(r'(\#DaylightSavingsTime)|(\#DaylightSavingTime)', re.I)

# drop hashtags that occur at beginning and end of line
# Q: drop these entirely? clear sign of obnoxious #relevant game tweets
startHashtagRe = re.compile(r'^(\#\w+\s+){2,}')
endHashtagRe = re.compile(r'(\s*[\-\–]\s*)?(\#\w+\s*)+$')

# there is some funky via usage, hence all the \s
viaRe = re.compile(r'(via|by)\s*\@\s*\w+\s*', re.I)
hangingViaRe = re.compile(r'\s*(via|by)\s*$', re.I)
hangingAtRe = re.compile(r'\s*\@\s*\w+\s*$')

def cleanCorpus():
  with open(corpusFileName, mode='r') as fraw:
    with open(cleanedCorpusName, mode='w') as fclean:
      for line in fraw:
        line = cleanLine(line)
        if line is None or line == '':
          continue
        fclean.write(line)
        fclean.write('\n')  # does line already contain \n?

# returns cleaned line or None if line should be excluded.
pastLines = set()
def cleanLine(line):
  # prevent repeated tweets
  if line in pastLines:
    return None
  pastLines.add(line)

  # exclude offenders
  if reCollide(rtRe, line) or reCollide(dstRe, line):
    return None

  #remove junk
  line = startHashtagRe.sub('', line)
  line = endHashtagRe.sub('', line)
  line = viaRe.sub('', line)
  line = hangingViaRe.sub('', line)
  # line = hangingAtRe.sub('', line)

  # fix formatting
  line = line.replace('\n', '')
  line = line.replace ('...', '')
  line = line.strip()

  # drop tweets with only a single word left.
  if len(nltk.word_tokenize(line)) == 1:
    return None

  return line

# returns True is `re` is matched in `line`
def reCollide(re, line):
  return (re.search(line) is not None)

if __name__ == '__main__':
  cleanCorpus()
