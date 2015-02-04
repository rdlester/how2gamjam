#!/usr/bin/env runhaskell
import qualified Data.ByteString.Char8 as B
import qualified Data.MarkovChain as Markov
import System.IO
import qualified System.Random as Random

main = withFile "cleanCorpus.txt" ReadMode $ \h -> do
  tweets <- B.hGetContents h
  let cleaned = B.words tweets
  gen1 <- Random.getStdGen
  let (start, gen2) = Random.randomR (0, length cleaned) gen1
  let output = take 20 $ Markov.run 2 cleaned start gen2
  print $ B.unwords output
