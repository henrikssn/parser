-- Chatterbot program in Haskell
-- Johan Förberg F10 & Erik Henriksson π10                

module Chatterbot where

import Data.Char
import System.Random

import Utilities
import Pattern

_wc_ = '*'

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n"
      question <- getLine

      answer <- stateOfMind brain
--    putStrLn (botName ++ ": " ++ 
      putStrLn $ (present . punctuate . capitalise . answer . prepare) question

      if (not.endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]

--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind brain = do
  randn <- randomIO :: IO Double
  let dict = map (map2 (id, pick randn)) brain
  return (rulesApply dict)

rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply dict = try $ transformationsApply [_wc_] reflect dict . reduce

--isolationDict :: [(Phrase, a)] -> [Phrase, Phrase]
--isolationDict dict = map (\ pp -> (fst pp, ["*"])) dict

capitalise :: Phrase -> Phrase
capitalise = id

punctuate :: Phrase -> Phrase
punctuate = id

--isPunctuation = flip elem ".,?:;!"

onlyNonGeneral :: [PhrasePair] -> [PhrasePair]
onlyNonGeneral dict = filter (\ pp -> fst pp /= [[_wc_]]) dict

chooseResponse :: Int -> [(a, [a])] -> [(a, a)]
chooseResponse i dict = map chooseOne dict
  where chooseOne entry = (fst entry, snd entry !! 
                           (i `mod` (length $ snd entry)))

-- reflect: try to replace each word with the appropriate reflection.
reflect :: Phrase -> Phrase
reflect = map . try $ flip lookup reflections

-- endOfDialog: are we done yet?
endOfDialog :: String -> Bool
endOfDialog = flip elem ["quit", "goodbye"] . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;!#%&|")

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile = map $ map2 (prepare, map prepare)

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply patterns =
        try (transformationsApply [_wc_] id patterns)

unwrap = map . map2 $ (unwords, unwords)

-- LITERALS --

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]

reductions :: [PhrasePair]
reductions = (map . map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

