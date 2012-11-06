-- Chatterbot program in Haskell
-- Johan Förberg F10 & Erik Henriksson π10                

module Chatterbot where

import Data.Char

import Utilities
import Pattern

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine

      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)

      if (not.endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]

--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
{- TO BE WRITTEN -}
stateOfMind _ = return id

rulesApply :: [PhrasePair] -> Phrase -> Phrase
{- TO BE WRITTEN -}
rulesApply _ = id

-- reflect: try to replace each word with the appropriate reflection.
reflect :: Phrase -> Phrase
reflect = map . try $ flip lookup reflections

-- endOfDialog: are we done yet?
endOfDialog :: String -> Bool
endOfDialog = flip elem ["quit", "goodbye"] . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile [] = []
rulesCompile ((a,b):xs) = (prepare a, map prepare b) : rulesCompile xs

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply patterns phrase = 
  maybe phrase
        words
        (transformationsApply '*' id (unwrap patterns) $ unwords phrase)

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

