module Chatterbot where
import Utilities
import System.Random
import Data.Char




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
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]


--------------------------------------------------------

-- brain : (question, [answers]) choose some of the answers, keep the questions
-- for every tuple (question, [answers]) in brain keep the question and pick a random answer
-- map2 (id, pick r) (question, [answers])

-- Based on the function rulesApply and generates a random response function
-- from a BotBrain argument
-- USE PICK FUNCTION SOMEWHERE

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind brain = do
  random <- randomIO :: IO Float
  return (rulesApply $ pickAnswers brain random)

-- Looper
pickAnswers [] random           = []
pickAnswers (state:rest) random = map2 (id, pick random) state : pickAnswers rest random




-- Example
--   transformations = [(words "I hate *", words "Why do you hate * ?")]
--   rulesApply transformations (words "I hate my mother")
--   = (words "Why do you hate your mother ?"),
--   rulesApply transformations (words "ARGH!")
--   = (words "")

-- transformations      : list of (patternA, patternB)
--                        = [["I","hate","*"], ["Why","do","you","hate","*","?"]]
-- phrase               : words "I hate my mother"
--                        = ["I","hate","my","mother"]

-- transformationsApply '*' reflect transformations phrase
--   =  Just "Why do you hate your mother ?"

-- Transforms a phrase according to a list of pattern transformations,
-- and where the intermediate result of the match is reflected (the fun part)
rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply transformations phrase
        | ab == phrase          = []
        | otherwise             = ab
        where ab = try (transformationsApply "*" reflect transformations) phrase

-- Example
-- reflect ["i", "will", "never", "see", "my", "reflection", "in", "your", "eyes"]
--       = ["you", "will", "never", "see", "your","reflection", "in", "my", "eyes"]

-- look for word in the list of tuple (a, b)
-- if a return b or if b return a
-- if not found return word
-- repeat

-- Reflect one word (a -> b)            : lookup reflections word
-- Reflect one word (a <> b)            : flip lookup reflections word
-- If word not in (a, b)s return word   : try f x = try (flip lookup reflections) word

reflect :: Phrase -> Phrase
reflect [] = []
reflect (phrase:rest) = switch phrase : reflect rest
        where switch = try $ flip lookup reflections


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


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

-- Example
botRules = [
  ("",
      ["Speak up! I can't hear you."]),

  ("I need *",
      ["Why do you need * ?",
       "Would it really help you to get * ?",
       "Are you sure you need * ?"])]

-- botRules : (question, [answers])
-- Goal
-- question : ["i","need","*"]
-- answer   : ["would","it","really","help","you","to","get","*","?"]
rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile = (map . map2) (lower, map lower)
        where lower = words . map toLower


--------------------------------------


reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
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

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply = fix . try . transformationsApply "*" id

-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Example
-- substitute 'x' "3*cos(x) + 4 - x" "5.37" = "3*cos(5.37) + 4 - 5.37"

-- Replaces each occurrence of wildcard in list (t:ts) with the list s
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute wildcard []     s = []
substitute wildcard (t:ts) s
                | t == wildcard = s   ++ substitute wildcard ts s
                | otherwise     = [t] ++ substitute wildcard ts s


-- Examples
-- match 'x' "2*x+3" "2*7+3" = Just "7"
-- match '*' "frodo" "gandalf" = Nothing
-- match 2 [1,3..5] [1,3..5] = Just []
-- match '*' "* and *" "you and me" = Just "you"
-- match 'x' "2*x+3+x" "2*7+3" = Nothing

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match wildcard [] []    = Just []
match wildcard [] s     = Nothing
match wildcard p []     = Nothing

match wildcard pattern@(p:ps) text@(s:ss)
      | p == wildcard   = sing `orElse` long
      | p == s          = match wildcard ps ss
      | otherwise       = Nothing
      where
              sing = singleWildcardMatch pattern text
              long = longerWildcardMatch pattern text


-- Helper function to match
-- single : match 'x' "2*x+3" "2*7+3" = Just "7"          -> Just take one
-- longer : match '*' "* and *" "you and me" = Just "you" -> Keep adding
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch pt@(wc:ps) (s:ss) = mmap (const [s])   $ match wc ps ss
longerWildcardMatch pt@(wc:ps) (s:ss) = mmap (s:)          $ match wc pt ss



-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Example
--   frenchPresentation = ("My name is *", "Je m'appelle *")
--   transformationApply '*' id "My name is Zacharias" frenchPresentation
--   = Just "Je m'appelle Zacharias"

-- wildcard                         : '*'
-- Find wildcard in s               : "My name is Zacharias" from patternA "My name is *"
-- Substitute (patternA, patternB)  : put wildcard value in patternB "Je m'appelle *"

-- Goal : substitute "*" "Je m'appelle *" "Zacharias"

-- mmap f something : applies f if something
 -- f         = substitute "*" "Je m'appelle *" ...
 -- something = match '*' "My name is *" "My name is Zacharias" = Just "Zacharias"

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply wildcard fun text (patternA, patternB) =
        mmap (substitute wildcard patternB . fun) (match wildcard patternA text)


-- Example
--   frenchPresentation = ("My name is *", "Je m'appelle *")
--   swedishPresentation = ("My name is *", "Mitt namn är *")
--   presentations = [frenchPresentation, swedishPresentation]
--   transformationsApply '*' id (reverse presentations) "My name is Zacharias"
--   = Just "Mitt namn är Zacharias"
--   transformationsApply '*' id presentations "My name is Zacharias"
--   =  Just "Je m'appelle Zacharias"

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply wildcard fun []                text = Nothing
transformationsApply wildcard fun (pattern : pRest) text = apply `orElse` keepSearching
        where
                apply           = transformationApply  wildcard fun text  pattern
                keepSearching   = transformationsApply wildcard fun pRest text
