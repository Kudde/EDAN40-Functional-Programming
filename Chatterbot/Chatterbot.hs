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

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
{- TO BE WRITTEN -}
-- use pick function here
stateOfMind _ = return id

rulesApply :: [PhrasePair] -> Phrase -> Phrase
{- TO BE WRITTEN -}
rulesApply _ = id

reflect :: Phrase -> Phrase
{- TO BE WRITTEN -}
reflect = id

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

rulesCompile :: [(String, [String])] -> BotBrain
{- TO BE WRITTEN -}
rulesCompile _ = []


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
{- TO BE WRITTEN -}
reductionsApply _ = id


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Example
-- substitute 'x' "3*cos(x) + 4 - x" "5.37" = "3*cos(5.37) + 4 - 5.37"

-- Replaces each occurrence of wildcard in list (t:ts) with the list s
substitute :: Eq a => a -> [a] -> [a] -> [a]
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
-- p is considered a pattern which may contain elements equal to wildcard
-- s is the list to look for the wildcard replacement
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match wildcard [] []    = Just []
match wildcard [] s     = Nothing
match wildcard p []     = Nothing

match wildcard pt@(p:ps) st@(s:ss)
      | p == wildcard   = sing `orElse` long
      | p == s          = match wildcard ps ss
      | otherwise       = Nothing
      where
              sing = singleWildcardMatch pt st
              long = longerWildcardMatch pt st


-- Helper function to match
-- single : match 'x' "2*x+3" "2*7+3" = Just "7"          -> Just take one
-- longer : match '*' "* and *" "you and me" = Just "you" -> Keep adding
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (s:ss) = mmap (const [s])   $ match wc ps ss
longerWildcardMatch (wc:ps) (s:ss) = mmap (s:)          $ match wc (wc:ps) ss



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

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply _ _ _ _ = Nothing
{- TO BE WRITTEN -}


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ _ _ = Nothing
{- TO BE WRITTEN -}
