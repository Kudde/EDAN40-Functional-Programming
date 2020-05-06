

-- ------------------------------------------- Problem info

-- Alignment of two strings
--      H A S K E L L
--      P A S C A - L
--
--      3 matches, 3 mismatches, and 1 space
--
--      H - A S K E L L
--      - P A S C - A L
--
--      3 matches, 2 mismatches, and 3 spaces
--
--      scoreMatch, scoreMismatch, and scoreSpace.
--
--
-- Problem specification
-- Input: Two strings s and t, and values for scoreMatch, scoreMismatch, and scoreSpace.
-- Output: All optimal alignments between s and t.

-- The length of an alignment is the number of columns it contains
-- An optimal alignment is an alignment with the highest possible score.
-- The score of such an alignment is called the similarity score of the two strings.
-- Note that there can be more than one optimal alignment.

-- ------------------------------------------- Variables

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

string1 = "writers"
string2 = "vintner"
e       = '-'

-- ------------------------------------------- 2a


-- Example
-- similarityScore string1 string2 = -5

-- Returns the score of the optimal alignment of the
-- two strings string1 and string2
similarityScore :: String -> String -> Int
similarityScore [] []    = scoreMatch
similarityScore s1 []    = scoreSpace
similarityScore [] s2    = scoreSpace
similarityScore xt@(x:xs) yt@(y:ys) = maximum [similarityScore xs ys + score x y,
                                               similarityScore xs yt + score x e,
                                               similarityScore xt ys + score e y]

-- Score of two characters
score :: Char -> Char -> Int
score x '-' = scoreSpace
score '-' y = scoreSpace
score x y = if x == y then scoreMatch
            else scoreMismatch

-- ------------------------------------------- 2b

-- Example
-- attachHeads 'h' '-' [("ej","ej"),("ola","oh")]
-- = [("hej","-ej"),("hola","-oh")]

-- h1 and h2 chars
-- aList, a list of tuples
-- for every tuple (a,b) in aList, attach h1 at the first position of a
-- and h2 at the first position of b
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]



-- ------------------------------------------- 2c

-- Example
-- maximaBy length ["cs", "efd", "lth", "it"]
-- = ["efd", "lth"].

-- Generalizes the maximum function in two respects:
--    1. The "value" of an element is defined by a function supplied as a parameter.
--    2. Instead of just one element, the result is a list of all maximum elements.

-- Apply valueFun to each element
-- Find the max value
-- Return elements matching the max value

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFun xs = filter (\x -> valueFun x == maxValue) xs
                where maxValue = maximum (map valueFun xs)


-- ------------------------------------------- 2d


-- Example
-- optAlignments string1 string2
-- = [("writ-ers","vintner-"), ("wri-t-ers","-vintner-"), ("wri-t-ers","v-intner-")]

-- Returns a list of all optimal alignments between string1 and string2.
-- (Hint: Follow the same pattern as similarityScore, and make use of the functions attachHeads and maximaBy

-- maximaBy ScoreFun allAlignments


type AlignmentType = (String,String)
optAlignments :: String -> String -> [AlignmentType]
optAlignments [] []               = [([],[])]
optAlignments (x:xs) []           = attachHeads x e $ optAlignments xs []
optAlignments [] (y:ys)           = attachHeads e y $ optAlignments [] ys

optAlignments xt@(x:xs) yt@(y:ys) = maximaBy sScore alignments
                 where   sScore ([], []) = 0
                         sScore ((x:xs),(y:ys)) = score x y + sScore (xs,ys)
                        --  sScore (x, y) = similarityScore x y
                         alignments = (attachHeads x y $ optAlignments xs ys) ++
                                      (attachHeads x e $ optAlignments xs yt) ++
                                      (attachHeads e y $ optAlignments xt ys)


-- ------------------------------------------- 2e

-- Example
-- outputOptAlignments string1 string2
-- There are 3 optimal alignments:
--
-- w r i t - e r s
-- v i n t n e r -
--
-- w r i - t - e r s
-- - v i n t n e r -
--
-- w r i - t - e r s
-- v - i n t n e r -
--
-- There were 3 optimal alignments!


-- Prints all optimal alignments between string1 and string2 to the screen in a neat and easy-to-read fashion.
outputOptAlignments string1 string2 = do
        putStrLn("There are... ")
        putStrLn(showAlignments opa)
        putStrLn(" -> " ++ show (length opa) ++ " optimal alignments:")

        where opa = optAlignments string1 string2
              showAlignments xs = concat $ map showAlignment opa
              showAlignment (a,b) = (nl ++ a ++ nl ++ b ++ nl)
              nl = "\n"

-- ------------------------------------------- 3




-- ------------------------------------------- Maximum Common Subsequence Problem

-- No Optimalish
-- mcsLength' :: Eq a => [a] -> [a] -> Int
-- mcsLength' _ [] = 0
-- mcsLength' [] _ = 0
-- mcsLength' (x:xs) (y:ys)
--     | x == y    = 1 + mcsLength' xs ys
--     | otherwise = max (mcsLength' xs (y:ys))
--                       (mcsLength' (x:xs) ys)


-- Optimalish
-- mcsLength :: Eq a => [a] -> [a] -> Int
-- mcsLength xs ys = mcsLen (length xs) (length ys)
--   where
--     mcsLen i j = mcsTable!!i!!j
--     mcsTable = [[ mcsEntry i j | j<-[0..]] | i<-[0..] ]
--
--     mcsEntry :: Int -> Int -> Int
--     mcsEntry _ 0 = 0
--     mcsEntry 0 _ = 0
--     mcsEntry i j
--       | x == y    = 1 + mcsLen (i-1) (j-1)
--       | otherwise = max (mcsLen i (j-1))
--                         (mcsLen (i-1) j)
--       where
--          x = xs!!(i-1)
--          y = ys!!(j-1)
