-- ------------------------------------------- Reflections

-- Which parts of your code / functions were the hardest to write and why?,
--   optAlignments       - due to it taking some time to just get a good understanding of the problem and getting the recursions right
--                         and saving part solutions, especially in the optimization part which requierd writing things out on papper,
--                         especially since haskells lack of a print function mid execution and will to run if the code isn't already perfect.
--                         Another challenge was the long execution time when taking a slightly wrong turn in the implemenation or when spending
--                         an hour or two on accidentally running optAlignments instead of optAlignmentsTurbo in part 3..
--   outputOptAlignments - trying to put spaces between each character. still haven't figured that one out.
--                         Can't seem to get the word on the right format for mapping (++" ") to the word as a string or iterating through the characters

-- Which parts of your code / functions are you the most proud of,
-- I kinda want to cry when i see it.
-- I not sure if I can say I'm proud of similarityScore and optAlignments since it felt most like a copy-paste
-- from mcsLength and the hint section but otherwise from that I feel pretty proud of just being able to read the code the day after
-- I wrote it, getting the filter and lambda function to work without looking things up in maximaBy and getting the outputOptAlignments
-- to look pretty neat for being an output function


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

string3 = "aferociousmonadatemyhamster"
string4 = "functionalprogrammingrules"

string5 = "bananrepubliksinvasionsarmestabsadjutant"
string6 = "kontrabasfiolfodralmakarmästarlärling"

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

attachTails :: a -> a -> [([a],[a])] -> [([a],[a])]
attachTails h1 h2 aList = [(xs ++ [h1], ys ++ [h2]) | (xs,ys) <- aList]



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
        putStrLn("-> " ++ show (length opa) ++ " optimal alignments" ++ nl)

        where opa = optAlignmentsTurbo string1 string2
              showAlignments xs = concat $ map showAlignment opa
              showAlignment (a,b) = (nl ++ a ++ nl ++ b ++ nl)
              nl = "\n "

-- ------------------------------------------- 3 Optimization

-- You will need to redefine the auxiliary functions used by mcsLength and the table's
-- initial values (that is, the values in the first "row" and the first "column").

-- Example
-- similarityScoreTurbo string1 string2 = -5

-- The elements in the similarityScore table should be of the type Int.

similarityScoreTurbo :: String -> String -> Int
similarityScoreTurbo xs ys = sScore (length xs) (length ys)
  where
    sScore i j = sTable !! i !! j

    sTable = [[ sEntry i j | j <- [0..]] | i <- [0..] ]

    sEntry :: Int -> Int -> Int
    sEntry 0 0 = scoreMatch
    sEntry _ 0 = scoreSpace
    sEntry 0 _ = scoreSpace
    sEntry i j = maximum [sScore (i - 1) (j - 1) + score x y ,
                          sScore (i - 1)  j      + score x e ,
                          sScore  i      (j - 1) + score y e ]
      where
         x = xs !! (i - 1)
         y = ys !! (j - 1)


-- Example
-- optAlignments "aferociousmonadatemyhamster" "functionalprogrammingrules"
-- = 308 optimal alignments
-- optAlignments "bananrepubliksinvasionsarmestabsadjutant" "kontrabasfiolfodralmakarmästarlärling"
-- = 1736 optimal alignments

-- For optAlignments, the elements should be of the type (Int, [AlignmentType])
-- Where the second element contain the optimal alignments themselves and
-- the first element is the score for these alignments

optAlignmentsTurbo :: String -> String -> [AlignmentType]
optAlignmentsTurbo xs ys = snd $ alignment (length xs) (length ys)
  where

    alignment i j = aTable !! i !! j

    aTable = [[ aEntry i j | j <- [0..]] | i <- [0..] ]

    aEntry :: Int -> Int -> (Int, [AlignmentType])
    aEntry 0 0 = (0, [([],[])])
    aEntry i 0 = (scoreSpace * i, [(take i xs    , replicate i e)])
    aEntry 0 j = (scoreSpace * j, [(replicate j e, take j ys)])
    aEntry i j = (head scores, concat alignments)
            where
                (dis, dal) = alignment (i - 1) (j - 1)
                (tos, tal) = alignment (i - 1)  j
                (sis, sal) = alignment  i      (j - 1)
                x = xs !! (i - 1)
                y = ys !! (j - 1)
                (scores, alignments) =  unzip $ maximaBy fst [(dis + score x y, attachTails x y dal),
                                                              (tos + score x e, attachTails x e tal),
                                                              (sis + score e y, attachTails e y sal)]

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
--     mcsLen i j = mcsTable!!i!!j                              -- return item at i,j
--     mcsTable = [[ mcsEntry i j | j<-[0..]] | i<-[0..] ]      -- create table and put mcsEntry at i,j
--
--     mcsEntry :: Int -> Int -> Int                            -- add entry to table
--     mcsEntry _ 0 = 0
--     mcsEntry 0 _ = 0
--     mcsEntry i j
--       | x == y    = 1 + mcsLen (i-1) (j-1)
--       | otherwise = max (mcsLen i (j-1))
--                         (mcsLen (i-1) j)
--       where
--          x = xs!!(i-1)                                       -- look for entry
--          y = ys!!(j-1)
