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


scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1
string1 = "writers"
string2 = "vintner"
e = '-'


-- Returns the score of the optimal alignment of the
-- two strings string1 and string2. If you need to, consult the Hint section below.
similarityScore :: String -> String -> Int
similarityScore [] []    = 0
similarityScore s1 []    = scoreSpace * length s1
similarityScore [] s2    = scoreSpace * length s2
similarityScore xt@(x:xs) yt@(y:ys) = maximum [similarityScore xs ys + score x y,
                                               similarityScore xs yt + score x e,
                                               similarityScore xt ys + score e y]


-- Score of two characters aligned
score x '-' = scoreSpace
score '-' y = scoreSpace
score x y = if x == y then scoreMatch
            else scoreMismatch


-- h1 and h2 chars
-- aList, a list of tuples
-- for every tuple (a,b) in aList, attach h1 at the first position of a
-- and h2 at the first position of b
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]


-- Generalizes the maximum function in two respects:
--    1. The "value" of an element is defined by a function supplied as a parameter.
--    2. Instead of just one element, the result is a list of all maximum elements.

-- For example, maximaBy length ["cs", "efd", "lth", "it"] should return ["efd", "lth"].

-- Find max value after valueFun is applied, return elements matching max value
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFun xs = [x | x <- xs, valueFun x == maxValue]
                where maxValue = maximum . map valueFun $ xs













-- HINTS

-- Observe that every alignment can start in exactly one of three ways:
--
--     1. Two non-space characters
--     2. Non-space above, space below
--     3. Space above, non-space below

-- In an optimal alignment of two strings (x:xs) and (y:ys) beginning as in (1), xs has
-- to be optimally aligned with ys. (If not, then a better-than-optimal solution could be
-- obtained by combining the optimal alignment between xs and ys with the first column.
-- Contradiction!) Therefore, the corresponding score is equal to the similarity score of
-- xs and ys plus the first column's score, which is either scoreMatch or scoreMismatch.

-- If an optimal solution starts like in (2), xs is optimally aligned with (y:ys) and x is
-- paired with a '-', and analogously for optimal alignments of type (3).
--
-- Thus, the following recursive relation holds for the similarity score of two non-empty strings:
--
--    sim((x:xs),(y:ys)) = max {sim(xs,ys) + score(x,y),
--                              sim(xs,(y:ys)) + score(x,'-'),
--                              sim((x:xs),ys) + score('-',y)}
--
-- where
--
--    score(x,'-') = score('-',y) = scoreSpace
--    score(x,y) = scoreMatch, if x == y
--                 scoreMismatch, if x /= y
