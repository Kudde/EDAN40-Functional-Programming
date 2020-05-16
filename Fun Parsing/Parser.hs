module Parser(module CoreParser, T, digit, digitVal, chars, letter, err,
              lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-)) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
infixl 7 -#, #-

type T a = Parser a

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

iter :: Parser a -> Parser [a]
iter m = m # iter m >-> cons ! return []

cons(a, b) = a:b

-- something # something then return snd something
(-#) :: Parser a -> Parser b -> Parser b
m -# n = m # n >-> snd

-- something # something then return fst something
(#-) :: Parser a -> Parser b -> Parser a
m #- n = m # n >-> fst

-- Check if first char is space. Could also be many spaces..
spaces :: Parser String
spaces =  iter $ char ? isSpace

token :: Parser a -> Parser a
token m = m #- spaces

-- Check if first char is letter
letter :: Parser Char
letter =  char ? isAlpha

word :: Parser String
word = token (letter # iter letter >-> cons)

-- Eats n chars of string
chars :: Int -> Parser String
chars 0 = return []
chars n = char # chars (n - 1) >-> cons

-- Is w first in string?
accept :: String -> Parser String
accept w = (token (chars (length w))) ? (==w)

-- Accepts the same string input as accept w but reports the missing string using err in case of failure.
require :: String -> Parser String
require w  = accept w ! err w

lit :: Char -> Parser Char
lit c = token char ? (==c)

digit :: Parser Char
digit = char ? isDigit

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
number :: Parser Integer
number = token (digitVal #> number')
