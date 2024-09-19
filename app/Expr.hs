{-# LANGUAGE TypeApplications #-}
module Expr where

import Text.ParserCombinators.ReadP (ReadP, eof, satisfy, char, chainl1, (+++))
import Data.Fixed (Pico, showFixed)
import Control.Applicative (many, some, optional)
import Data.Char (isSpace, isDigit)
import Data.Ratio ((%))
import Data.Foldable (foldl')

data Binop
  = Add
  | Sub
  | Div
  | Mul

data Expr
  = Integer Integer
  | Decimal Rational
  | Binop Binop Expr Expr

parser :: ReadP Expr
parser =
  addsub <* eof
  where
    token x = x <* many (satisfy isSpace)
    
    number = token $ do
      digits <- some (satisfy isDigit)
      mDecimals <- optional $ char '.' *> some (satisfy isDigit)
      case mDecimals of
        Nothing ->
          pure . Integer $ fromDigits digits
        Just decimals ->
          pure . Decimal $ fromDigits (digits <> decimals) % (10 ^ length decimals)

    addsub = chainl1 muldiv ((Binop Add <$ token (char '+')) +++ (Binop Sub <$ token (char '-')))

    muldiv = chainl1 number ((Binop Mul <$ token (char '*')) +++ (Binop Div <$ token (char '/')))

    fromDigit '0' = 0
    fromDigit '1' = 1
    fromDigit '2' = 2
    fromDigit '3' = 3
    fromDigit '4' = 4
    fromDigit '5' = 5
    fromDigit '6' = 6
    fromDigit '7' = 7
    fromDigit '8' = 8
    fromDigit '9' = 9
    fromDigit _ = undefined

    fromDigits :: [Char] -> Integer
    fromDigits = foldl' (\acc d -> 10 * acc + fromDigit d) 0

data Value
  = VInteger Integer
  | VDecimal Rational

eval :: Expr -> Value
eval (Integer i) = VInteger i
eval (Decimal i) = VDecimal i
eval (Binop op a b) =
    case (eval a, eval b) of
      (VInteger a', VInteger b') ->
        VInteger (fnInteger a' b')
      (VInteger a', VDecimal b') ->
        VDecimal (fnDecimal (fromIntegral a') b')
      (VDecimal a', VInteger b') ->
        VDecimal (fnDecimal a' (fromIntegral b'))
      (VDecimal a', VDecimal b') ->
        VDecimal (fnDecimal a' b')
  where
    fnInteger :: Integral a => a -> a -> a
    fnInteger =
      case op of
        Add -> (+)
        Sub -> (-)
        Mul -> (*)
        Div -> div
    
    fnDecimal :: Fractional a => a -> a -> a
    fnDecimal =
      case op of
        Add -> (+)
        Sub -> (-)
        Mul -> (*)
        Div -> (/)

printValue :: Value -> String
printValue (VInteger i) = show i
printValue (VDecimal i) = showFixed True (realToFrac @Rational @Pico i)
