{-#LANGUAGE OverloadedStrings #-}

module NExpr where

import Data.Functor (void)

import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char

import Parsing (Parser, d, e, n, o, v)

data NExpr
  = Constant Int
  | Linear Int Int
  | Odd
  | Even
  deriving Show

integer :: Parser Int
integer = read <$> some digitChar

plusOrMinus :: Parser Text
plusOrMinus = try "-" <|> "+"

signToInt :: Text -> Int
signToInt "-" = -1
signToInt _   = 1

{-
  nth
    : S* [ ['-'|'+']? INTEGER? {N} [ S* ['-'|'+'] S* INTEGER ]? |
          ['-'|'+']? INTEGER | {O}{D}{D} | {E}{V}{E}{N} ] S*
    ;
-}
nExpr :: Parser NExpr
nExpr = do
  space
  expr <- try linear <|> try constant <|> try odd_ <|> even_
  space
  pure expr
    where
      linear = do
        kSign <- option 1 (try $ signToInt <$> plusOrMinus)
        k     <- option 1 (try integer)
        void n
        bPart <- option 0 $ try $ do
          space
          bSign <- signToInt <$> plusOrMinus
          space
          b <- integer
          pure $ bSign * b
        space
        pure $ Linear (kSign * k) bPart

      constant = do
        sign  <- option 1 (try $ signToInt <$> plusOrMinus)
        value <- integer
        pure $ Constant (sign * value)

      odd_  = o >> d >> d >> pure Odd
      even_ = e >> v >> e >> n >> pure Even

-- n >= 0
-- a > 0
isN :: NExpr -> Int -> Bool
isN (Constant value) n = value == n
isN (Linear a b)     n = let (d, r) = (n - b) `quotRem` a in d >= 0 && r == 0
isN Odd              n = odd n
isN Even             n = even n