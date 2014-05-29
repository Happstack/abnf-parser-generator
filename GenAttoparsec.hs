{-# LANGUAGE TemplateHaskell #-}
module GenAttoparsec where

import Classes
import Control.Applicative (Applicative(pure, (<*>),(<*), (*>)), many, optional)
import Data.Char (chr)
import qualified Data.Attoparsec.Combinator as A
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.ByteString.Char8 as ABC
-- import qualified Data.Attoparsec.Text       as AT
import Language.Haskell.TH        (Exp, ExpQ, Q, appE, varE)
import Language.Haskell.TH.Syntax (Lift(lift))
import Language.Haskell.TH.Lift   (deriveLift)
import Numeric                    (readDec)

newtype GenAttoparsec a = GA { runGenAttoparsec :: ExpQ }

instance Lift (GenAttoparsec a) where
    lift (GA expQ) = expQ

instance ApplicativeRepr GenAttoparsec where
    pureR a = GA [| pure a |]
    app  (GA f) (GA x) = GA (appE (appE (varE '(<*>)) f) x)
    appL (GA f) (GA x) = GA (appE (appE (varE '(<*))  f) x)
    appR (GA f) (GA x) = GA (appE (appE (varE '(*>))  f) x)

instance Pair GenAttoparsec where
    pair = GA [| pure (,) |]
    prj1 = GA [| pure fst |]
    prj2 = GA [| pure snd |]

genAttoparsecPredicate :: Predicate -> (Char -> Bool)
genAttoparsecPredicate (NotInClass str) = ABC.notInClass str

instance ABNFParser GenAttoparsec where
    pCharVal t = GA [| AB.string t |]
    pHexChar s =
        let c = chr (read $ "0x" ++ s)
        in GA [| ABC.char c |]
    pEnumerate choices =
        GA [| A.choice [ AB.string t *> pure v | (t, v) <- choices] |]
    pDigit = GA [| ABC.digit |]
    pTakeWhile1 pred = GA [| ABC.takeWhile1 (genAttoparsecPredicate pred) |]
    pMany p = GA [| many p |]
    pMany1 p = GA [| A.many1 p |]
    pOptional p = GA [| optional p |]
    digitsToInt p = GA [| fmap readDec_  p |]

readDec_ :: String -> Int
readDec_ s =
    case readDec s of
      [(n,[])] -> n
      _ -> error "readDec_ failed."
{-
    pHexChar s =
        let c = chr (read $ "0x" ++ s)
        in TH [| char c |]
--    pAnyChar   = TH [| anyChar |]
    pEnumerate choices = TH [| choice (map (\(t, v) -> string (unpack t) >> return v) choices) |]
-}