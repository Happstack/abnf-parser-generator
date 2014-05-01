{-# LANGUAGE TemplateHaskell #-}
module GenAttoparsec where

import Classes
import Control.Applicative (Applicative(pure, (<*>),(<*), (*>)), many)
import Data.Char (chr)
import qualified Data.Attoparsec.Combinator as A
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.Text       as AT
import Language.Haskell.TH        (Exp, ExpQ, Q, appE, varE)
import Language.Haskell.TH.Syntax (Lift(lift))
import Language.Haskell.TH.Lift   (deriveLift)

newtype GenAttoparsec a = GA { runGenAttoparsec :: ExpQ }

instance Lift (GenAttoparsec a) where
    lift (GA expQ) = expQ

instance ApplicativeRepr GenAttoparsec where
    pureR a = GA [| a |]
    app  (GA f) (GA x) = GA (appE (appE (varE '(<*>)) f) x)
    appL (GA f) (GA x) = GA (appE (appE (varE '(<*))  f) x)
    appR (GA f) (GA x) = GA (appE (appE (varE '(*>))  f) x)

instance Pair GenAttoparsec where
    pair = GA [| pure (,) |]
    prj1 = GA [| pure fst |]
    prj2 = GA [| pure snd |]

genAttoparsecPredicate :: Predicate -> (Char -> Bool)
genAttoparsecPredicate (NotInClass str) = AT.notInClass str

instance ABNFParser GenAttoparsec where
    pCharVal t = GA [| AT.string t |]
    pHexChar s =
        let c = chr (read $ "0x" ++ s)
        in GA [| AT.char c |]
    pEnumerate choices =
        GA [| A.choice [ AT.string t *> pure v | (t, v) <- choices] |]
    pDigit = GA [| AT.digit |]
    pTakeWhile1 pred = GA [| AT.takeWhile1 (genAttoparsecPredicate pred) |]
    pMany p = GA [| many p |]
{-
    pHexChar s =
        let c = chr (read $ "0x" ++ s)
        in TH [| char c |]
--    pAnyChar   = TH [| anyChar |]
    pEnumerate choices = TH [| choice (map (\(t, v) -> string (unpack t) >> return v) choices) |]
-}